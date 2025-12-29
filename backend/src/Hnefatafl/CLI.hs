{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.CLI (
  run,
  GameImport (..),
  GameImportInput (..),
  getOrCreateHumanPlayer,
) where

import Chronos (Time)
import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple (close, open)
import Effectful
import Effectful.Console.ByteString
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static
import Effectful.FileSystem
import Hnefatafl.Bindings (
  EngineGameStatus,
  MoveValidationResult (..),
  applyMoveSequence,
  nextGameState,
  startBoard,
 )
import Hnefatafl.Board (formatGameMove, formatMoveResult, printBoard)
import Hnefatafl.Core.Data (
  Game (..),
  GameId (..),
  GameMove (..),
  HumanPlayer (..),
  Move,
  PlayerId (..),
 )
import Hnefatafl.Effect.IdGen
import Hnefatafl.Effect.Storage
import Hnefatafl.Import (importGameFromFile)
import Hnefatafl.Interpreter.Clock.IO
import Hnefatafl.Interpreter.IdGen.UUIDv7
import Hnefatafl.Interpreter.Storage.SQLite
import Hnefatafl.Serialization (moveToNotation, parseMoveList)
import Options.Applicative
import System.Exit (ExitCode (..))
import Version (version)

data ProcessMovesOptions = ProcessMovesOptions
  { moveText :: Text
  , silentSuccess :: Bool
  , allowRepetition :: Bool
  }
  deriving (Show)

data ImportOptions = ImportOptions
  { filePath :: Text
  }
  deriving (Show)

data PrintGameOptions = PrintGameOptions
  { gameId :: Text
  }
  deriving (Show)

data Command
  = ProcessMoves ProcessMovesOptions
  | Import ImportOptions
  | PrintGame PrintGameOptions
  | Other
  deriving (Show)

data GlobalOptions = GlobalOptions
  { db :: Maybe Text
  , other :: Maybe Text
  }
  deriving (Show)

data Options = Options
  { globalOpts :: GlobalOptions
  , cmd :: Command
  }
  deriving (Show)

processMovesParser :: Parser Command
processMovesParser =
  ProcessMoves
    <$> ( ProcessMovesOptions
            <$> strArgument (metavar "MOVES" <> help "Move list to process")
            <*> switch (long "silent-success" <> help "Don't print moves if they're valid")
            <*> switch
              (long "allow-repetition" <> help "Allow threefold repetition without failing")
        )

importParser :: Parser Command
importParser =
  Import
    <$> ( ImportOptions
            <$> strArgument (metavar "FILE" <> help "JSON file to import games from")
        )

printGameParser :: Parser Command
printGameParser =
  PrintGame
    <$> ( PrintGameOptions
            <$> strArgument (metavar "GAME_ID" <> help "Game ID to print moves for")
        )

otherParser :: Parser Command
otherParser = pure Other

commandParser :: Parser Command
commandParser =
  subparser
    ( command "process-moves" (info processMovesParser (progDesc "Process moves"))
        <> command "import" (info importParser (progDesc "Import games from JSON file"))
        <> command
          "print-game"
          ( info
              printGameParser
              (progDesc "Print all moves of a game with board representations")
          )
        <> command "other" (info otherParser (progDesc "Other command"))
    )

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> optional
      ( strOption
          ( long "db"
              <> metavar "DATABASE"
              <> help "Database file path"
          )
      )
    <*> optional
      ( strOption
          ( long "other"
              <> metavar "FAKE"
              <> help "fake"
          )
      )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (toString version)
    ( long "version"
        <> short 'v'
        <> help "Show version"
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> globalOptionsParser
    <*> commandParser

formatMoves :: NonEmpty Move -> Text
formatMoves ms =
  mconcat $
    zipWith
      (\i r -> "Move: " <> show i <> "\n" <> formatMoveResult r)
      [1 :: Integer ..]
      (toList $ fst $ applyMoveSequence ms)

printMoves :: EngineGameStatus -> NonEmpty Move -> IO ()
printMoves status ms = do
  putTextLn $ formatMoves ms
  putTextLn $ "Game status: " <> show status

printError :: MoveValidationResult -> NonEmpty Move -> IO ()
printError MoveValidationResult{err, moveIndex} (m :| ms) = do
  putTextLn formattedResults
  putTextLn errorMsg
 where
  moveIdx :: Int = fromIntegral moveIndex
  movesUntilError :: NonEmpty Move = m :| take moveIdx ms
  formattedResults = formatMoves movesUntilError
  errorMsg = "Error at last move (" <> show (1 + moveIdx) <> "): " <> show err

runOptions :: Options -> IO ExitCode
runOptions options = case options.cmd of
  ProcessMoves ProcessMovesOptions{moveText, silentSuccess, allowRepetition} -> do
    case parseMoveList moveText of
      Left parseErr -> do
        putTextLn $ "Failed to parse moves: " <> parseErr
        pure $ ExitFailure 1
      Right moves -> do
        case nextGameState moves allowRepetition of
          Right status -> do
            if silentSuccess
              then pure ()
              else printMoves status moves
            pure ExitSuccess
          Left err -> do
            printError err moves
            pure $ ExitFailure 1
  Import ImportOptions{filePath} -> do
    let dbPath = fromMaybe "db.db" options.globalOpts.db
    conn <- open (toString dbPath)
    connectionVar <- newMVar conn
    result <-
      runEff $
        runErrorNoCallStack @String $
          runConsole $
            runFileSystem $
              runStorageSQLite connectionVar $
                runClockIO $
                  runIdGenUUIDv7 $
                    importGameFromFile filePath
    close conn
    case result of
      Left err -> do
        putTextLn $ "Import failed: " <> toText err
        pure $ ExitFailure 1
      Right (Left importErr) -> do
        putTextLn $ "Import failed: " <> importErr
        pure $ ExitFailure 1
      Right (Right _) -> do
        putTextLn "Import successful"
        pure ExitSuccess
  PrintGame PrintGameOptions{gameId} -> do
    let dbPath = fromMaybe "db.db" options.globalOpts.db
    conn <- open (toString dbPath)
    connectionVar <- newMVar conn
    result <-
      runEff $
        runErrorNoCallStack @String $
          runConsole $
            runFileSystem $
              runStorageSQLite connectionVar $
                runClockIO $
                  runIdGenUUIDv7 $
                    printGameMoves (GameId gameId)
    close conn
    case result of
      Left err -> do
        putTextLn $ "Error: " <> toText err
        pure $ ExitFailure 1
      Right _ -> pure ExitSuccess
  Other -> do
    putTextLn "other"
    pure ExitSuccess

printGameMoves ::
  (Storage :> es, Console :> es) =>
  GameId -> Eff es ()
printGameMoves gameId = do
  game <- getGame gameId
  moves <- getMovesForGame gameId
  Console.putStrLn $
    encodeUtf8 $
      ("Game: " <> fromMaybe (let (GameId gid) = game.gameId in gid) game.name :: Text)
  Console.putStrLn $ encodeUtf8 $ ("Status: " <> show game.gameStatus :: Text)
  Console.putStrLn ""

  if null moves
    then Console.putStrLn $ encodeUtf8 ("No moves found for this game." :: Text)
    else do
      -- Print initial board state
      Console.putStrLn $ encodeUtf8 ("Initial board:" :: Text)
      Console.putStrLn $ encodeUtf8 $ printBoard startBoard
      Console.putStrLn ""

      -- Print each move with board state
      traverse_ printMoveWithBoard (zip [1 ..] moves)
 where
  printMoveWithBoard :: Console :> es => (Int, GameMove) -> Eff es ()
  printMoveWithBoard (moveNum, gameMove) = do
    Console.putStrLn $
      encodeUtf8 $
        ( "Move "
            <> show moveNum
            <> " ("
            <> show gameMove.playerColor
            <> "): "
            <> moveToNotation gameMove.move ::
            Text
        )
    Console.putStrLn $ encodeUtf8 $ formatGameMove gameMove
    Console.putStrLn ""

--------------------------------------------------------------------------------
-- import

-- importGames :: IO (
-- importGames = undefined

data GameImportInput = GameImportInput
  { gameName :: Maybe Text
  , blackPlayerName :: Text
  , whitePlayerName :: Text
  , startTime :: Maybe Time
  , endTime :: Maybe Time
  , game_status :: Maybe Text
  , moveString :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameImport = GameImport
  { gameName :: Text
  , blackPlayerName :: Text
  , whitePlayerName :: Text
  , startTime :: Time
  , endTime :: Maybe Time
  , game_status :: Text
  }

-- | Look up a human player by name, creating one if it doesn't exist
getOrCreateHumanPlayer ::
  (Storage :> es, IdGen :> es) =>
  Text -> Eff es HumanPlayer
getOrCreateHumanPlayer name =
  humanPlayerFromName name >>= \case
    Just player -> pure player
    Nothing -> do
      playerId <- generateId
      let newPlayer = HumanPlayer playerId name Nothing
      insertHumanPlayer newPlayer
      pure newPlayer

--------------------------------------------------------------------------------

run :: IO ()
run = do
  opts <-
    execParser
      (info (optionsParser <**> helper <**> versionOption) (progDesc "Test Program"))
  exitCode <- runOptions opts
  exitWith exitCode
