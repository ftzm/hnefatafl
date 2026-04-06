{-# LANGUAGE DataKinds #-}
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
import Control.Exception.Safe (tryAny)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.QSem
import Effectful.Console.ByteString
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Labeled
import Hnefatafl.Effect.Log (runKatipE)
import Hnefatafl.Interpreter.Log.JSON (withNoLogEnv)
import Hnefatafl.Api.Routes (Routes (..), VersionResponse (..))
import Hnefatafl.Bindings (
  EngineGameStatus,
  MoveValidationResult (..),
  applyMoveSequence,
  nextGameState,
  startBoard,
 )
import Hnefatafl.Board (formatGameMove, formatMoveResult, printBoard)
import Hnefatafl.Client (createClient)
import Hnefatafl.Core.Data (
  Game (..),
  GameId (..),
  GameMove (..),
  HumanPlayer (..),
  Move,
  PlayerId (..),
 )
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen
import Hnefatafl.Effect.Storage
import Hnefatafl.Import (importGameFromFile)
import Hnefatafl.Interpreter.Clock.IO
import Hnefatafl.Interpreter.IdGen.UUIDv7
import Hnefatafl.Interpreter.Search.Local
import Hnefatafl.Interpreter.Search.Remote
import Hnefatafl.Interpreter.Storage.SQLite
import Hnefatafl.SelfPlay (VersionId (..))
import Hnefatafl.SelfPlay.Runner (runSelfPlayHeadless, runSelfPlayWithUI)
import Hnefatafl.Serialization (moveToNotation, parseMoveList)
import Hnefatafl.Server (runServer)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative
import Servant.Client (ClientError, mkClientEnv, parseBaseUrl, runClientM)
import System.Exit (ExitCode (..))
import Version qualified

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

data ServerOptions = ServerOptions
  { port :: Int
  }
  deriving (Show)

data SelfPlayOptions = SelfPlayOptions
  { numActors :: Int
  , stateDir :: Text
  , startPositionsFile :: Text
  , oldServerUrl :: Text
  , oldVersion :: Maybe Text
  , headless :: Bool
  }
  deriving (Show)

data Command
  = ProcessMoves ProcessMovesOptions
  | Import ImportOptions
  | PrintGame PrintGameOptions
  | Server ServerOptions
  | SelfPlay SelfPlayOptions
  deriving (Show)

data GlobalOptions = GlobalOptions
  { db :: Maybe Text
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
  Import . ImportOptions
    <$> strArgument (metavar "FILE" <> help "JSON file to import games from")

printGameParser :: Parser Command
printGameParser =
  PrintGame . PrintGameOptions
    <$> strArgument (metavar "GAME_ID" <> help "Game ID to print moves for")

serverParser :: Parser Command
serverParser =
  Server . ServerOptions
    <$> argument auto (metavar "PORT" <> help "Port number to run the server on")

selfPlayParser :: Parser Command
selfPlayParser =
  SelfPlay
    <$> ( SelfPlayOptions
            <$> option
              auto
              (long "actors" <> metavar "N" <> value 1 <> help "Number of parallel game actors")
            <*> strOption
              ( long "state-dir"
                  <> metavar "DIR"
                  <> value "."
                  <> help "Directory for state files"
              )
            <*> strArgument (metavar "POSITIONS_FILE" <> help "File containing start positions")
            <*> strOption
              ( long "old-server"
                  <> metavar "URL"
                  <> help "URL of the server running the old version"
              )
            <*> optional
              ( strOption
                  ( long "old-version"
                      <> metavar "VERSION"
                      <> help "Expected version of the old server (verified at startup)"
                  )
              )
            <*> switch
              ( long "headless"
                  <> help "Run without UI (for scripts)"
              )
        )

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
        <> command "server" (info serverParser (progDesc "Run the web server"))
        <> command "self-play" (info selfPlayParser (progDesc "Run self-play with UI"))
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

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (toString Version.version)
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

-- | Run an effectful computation with standard CLI effects (storage, clock, ID gen, console, filesystem).
-- Catches synchronous exceptions and returns them as Left.
withStandardEffects ::
  Maybe Text ->
  ( forall es.
    ( Storage :> es
    , Clock :> es
    , IdGen :> es
    , Console :> es
    , FileSystem :> es
    ) =>
    Eff es a
  ) ->
  IO (Either String a)
withStandardEffects dbPathOpt eff = do
  let dbPath = fromMaybe "db.db" dbPathOpt
  conn <- open (toString dbPath)
  connectionVar <- newMVar conn
  result <-
    tryAny $
      withNoLogEnv "hnefatafl-cli" $ \logEnv ->
        runEff $
          runConsole $
            runFileSystem $
              runKatipE logEnv $
                runStorageSQLite connectionVar $
                  runClockIO $
                    runIdGenUUIDv7 eff
  close conn
  pure $ first displayException result

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
    result <-
      withStandardEffects options.globalOpts.db (importGameFromFile filePath)
    case result of
      Left err -> putTextLn ("Import failed: " <> toText err) >> pure (ExitFailure 1)
      Right (Left importErr) -> putTextLn ("Import failed: " <> importErr) >> pure (ExitFailure 1)
      Right (Right _) -> putTextLn "Import successful" >> pure ExitSuccess
  PrintGame PrintGameOptions{gameId} -> do
    result <-
      withStandardEffects options.globalOpts.db (printGameMoves (GameId gameId))
    case result of
      Left err -> putTextLn ("Error: " <> toText err) >> pure (ExitFailure 1)
      Right _ -> pure ExitSuccess
  Server ServerOptions{port} -> do
    runServer port
    pure ExitSuccess
  SelfPlay
    SelfPlayOptions
      { numActors
      , stateDir
      , startPositionsFile
      , oldServerUrl
      , oldVersion
      , headless
      } -> do
      runSelfPlayWithInterpreters
        numActors
        (toString stateDir)
        (toString startPositionsFile)
        (toString oldServerUrl)
        oldVersion
        headless

-- | Run self-play with all necessary effect interpreters
runSelfPlayWithInterpreters ::
  Int -> FilePath -> FilePath -> String -> Maybe Text -> Bool -> IO ExitCode
runSelfPlayWithInterpreters numActors stateDir startPositionsFile oldServerUrlStr expectedOldVersion headless = do
  manager <- newManager defaultManagerSettings
  baseUrl <- parseBaseUrl oldServerUrlStr
  let clientEnv = mkClientEnv manager baseUrl
      client = createClient

  -- Verify the old server's version if expected version provided
  for_ expectedOldVersion $ \expected -> do
    versionResult <- runClientM client.version clientEnv
    case versionResult of
      Left err -> do
        putTextLn $ "Failed to query old server version: " <> show err
        exitFailure
      Right resp -> when (expected /= resp.versionNumber) $ do
        putTextLn $
          "Version mismatch: expected "
            <> expected
            <> " but server reports "
            <> resp.versionNumber
        exitFailure

  let runner = if headless then runSelfPlayHeadless else runSelfPlayWithUI
      oldVersionLabel = fromMaybe "old" expectedOldVersion
  result <- withNoLogEnv "hnefatafl-cli" $ \logEnv ->
    runEff $
      runErrorNoCallStack @Text $
        runErrorNoCallStack @ClientError $
          runConcurrent $
            runKatipE logEnv $ do
              qsem <- newQSem numActors
              runFileSystem $
                runLabeled @"new" (runSearchLocal qsem) $
                  runLabeled @"old" (runSearchRemote clientEnv client) $
                    runner
                      numActors
                      (VersionId Version.version)
                      (VersionId oldVersionLabel)
                      stateDir
                      startPositionsFile

  case result of
    Left err -> putTextLn ("Self-play failed: " <> err) >> pure (ExitFailure 1)
    Right (Left clientErr) ->
      putTextLn ("Self-play failed (client error): " <> show clientErr)
        >> pure (ExitFailure 1)
    Right (Right ()) -> putTextLn "Self-play completed successfully" >> pure ExitSuccess

printGameMoves ::
  (Storage :> es, Console :> es) =>
  GameId -> Eff es ()
printGameMoves gameId = do
  (game, moves) <-
    runTransaction $ (,) <$> getGame gameId <*> getMovesForGame gameId
  Console.putStrLn $
    encodeUtf8
      ("Game: " <> fromMaybe (let (GameId gid) = game.gameId in gid) game.name :: Text)
  Console.putStrLn $
    encodeUtf8 ("Status: " <> maybe "ongoing" show game.outcome :: Text)
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
      encodeUtf8
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
getOrCreateHumanPlayer playerName = do
  freshId <- generateId
  runTransaction $
    humanPlayerFromName playerName >>= \case
      Just player -> pure player
      Nothing -> do
        let newPlayer = HumanPlayer freshId playerName Nothing
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
