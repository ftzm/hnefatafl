{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.CLI (
  run,
  GameImport (..),
  GameImportInput (..),
  getOrCreateHumanPlayer,
) where

import Chronos (Time)
import Data.Aeson (FromJSON, ToJSON)
import Effectful
import Hnefatafl.Bindings (
  EngineGameStatus,
  MoveValidationResult (..),
  applyMoveSequence,
  nextGameState,
 )
import Hnefatafl.Board (formatMoveResult)
import Hnefatafl.Core.Data (HumanPlayer (..), Move, PlayerId (..))
import Hnefatafl.Effect.IdGen
import Hnefatafl.Effect.Storage
import Hnefatafl.Serialization (parseMoveList)
import Options.Applicative
import System.Exit (ExitCode (..))

data ProcessMovesOptions = ProcessMovesOptions
  { moveText :: Text
  , silentSuccess :: Bool
  , allowRepetition :: Bool
  }
  deriving (Show)

data Command
  = ProcessMoves ProcessMovesOptions
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

otherParser :: Parser Command
otherParser = pure Other

commandParser :: Parser Command
commandParser =
  subparser
    ( command "process-moves" (info processMovesParser (progDesc "Process moves"))
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
    "hnefatafl version 0.0.0.2"
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
      (toList $ applyMoveSequence ms)

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
  Other -> do
    putTextLn "other"
    pure ExitSuccess

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
