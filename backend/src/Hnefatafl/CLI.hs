module Hnefatafl.CLI (run) where

import Hnefatafl.Bindings (
  EngineGameStatus,
  MoveValidationResult (..),
  applyMoveSequence,
  nextGameState,
 )
import Hnefatafl.Board (formatMoveResult)
import Hnefatafl.Core.Data (Move)
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
            <*> switch (long "allow-repetition" <> help "Allow threefold repetition without failing")
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
versionOption = infoOption "hnefatafl version 0.0.0.2"
  ( long "version"
 <> short 'v'
 <> help "Show version" )

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
      Right [] -> do
        putTextLn "No moves provided"
        pure $ ExitFailure 1
      Right (m : ms) -> do
        let moves = m :| ms
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

run :: IO ()
run = do
  opts <- execParser (info (optionsParser <**> helper <**> versionOption) (progDesc "Test Program"))
  exitCode <- runOptions opts
  exitWith exitCode
