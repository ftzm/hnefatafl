{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.SelfPlay (
  module Hnefatafl.SelfPlay,
) where

import Control.Concurrent.STM (stateTVar)
import Data.Aeson (
  FromJSON,
  FromJSONKey,
  ToJSON,
  ToJSONKey,
  eitherDecodeStrict,
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Effectful (Eff (), IOE, runEff, (:>))
import Effectful.Concurrent.Chan.Strict
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.FileSystem.IO (runFileSystem)
import Effectful.FileSystem.IO.ByteString (readFile)
import Effectful.FileSystem.IO.ByteString qualified as FSBS
import Effectful.Labeled (Labeled (..))
import Focus qualified
import Hnefatafl.Bindings (
  EngineGameStatus (..),
  SearchTrustedResult (..),
  applyMoveSequence,
 )
import Hnefatafl.Core.Data (
  ExternBoard,
  MoveResult (..),
 )
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Search (SearchTimeout (..))
import Hnefatafl.Serialization (parseMoveList)
import ListT qualified
import StmContainers.Map qualified as STMMap
import Prelude hiding (readFile)

data EngineVersion = New | Old
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data Player = Black | White
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data GameResult = GameResult
  { winner :: Player
  , moves :: Int
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data GameDefinition = GameDefinition
  { name :: GameName
  , notation :: Text
  , board :: ExternBoard
  , blackToMove :: Bool
  , newAsBlack :: Bool
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data InProgressGame
  = Claimed GameDefinition
  | Running GameDefinition SearchTrustedResult
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data CompletedGame = CompletedGame
  { gameDefinition :: GameDefinition
  , result :: GameResult
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

newtype GameName = GameName Text
  deriving (Show, Read, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

data ProcessingState = ProcessingState
  { unprocessedGames :: TVar [GameDefinition]
  , inProgressGames :: STMMap.Map GameName InProgressGame
  , completedGames :: TVar [CompletedGame]
  }

-- For serialization, we need to extract data from STM
data ProcessingStateSnapshot = ProcessingStateSnapshot
  { unprocessedGames :: [GameDefinition]
  , inProgressGames :: Map GameName InProgressGame
  , completedGames :: [CompletedGame]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGameName :: Int -> Bool -> GameName
mkGameName index newIsBlack = GameName $ show index <> " " <> players
 where
  players = if newIsBlack then "New vs. Old" else "Old vs. New"

loadStartPositions ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  Eff es ProcessingStateSnapshot
loadStartPositions = do
  ls <- lines . decodeUtf8 <$> readFile "engine_comparison_positions.txt"
  let (errors, moveLists) = partitionEithers $ map (\l -> (l,) <$> parseMoveList l) ls
  unless (null errors) $ throwError $ "Got errors: \n" <> unlines errors
  let definitions =
        concat $
          zipWith
            ( \i (notation, moveList) ->
                let (moveResults, _finalStatus) = applyMoveSequence moveList
                    movesResult = last moveResults
                    b = movesResult.board
                    blackToMove = not movesResult.wasBlackTurn
                 in [ GameDefinition
                        { name = mkGameName (i * 2) True
                        , notation = notation
                        , board = b
                        , blackToMove = blackToMove
                        , newAsBlack = True
                        }
                    , GameDefinition
                        { name = mkGameName (i * 2 + 1) False
                        , notation = notation
                        , board = b
                        , blackToMove = blackToMove
                        , newAsBlack = False
                        }
                    ]
            )
            [0 ..]
            moveLists
  pure $
    ProcessingStateSnapshot
      { unprocessedGames = definitions
      , inProgressGames = mempty
      , completedGames = mempty
      }

-- | Save ProcessingStateSnapshot to a JSON file
saveProcessingStateSnapshot ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  FilePath -> ProcessingStateSnapshot -> Eff es ()
saveProcessingStateSnapshot filePath processingStateSnapshot = do
  let jsonBytes = encodePretty processingStateSnapshot
  FSBS.writeFile filePath (BSL.toStrict jsonBytes)

-- | Runner for saveProcessingState that handles all required effects
runSaveProcessingState ::
  FilePath -> ProcessingStateSnapshot -> IO (Either Text ())
runSaveProcessingState filePath processingStateSnapshot =
  runEff
    . runErrorNoCallStack @Text
    . runFileSystem
    $ saveProcessingStateSnapshot filePath processingStateSnapshot

-- | Generate canonical filename for state file based on engine versions
getStateFileName :: VersionId -> VersionId -> FilePath
getStateFileName (VersionId v1) (VersionId v2) =
  let sortedVersions = sort [v1, v2]
      versionString = intercalate "_vs_" (map toString sortedVersions)
   in "self_play_state_" <> versionString <> ".json"

-- | Load existing state file or create new one from start positions
loadOrCreateStateSnapshot ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  FilePath -> Eff es ProcessingStateSnapshot
loadOrCreateStateSnapshot stateFilePath = do
  exists <- doesFileExist stateFilePath
  if exists
    then do
      jsonData <- FSBS.readFile stateFilePath
      case eitherDecodeStrict jsonData of
        Left err -> throwError $ "Failed to parse state file: " <> toText err
        Right processingStateSnapshot -> pure processingStateSnapshot
    else do
      initialState <- loadStartPositions
      saveProcessingStateSnapshot stateFilePath initialState
      pure initialState

-- | Load or create state for specific engine versions
loadOrCreateStateForVersions ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  VersionId -> VersionId -> Eff es ProcessingStateSnapshot
loadOrCreateStateForVersions version1 version2 =
  loadOrCreateStateSnapshot $ getStateFileName version1 version2

-- | Runner for loadOrCreateState that handles all required effects
runLoadOrCreateState :: FilePath -> IO (Either Text ProcessingStateSnapshot)
runLoadOrCreateState stateFilePath =
  runEff
    . runErrorNoCallStack @Text
    . runFileSystem
    $ loadOrCreateStateSnapshot stateFilePath

-- | Save state for specific engine versions using canonical filename
saveStateForVersions ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  VersionId -> VersionId -> ProcessingStateSnapshot -> Eff es ()
saveStateForVersions version1 version2 processingStateSnapshot = do
  let stateFilePath = getStateFileName version1 version2
  saveProcessingStateSnapshot stateFilePath processingStateSnapshot

-- | Runner for loadOrCreateStateForVersions that handles all required effects
runLoadOrCreateStateForVersions ::
  VersionId -> VersionId -> IO (Either Text ProcessingStateSnapshot)
runLoadOrCreateStateForVersions version1 version2 =
  runEff
    . runErrorNoCallStack @Text
    . runFileSystem
    $ loadOrCreateStateForVersions version1 version2

-- | Runner for saveStateForVersions that handles all required effects
runSaveStateForVersions ::
  VersionId -> VersionId -> ProcessingStateSnapshot -> IO (Either Text ())
runSaveStateForVersions version1 version2 processingStateSnapshot =
  runEff
    . runErrorNoCallStack @Text
    . runFileSystem
    $ saveStateForVersions version1 version2 processingStateSnapshot

-- | Runner for loadStartPositions that handles all required effects
runLoadStartPositions :: IO (Either Text ProcessingStateSnapshot)
runLoadStartPositions =
  runEff
    . runErrorNoCallStack @Text
    . runFileSystem
    $ loadStartPositions

newtype VersionId = VersionId Text
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create ProcessingState from snapshot
mkProcessingState :: ProcessingStateSnapshot -> STM ProcessingState
mkProcessingState snapshot = do
  unprocessedVar <- newTVar snapshot.unprocessedGames
  inProgressMap <- STMMap.new
  completedVar <- newTVar snapshot.completedGames

  -- Populate in-progress map
  forM_ (Map.toList snapshot.inProgressGames) $ \(name, game) -> do
    STMMap.insert game name inProgressMap

  pure $ ProcessingState unprocessedVar inProgressMap completedVar

-- | Take snapshot of current state for serialization
takeSnapshot :: ProcessingState -> STM ProcessingStateSnapshot
takeSnapshot processingState = do
  unprocessed <- readTVar processingState.unprocessedGames
  inProgress <-
    fromList <$> ListT.toList (STMMap.listT processingState.inProgressGames)
  completed <- readTVar processingState.completedGames
  pure $ ProcessingStateSnapshot unprocessed inProgress completed

-- | Atomically claim the next unprocessed game
claimNextGame :: ProcessingState -> STM (Maybe GameDefinition)
claimNextGame processingState = do
  maybeGame <- stateTVar processingState.unprocessedGames $ \case
    [] -> (Nothing, [])
    (game : rest) -> (Just game, rest)

  for maybeGame $ \game -> do
    STMMap.insert (Claimed game) game.name processingState.inProgressGames
    pure game

updateGameProgress ::
  GameName -> SearchTrustedResult -> ProcessingState -> STM ()
updateGameProgress name result processingState = do
  STMMap.focus (Focus.adjust updateGame) name processingState.inProgressGames
 where
  updateGame = \case
    Claimed gameDef -> Running gameDef result
    Running gameDef _ -> Running gameDef result

-- | Complete a game by moving it from in-progress to completed
completeGame :: GameName -> GameResult -> ProcessingState -> STM ()
completeGame name gameResult processingState = do
  maybeGame <-
    STMMap.focus Focus.lookupAndDelete name processingState.inProgressGames
  let gameDefinition =
        maybeGame <&> \case
          Claimed def -> def
          Running def _ -> def
  for_
    gameDefinition
    \def ->
      modifyTVar' processingState.completedGames (++ [CompletedGame def gameResult])

getWinner :: EngineGameStatus -> Maybe Player
getWinner = \case
  EngineOngoing -> Nothing
  EngineKingCaptured -> Just Black
  EngineWhiteSurrounded -> Just Black
  EngineNoWhiteMoves -> Just Black
  EngineKingEscaped -> Just White
  EngineExitFort -> Just White
  EngineNoBlackMoves -> Just White

-- run game loop
playGame ::
  forall current next es.
  ( Labeled current Search :> es
  , Labeled next Search :> es
  , IOE :> es
  , Concurrent :> es
  ) =>
  -- | The name of game
  Text ->
  -- | a chan to which we send processing updates
  Chan' (Text, SearchTrustedResult) ->
  -- | the number of moves played
  Int ->
  -- | the current board
  ExternBoard ->
  -- | if it is black to move
  Bool ->
  -- | the zobrist hashes to point
  [Word64] ->
  Eff es GameResult
playGame name chan moveCount board blackToMove hashes = do
  result <-
    send $
      (Labeled @current) $
        SearchTrusted board blackToMove hashes (SearchTimeout 1000)
  case getWinner result.gameStatus of
    Nothing -> do
      writeChan' chan (name, result)
      playGame @next @current
        name
        chan
        (moveCount + 1)
        result.updatedBoard
        (not blackToMove)
        (result.updatedZobristHash : hashes)
    Just winner -> pure $ GameResult winner moveCount
