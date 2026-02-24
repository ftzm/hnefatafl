{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.SelfPlay (
  Player (..),
  GameResult (..),
  GameKey,
  GameSetup (..),
  GameProgress (..),
  GameDefinition (..),
  CompletedGame (..),
  GameMoveEvent (..),
  StateUpdate (..),
  StateUpdatePayload (..),
  ProcessingState (..),
  ProcessingStateSnapshot (..),
  VersionId (..),
  mkGameName,
  gameSetupKey,
  searchResultToGameMoveEvent,
  gameMoveEventToMoveResult,
  beginGame,
  getStateFileName,
  claimNextGame,
  completeGame,
  gameActor,
  loadOrCreateStateSnapshot,
  mkProcessingState,
  runSelfPlayParallel,
  saveProcessingStateSnapshot,
  snapshotTimerActor,
  takeSnapshot,
  updateGameProgress,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (stateTVar)
import Data.Aeson (
  FromJSON (..),
  ToJSON,
  eitherDecodeStrict,
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Traversable (for)
import Effectful (Eff (), IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async, cancel, wait)
import Effectful.Concurrent.STM (TChan, writeTChan)
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
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
  Layer (..),
  Move (..),
  MoveResult (..),
 )
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Search (SearchTimeout (..))
import Hnefatafl.Serialization (parseMoveList)
import ListT qualified
import Optics ((%~))
import StmContainers.Map qualified as STMMap

import System.FilePath ((</>))
import Prelude hiding (readFile)

data Player = Black | White
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data GameResult = GameResult
  { winner :: Player
  , moves :: Int
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- Key type for identifying games
type GameKey = (Int, Bool, Int) -- (id, newAsBlack, playIndex)

-- Helper to create display name from key
mkGameName :: GameKey -> Text
mkGameName (gameId, newAsBlack, playIndex) =
  show gameId <> "." <> show playIndex <> " " <> if newAsBlack then "New vs. Old" else "Old vs. New"

-- Extract key from setup
gameSetupKey :: GameSetup -> GameKey
gameSetupKey setup = (setup.id, setup.newAsBlack, setup.playIndex)

-- Immutable game setup
data GameSetup = GameSetup
  { id :: Int
  , -- | Index for repeated plays of the same game configuration.
    -- Multiple plays allow majority voting to filter out timing variance.
    playIndex :: Int
  , setupNotation :: Text
  , startingBoard :: ExternBoard
  , startingBlackToMove :: Bool
  , newAsBlack :: Bool
  , startingHashes :: [Word64]
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- Mutable game progress
data GameProgress = GameProgress
  { currentBoard :: ExternBoard
  , currentBlackToMove :: Bool
  , currentHashes :: [Word64]
  , moveCount :: Int
  , selfPlayMoves :: [Move]
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- Combined definition (for unprocessed and in-progress)
data GameDefinition = GameDefinition
  { setup :: GameSetup
  , progress :: GameProgress
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data CompletedGame = CompletedGame
  { setup :: GameSetup
  , moves :: [Move]
  , result :: GameResult
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Superset type containing all move information for event stream
data GameMoveEvent = GameMoveEvent
  { move :: Move
  , board :: ExternBoard
  , captures :: Layer
  , zobristHash :: Word64
  , wasBlackTurn :: Bool
  , gameStatus :: EngineGameStatus
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data StateUpdate = StateUpdate GameKey StateUpdatePayload
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data StateUpdatePayload
  = GameClaimed GameSetup
  | GameProgressed GameMoveEvent
  | GameCompleted GameSetup [Move] GameResult
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Convert SearchTrustedResult + Bool to GameMoveEvent
searchResultToGameMoveEvent :: SearchTrustedResult -> Bool -> GameMoveEvent
searchResultToGameMoveEvent result wasBlackTurn =
  GameMoveEvent
    { move = result.searchMove
    , board = result.updatedBoard
    , captures = result.captures
    , zobristHash = result.updatedZobristHash
    , wasBlackTurn = wasBlackTurn
    , gameStatus = result.gameStatus
    }

-- | Convert GameMoveEvent to MoveResult (for UI)
gameMoveEventToMoveResult :: GameMoveEvent -> MoveResult
gameMoveEventToMoveResult event =
  MoveResult
    { move = event.move
    , board = event.board
    , captures = event.captures
    , zobristHash = event.zobristHash
    , wasBlackTurn = event.wasBlackTurn
    }

data ProcessingState = ProcessingState
  { unprocessedGames :: TVar [GameDefinition]
  , inProgressGames :: STMMap.Map GameKey GameDefinition
  , completedGames :: TVar [CompletedGame]
  }

-- For serialization, we need to extract data from STM
data ProcessingStateSnapshot = ProcessingStateSnapshot
  { unprocessedGames :: [GameDefinition]
  , completedGames :: [CompletedGame]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

loadStartPositions ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  FilePath ->
  Eff es ProcessingStateSnapshot
loadStartPositions startPositionsFile = do
  ls <- lines . decodeUtf8 <$> readFile startPositionsFile
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
                    hashes = reverse $ map (.zobristHash) $ toList moveResults
                    mkGameDef newAsBlack pIdx =
                      let setup =
                            GameSetup
                              { id = i
                              , playIndex = pIdx
                              , setupNotation = notation
                              , startingBoard = b
                              , startingBlackToMove = blackToMove
                              , newAsBlack = newAsBlack
                              , startingHashes = hashes
                              }
                          progress =
                            GameProgress
                              { currentBoard = b
                              , currentBlackToMove = blackToMove
                              , currentHashes = hashes
                              , moveCount = 0
                              , selfPlayMoves = []
                              }
                       in GameDefinition setup progress
                 in [mkGameDef newAsBlack pIdx | newAsBlack <- [True, False], pIdx <- [0 .. 2]]
            )
            [0 ..]
            moveLists
  pure $
    ProcessingStateSnapshot
      { unprocessedGames = sortOn (.setup.id) definitions
      , completedGames = mempty
      }

-- | Save ProcessingStateSnapshot to a JSON file
saveProcessingStateSnapshot ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  FilePath -> ProcessingStateSnapshot -> Eff es ()
saveProcessingStateSnapshot filePath processingStateSnapshot = do
  let jsonBytes = encodePretty processingStateSnapshot
  FSBS.writeFile filePath (BSL.toStrict jsonBytes)

-- | Generate canonical filename for state file based on engine versions
getStateFileName :: VersionId -> VersionId -> FilePath
getStateFileName (VersionId v1) (VersionId v2) =
  let sortedVersions = sort [v1, v2]
      versionString = intercalate "_vs_" (map toString sortedVersions)
   in "self_play_state_" <> versionString <> ".json"

-- | Load existing state file or create new one from start positions
loadOrCreateStateSnapshot ::
  (IOE :> es, Error Text :> es, FileSystem :> es) =>
  FilePath -> FilePath -> Eff es ProcessingStateSnapshot
loadOrCreateStateSnapshot stateFilePath startPositionsFile = do
  exists <- doesFileExist stateFilePath
  if exists
    then do
      jsonData <- FSBS.readFile stateFilePath
      case eitherDecodeStrict jsonData of
        Left err -> throwError $ "Failed to parse state file: " <> toText err
        Right processingStateSnapshot ->
          pure $ processingStateSnapshot & #unprocessedGames %~ sortOn (.setup.id)
    else do
      initialState <- loadStartPositions startPositionsFile
      saveProcessingStateSnapshot stateFilePath initialState
      pure initialState

newtype VersionId = VersionId Text
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create ProcessingState from snapshot
mkProcessingState :: ProcessingStateSnapshot -> STM ProcessingState
mkProcessingState snapshot = do
  unprocessedVar <- newTVar snapshot.unprocessedGames
  inProgressMap <- STMMap.new
  completedVar <- newTVar snapshot.completedGames

  pure $ ProcessingState unprocessedVar inProgressMap completedVar

-- | Take snapshot of current state for serialization
-- Convert in-progress games back to unprocessed with their current state
takeSnapshot :: ProcessingState -> STM ProcessingStateSnapshot
takeSnapshot processingState = do
  unprocessed <- readTVar processingState.unprocessedGames
  inProgressList <-
    ListT.toList (STMMap.listT processingState.inProgressGames)
  completed <- readTVar processingState.completedGames

  -- Convert in-progress games back to GameDefinition
  let inProgressAsUnprocessed = map snd inProgressList
      allUnprocessed = sortOn (.setup.id) (unprocessed ++ inProgressAsUnprocessed)

  pure $ ProcessingStateSnapshot allUnprocessed completed

-- | Atomically claim the next unprocessed game and send event
claimNextGame ::
  (IOE :> es, Concurrent :> es) =>
  ProcessingState ->
  TChan StateUpdate ->
  Eff es (Maybe GameDefinition)
claimNextGame processingState eventChan = atomically $ do
  maybeGame <- stateTVar processingState.unprocessedGames $ \case
    [] -> (Nothing, [])
    (game : rest) -> (Just game, rest)

  for maybeGame $ \game -> do
    let key = gameSetupKey game.setup
    STMMap.insert game key processingState.inProgressGames
    writeTChan eventChan (StateUpdate key (GameClaimed game.setup))
    pure game

-- | Advance game progress with a search result
advanceGameProgress :: GameProgress -> SearchTrustedResult -> GameProgress
advanceGameProgress progress result =
  progress
    { currentBoard = result.updatedBoard
    , currentBlackToMove = not progress.currentBlackToMove
    , currentHashes = result.updatedZobristHash : progress.currentHashes
    , moveCount = progress.moveCount + 1
    , selfPlayMoves = progress.selfPlayMoves ++ [result.searchMove]
    }

updateGameProgress ::
  (IOE :> es, Concurrent :> es) =>
  GameKey ->
  SearchTrustedResult ->
  Bool ->
  ProcessingState ->
  TChan StateUpdate ->
  Eff es ()
updateGameProgress key result wasBlackTurn processingState eventChan = do
  let moveEvent = searchResultToGameMoveEvent result wasBlackTurn
  atomically $ do
    STMMap.focus
      (Focus.adjust (\gameDef -> gameDef & #progress %~ (`advanceGameProgress` result)))
      key
      processingState.inProgressGames
    writeTChan eventChan (StateUpdate key (GameProgressed moveEvent))

-- | Complete a game by moving it from in-progress to completed and send event
completeGame ::
  (IOE :> es, Concurrent :> es) =>
  GameKey ->
  GameResult ->
  ProcessingState ->
  TChan StateUpdate ->
  Eff es ()
completeGame key gameResult processingState eventChan = atomically $ do
  maybeGame <-
    STMMap.focus Focus.lookupAndDelete key processingState.inProgressGames
  for_ maybeGame $ \def -> do
    let completed = CompletedGame def.setup def.progress.selfPlayMoves gameResult
    modifyTVar' processingState.completedGames (++ [completed])
    writeTChan
      eventChan
      (StateUpdate key (GameCompleted completed.setup completed.moves gameResult))

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
  -- | The game key
  GameKey ->
  -- | Processing state to update
  ProcessingState ->
  -- | Event channel for updates
  TChan StateUpdate ->
  -- | the number of moves played
  Int ->
  -- | the current board
  ExternBoard ->
  -- | if it is black to move
  Bool ->
  -- | the zobrist hashes to point
  [Word64] ->
  Eff es GameResult
playGame gameKey processingState eventChan moveCount board blackToMove hashes = do
  result <-
    send $
      (Labeled @current) $
        -- Drop first hash because it represents the current position
        -- searchTrusted expects only past positions (game history)
        SearchTrusted board blackToMove (drop 1 hashes) (SearchTimeout 5000)
  case getWinner result.gameStatus of
    Nothing -> do
      updateGameProgress gameKey result blackToMove processingState eventChan
      playGame @next @current
        gameKey
        processingState
        eventChan
        (moveCount + 1)
        result.updatedBoard
        (not blackToMove)
        (result.updatedZobristHash : hashes)
    Just winner -> pure $ GameResult winner moveCount

-- run game loop
beginGame ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  ) =>
  GameDefinition ->
  ProcessingState ->
  TChan StateUpdate ->
  Eff es GameResult
beginGame gameDef processingState eventChan = do
  let key = gameSetupKey gameDef.setup
      prog = gameDef.progress
      newPlaysFirst = prog.currentBlackToMove == gameDef.setup.newAsBlack

  if newPlaysFirst
    then
      playGame @"new" @"old"
        key
        processingState
        eventChan
        prog.moveCount
        prog.currentBoard
        prog.currentBlackToMove
        prog.currentHashes
    else
      playGame @"old" @"new"
        key
        processingState
        eventChan
        prog.moveCount
        prog.currentBoard
        prog.currentBlackToMove
        prog.currentHashes

gameActor ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  ) =>
  ProcessingState ->
  TChan StateUpdate ->
  Eff es ()
gameActor processingState eventChan = do
  -- liftIO $ putStrLn "Actor: trying to claim game"
  maybeGame <- claimNextGame processingState eventChan
  case maybeGame of
    Nothing -> do
      -- liftIO $ putStrLn "Actor: no games available, terminating"
      pure () -- No more games to process
    Just gameDef -> do
      let key = gameSetupKey gameDef.setup
      -- liftIO $
      --   putStrLn $
      --     "Actor: claimed game " <> show (mkGameName key) <> ", starting play"
      result <- beginGame gameDef processingState eventChan
      -- liftIO $
      --   putStrLn $
      --     "Actor: game " <> show (mkGameName key) <> " finished, completing"
      completeGame key result processingState eventChan
      -- liftIO $ putStrLn $ "Actor: game " <> show (mkGameName key) <> " completed, looping"
      gameActor processingState eventChan

-- | Actor that periodically takes snapshots and saves them to disk
snapshotTimerActor ::
  (IOE :> es, Concurrent :> es, Error Text :> es, FileSystem :> es) =>
  FilePath ->
  ProcessingState ->
  Eff es ()
snapshotTimerActor stateFilePath processingState = do
  -- liftIO $
  --   putStrLn "Snapshot timer: starting snapshot timer (10 second intervals)"
  snapshotLoop
 where
  snapshotLoop = do
    liftIO $ threadDelay (10 * 1000 * 1000) -- 10 seconds in microseconds
    snapshot <- atomically $ takeSnapshot processingState
    saveProcessingStateSnapshot stateFilePath snapshot
    snapshotLoop

runGameActors ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  ) =>
  Int ->
  ProcessingState ->
  TChan StateUpdate ->
  Eff es ()
runGameActors numActors processingState eventChan = do
  let spawnActor = gameActor processingState eventChan
  actors <- replicateM numActors (async spawnActor)
  mapM_ wait actors

runSelfPlayParallel ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  , Error Text :> es
  , FileSystem :> es
  ) =>
  Int ->
  VersionId ->
  VersionId ->
  FilePath ->
  FilePath ->
  TChan StateUpdate ->
  Eff es ()
runSelfPlayParallel numActors version1 version2 stateDir startPositionsFile eventChan = do
  let stateFilePath = stateDir </> getStateFileName version1 version2
  snapshot <- loadOrCreateStateSnapshot stateFilePath startPositionsFile

  processingState <- atomically $ mkProcessingState snapshot
  -- Start snapshot timer
  snapshotAsync <- async (snapshotTimerActor stateFilePath processingState)

  -- Wait for all game actors to complete
  runGameActors numActors processingState eventChan

  -- Take final snapshot before terminating
  finalSnapshot <- atomically $ takeSnapshot processingState
  saveProcessingStateSnapshot stateFilePath finalSnapshot

  -- Cancel snapshot timer since all processing is done
  cancel snapshotAsync
