{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.SelfPlay (
  Player (..),
  GameResult (..),
  GameDefinition (..),
  InProgressGame (..),
  CompletedGame (..),
  GameMoveEvent (..),
  StateUpdate (..),
  GameName (..),
  ProcessingState (..),
  ProcessingStateSnapshot (..),
  VersionId (..),
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
  FromJSONKey,
  ToJSON,
  ToJSONKey,
  eitherDecodeStrict,
  withObject,
  (.!=),
  (.:),
  (.:?),
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
import StmContainers.Map qualified as STMMap

-- import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Prelude hiding (readFile)

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
  , hashes :: [Word64]
  , moveCount :: Int
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON)

instance FromJSON GameDefinition where
  parseJSON = withObject "GameDefinition" $ \o -> do
    name <- o .: "name"
    notation <- o .: "notation"
    board <- o .: "board"
    blackToMove <- o .: "blackToMove"
    newAsBlack <- o .: "newAsBlack"
    hashes <- o .: "hashes"
    moveCount <- o .:? "moveCount" .!= 0
    pure $
      GameDefinition name notation board blackToMove newAsBlack hashes moveCount

data InProgressGame
  = Claimed GameDefinition
  | Running GameDefinition Move Layer
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data CompletedGame = CompletedGame
  { gameDefinition :: GameDefinition
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

data StateUpdate
  = GameClaimed GameName GameDefinition
  | GameProgressed GameName GameMoveEvent
  | GameCompleted GameName GameResult
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

newtype GameName = GameName Text
  deriving (Show, Read, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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
  , inProgressGames :: STMMap.Map GameName InProgressGame
  , completedGames :: TVar [CompletedGame]
  }

-- For serialization, we need to extract data from STM
data ProcessingStateSnapshot = ProcessingStateSnapshot
  { unprocessedGames :: [GameDefinition]
  , completedGames :: [CompletedGame]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGameName :: Int -> Bool -> GameName
mkGameName index newIsBlack = GameName $ show index <> " " <> players
 where
  players = if newIsBlack then "New vs. Old" else "Old vs. New"

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
                    mkGameDef newAsBlack =
                      GameDefinition
                        { name = mkGameName i newAsBlack
                        , notation = notation
                        , board = b
                        , blackToMove = blackToMove
                        , newAsBlack = newAsBlack
                        , hashes = reverse $ map (.zobristHash) $ toList moveResults
                        , moveCount = 0
                        }
                 in [mkGameDef True, mkGameDef False]
            )
            [0 ..]
            moveLists
  pure $
    ProcessingStateSnapshot
      { unprocessedGames = definitions
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
        Right processingStateSnapshot -> pure processingStateSnapshot
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
  let inProgressAsUnprocessed = map (convertToGameDefinition . snd) inProgressList
      allUnprocessed = unprocessed ++ inProgressAsUnprocessed

  pure $ ProcessingStateSnapshot allUnprocessed completed
 where
  convertToGameDefinition :: InProgressGame -> GameDefinition
  convertToGameDefinition = \case
    Claimed gameDef -> gameDef
    Running gameDef _ _ -> gameDef -- GameDefinition already has current state

-- | Atomically claim the next unprocessed game and send event
claimNextGame ::
  (IOE :> es, Concurrent :> es) =>
  ProcessingState ->
  TChan StateUpdate ->
  Eff es (Maybe GameDefinition)
claimNextGame processingState eventChan = do
  maybeGame <- atomically $ do
    maybeGame <- stateTVar processingState.unprocessedGames $ \case
      [] -> (Nothing, [])
      (game : rest) -> (Just game, rest)

    for maybeGame $ \game -> do
      STMMap.insert (Claimed game) game.name processingState.inProgressGames
      pure game

  for_ maybeGame $ \game -> do
    atomically $ writeTChan eventChan (GameClaimed game.name game)

  pure maybeGame

updateGameProgress ::
  (IOE :> es, Concurrent :> es) =>
  GameName ->
  SearchTrustedResult ->
  Bool ->
  ProcessingState ->
  TChan StateUpdate ->
  Eff es ()
updateGameProgress name result wasBlackTurn processingState eventChan = do
  atomically $
    STMMap.focus (Focus.adjust updateGame) name processingState.inProgressGames
  let moveEvent = searchResultToGameMoveEvent result wasBlackTurn
  atomically $ writeTChan eventChan (GameProgressed name moveEvent)
 where
  updateGame = \case
    Claimed gameDef ->
      let updatedGameDef =
            gameDef
              { board = result.updatedBoard
              , blackToMove = not gameDef.blackToMove
              , hashes = result.updatedZobristHash : gameDef.hashes
              , moveCount = gameDef.moveCount + 1
              }
       in Running updatedGameDef result.searchMove result.captures
    Running gameDef _ _ ->
      let updatedGameDef =
            gameDef
              { board = result.updatedBoard
              , blackToMove = not gameDef.blackToMove
              , hashes = result.updatedZobristHash : gameDef.hashes
              , moveCount = gameDef.moveCount + 1
              }
       in Running updatedGameDef result.searchMove result.captures

-- | Complete a game by moving it from in-progress to completed and send event
completeGame ::
  (IOE :> es, Concurrent :> es) =>
  GameName ->
  GameResult ->
  ProcessingState ->
  TChan StateUpdate ->
  Eff es ()
completeGame name gameResult processingState eventChan = do
  atomically $ do
    maybeGame <-
      STMMap.focus Focus.lookupAndDelete name processingState.inProgressGames
    let gameDefinition =
          maybeGame <&> \case
            Claimed def -> def
            Running def _ _ -> def
    for_
      gameDefinition
      \def ->
        modifyTVar' processingState.completedGames (++ [CompletedGame def gameResult])

  atomically $ writeTChan eventChan (GameCompleted name gameResult)

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
  GameName ->
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
playGame gameName processingState eventChan moveCount board blackToMove hashes = do
  result <-
    send $
      (Labeled @current) $
        -- Drop first hash because it represents the current position
        -- searchTrusted expects only past positions (game history)
        SearchTrusted board blackToMove (drop 1 hashes) (SearchTimeout 5000)
  case getWinner result.gameStatus of
    Nothing -> do
      updateGameProgress gameName result blackToMove processingState eventChan
      playGame @next @current
        gameName
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
  if (gameDef.blackToMove && gameDef.newAsBlack)
    || (not gameDef.blackToMove && not gameDef.newAsBlack)
    then do
      liftIO $ putStrLn $ "Actor: playing as new@old for " <> show gameDef.name
      playGame @"new" @"old"
        gameDef.name
        processingState
        eventChan
        gameDef.moveCount
        gameDef.board
        gameDef.blackToMove
        gameDef.hashes
    else do
      liftIO $ putStrLn $ "Actor: playing as old@new for " <> show gameDef.name
      playGame @"old" @"new"
        gameDef.name
        processingState
        eventChan
        gameDef.moveCount
        gameDef.board
        gameDef.blackToMove
        gameDef.hashes

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
  liftIO $ putStrLn "Actor: trying to claim game"
  maybeGame <- claimNextGame processingState eventChan
  case maybeGame of
    Nothing -> do
      liftIO $ putStrLn "Actor: no games available, terminating"
      pure () -- No more games to process
    Just gameDef -> do
      liftIO $
        putStrLn $
          "Actor: claimed game " <> show gameDef.name <> ", starting play"
      result <- beginGame gameDef processingState eventChan
      liftIO $
        putStrLn $
          "Actor: game " <> show gameDef.name <> " finished, completing"
      completeGame gameDef.name result processingState eventChan
      liftIO $ putStrLn $ "Actor: game " <> show gameDef.name <> " completed, looping"
      gameActor processingState eventChan

-- | Actor that periodically takes snapshots and saves them to disk
snapshotTimerActor ::
  (IOE :> es, Concurrent :> es, Error Text :> es, FileSystem :> es) =>
  FilePath ->
  ProcessingState ->
  Eff es ()
snapshotTimerActor stateFilePath processingState = do
  liftIO $
    putStrLn "Snapshot timer: starting snapshot timer (10 second intervals)"
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
