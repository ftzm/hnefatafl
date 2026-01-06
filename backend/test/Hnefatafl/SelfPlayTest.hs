{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Hnefatafl.SelfPlayTest where

import Data.List (findIndex, (!!), (\\))
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (async, wait)
import Effectful.Concurrent.STM (
  TChan,
  newTChan,
  tryReadTChan,
 )
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (runFileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FSBS
import Effectful.Labeled (Labeled (..), runLabeled)
import Hnefatafl.Bindings (
  EngineGameStatus (..),
  SearchTrustedResult (..),
  applyMoveSequence,
  startBoard,
 )
import Hnefatafl.Core.Data (Layer (..), Move (..), MoveResult (..))
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Interpreter.Search.Test (TestSearchConfig (..), runSearchTest)
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameDefinition (..),
  GameName (..),
  GameResult (..),
  InProgressGame (..),
  Player (..),
  ProcessingState (..),
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  VersionId (..),
  beginGame,
  claimNextGame,
  completeGame,
  gameActor,
  loadOrCreateStateSnapshot,
  mkProcessingState,
  runSelfPlayParallel,
  saveProcessingStateSnapshot,
  takeSnapshot,
  updateGameProgress,
 )
import Hnefatafl.Serialization (parseMoveList)
import ListT qualified
import StmContainers.Map qualified as STMMap
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Utility function to create ProcessingState directly from components
mkProcessingStateFromComponents ::
  [GameDefinition] ->
  Map GameName InProgressGame ->
  [CompletedGame] ->
  STM ProcessingState
mkProcessingStateFromComponents unprocessed inProgress completed = do
  unprocessedVar <- newTVar unprocessed
  inProgressMap <- STMMap.new
  completedVar <- newTVar completed

  -- Populate in-progress map
  forM_ (Map.toList inProgress) $ \(name, game) -> do
    STMMap.insert game name inProgressMap

  pure $ ProcessingState unprocessedVar inProgressMap completedVar

spec_round_trip_state_snapshot :: Spec
spec_round_trip_state_snapshot =
  describe "ProcessingState and ProcessingStateSnapshot round trip" $ do
    it "should preserve data when converting state to snapshot and back" $ do
      let testGameDef1 =
            GameDefinition
              { name = GameName "test-game-1"
              , notation = "a1-a2"
              , board = startBoard
              , blackToMove = True
              , newAsBlack = True
              , hashes = []
              , moveCount = 0
              }
      let testGameDef2 =
            GameDefinition
              { name = GameName "test-game-2"
              , notation = "b1-b2"
              , board = startBoard
              , blackToMove = False
              , newAsBlack = False
              , hashes = []
              , moveCount = 0
              }
      let testCompletedGame =
            CompletedGame
              { gameDefinition = testGameDef2
              , result = GameResult{winner = Black, moves = 42}
              }

      let originalSnapshot =
            ProcessingStateSnapshot
              { unprocessedGames = [testGameDef1, testGameDef2]
              , completedGames = [testCompletedGame]
              }

      -- Convert snapshot to state and back to snapshot
      roundTripSnapshot <- atomically $ do
        processingState <- mkProcessingState originalSnapshot
        takeSnapshot processingState

      -- Verify the round trip preserves data
      let expectedSnapshot =
            ProcessingStateSnapshot
              { unprocessedGames = [testGameDef1, testGameDef2] -- original unprocessed games
              , completedGames = [testCompletedGame] -- completed games remain the same
              }

      roundTripSnapshot `shouldBe` expectedSnapshot

spec_claim_next_game :: Spec
spec_claim_next_game =
  describe "claimNextGame procState transition" $ do
    it "should move game from unprocessed to in-progress as Claimed" $ do
      let testGameDef =
            GameDefinition
              { name = GameName "test-game"
              , notation = "a1-a2"
              , board = startBoard
              , blackToMove = True
              , newAsBlack = True
              , hashes = []
              , moveCount = 0
              }
      let initialSnapshot =
            ProcessingStateSnapshot
              { unprocessedGames = [testGameDef]
              , completedGames = []
              }

      result <- runEff $
        runFileSystem $
          runConcurrent $ do
            eventChan <- atomically newTChan
            procState <- atomically $ mkProcessingState initialSnapshot
            claimedGame <- claimNextGame procState eventChan
            finalSnapshot <- atomically $ takeSnapshot procState
            pure (claimedGame, finalSnapshot)

      let (claimedGame, finalSnapshot) = result

      claimedGame `shouldBe` Just testGameDef

      finalSnapshot.unprocessedGames `shouldBe` [testGameDef] -- claimed game converted back to unprocessed
      finalSnapshot.completedGames `shouldBe` []

    it "should return Nothing when no games are available" $ do
      let emptySnapshot =
            ProcessingStateSnapshot
              { unprocessedGames = []
              , completedGames = []
              }

      result <- runEff $
        runFileSystem $
          runConcurrent $ do
            eventChan <- atomically newTChan
            procState <- atomically $ mkProcessingState emptySnapshot
            claimNextGame procState eventChan

      result `shouldBe` Nothing

spec_complete_game :: Spec
spec_complete_game =
  describe "completeGame state transition" $ do
    it "should move game from in-progress to completed" $ do
      let testGameDef =
            GameDefinition
              { name = GameName "test-game"
              , notation = "a1-a2"
              , board = startBoard
              , blackToMove = True
              , newAsBlack = True
              , hashes = []
              , moveCount = 0
              }
      let testResult = GameResult{winner = Black, moves = 42}
      finalSnapshot <- runEff $
        runFileSystem $
          runConcurrent $ do
            eventChan <- atomically newTChan
            procState <-
              atomically $
                mkProcessingStateFromComponents
                  [] -- no unprocessed games
                  (Map.singleton (GameName "test-game") (Claimed testGameDef)) -- game already claimed
                  [] -- no completed games yet
            completeGame (GameName "test-game") testResult procState eventChan
            atomically $ takeSnapshot procState

      let expectedCompletedGame =
            CompletedGame
              { gameDefinition = testGameDef
              , result = testResult
              }

      finalSnapshot.unprocessedGames `shouldBe` []
      finalSnapshot.completedGames `shouldBe` [expectedCompletedGame]

spec_update_game_progress :: Spec
spec_update_game_progress =
  describe "updateGameProgress state transition" $ do
    it "should update Claimed game to Running with search result" $ do
      let testGameDef =
            GameDefinition
              { name = GameName "test-game"
              , notation = "a1-a2"
              , board = startBoard
              , blackToMove = True
              , newAsBlack = True
              , hashes = []
              , moveCount = 0
              }
      let searchResult =
            SearchTrustedResult
              { searchMove = Move 0 1
              , updatedBoard = startBoard
              , captures = Layer 0 0
              , updatedZobristHash = 12345
              , gameStatus = EngineOngoing
              }
      finalSnapshot <- runEff $
        runFileSystem $
          runConcurrent $ do
            eventChan <- atomically newTChan
            procState <-
              atomically $
                mkProcessingStateFromComponents
                  [] -- no unprocessed games
                  (Map.singleton (GameName "test-game") (Claimed testGameDef)) -- game already claimed
                  [] -- no completed games
            updateGameProgress (GameName "test-game") searchResult True procState eventChan
            atomically $ takeSnapshot procState

      let expectedGameDef =
            testGameDef
              { moveCount = 1
              , blackToMove = not testGameDef.blackToMove -- turn flipped after move
              , hashes = [12345] -- hash added from search result
              }
      finalSnapshot.unprocessedGames `shouldBe` [expectedGameDef] -- game converted back with updated state
    it "should update Running game with new search result" $ do
      let testGameDef =
            GameDefinition
              { name = GameName "test-game"
              , notation = "a1-a2"
              , board = startBoard
              , blackToMove = True
              , newAsBlack = True
              , hashes = []
              , moveCount = 0
              }
      let newResult =
            SearchTrustedResult
              { searchMove = Move 1 2
              , updatedBoard = startBoard
              , captures = Layer 0 0
              , updatedZobristHash = 67890
              , gameStatus = EngineOngoing
              }
      let currentGameDef =
            testGameDef
              { moveCount = 1
              , blackToMove = not testGameDef.blackToMove
              , hashes = [12345] -- already has the hash from first move
              }
      finalSnapshot <- runEff $
        runFileSystem $
          runConcurrent $ do
            eventChan <- atomically newTChan
            procState <-
              atomically $
                mkProcessingStateFromComponents
                  [] -- no unprocessed games
                  ( Map.singleton
                      (GameName "test-game")
                      (Running currentGameDef (Move 0 1) (Layer 0 0)) -- game already running
                  )
                  [] -- no completed games
            updateGameProgress (GameName "test-game") newResult False procState eventChan
            atomically $ takeSnapshot procState

      let expectedGameDef =
            testGameDef
              { moveCount = 2
              , blackToMove = testGameDef.blackToMove -- flipped back after second move (True -> False -> True)
              , hashes = [67890, 12345] -- both hashes added, newest first
              }
      finalSnapshot.unprocessedGames `shouldBe` [expectedGameDef] -- game converted back with updated state

-- | Create a complex live processing state with games in various stages using real game data
createComplexLiveState :: Text -> Either Text (STM ProcessingState)
createComplexLiveState gameNotation = do
  moves <- parseMoveList gameNotation
  let (moveResults, finalStatus) = applyMoveSequence moves
      allResults = toList moveResults

      -- Helper to create GameDefinition at specific move count
      makeGameDef :: GameName -> Int -> Bool -> GameDefinition
      makeGameDef name moveCount newAsBlack =
        if moveCount == 0
          then
            GameDefinition
              { name = name
              , notation = gameNotation
              , board = startBoard
              , blackToMove = True
              , newAsBlack = newAsBlack
              , hashes = []
              , moveCount = 0
              }
          else
            let result = allResults !! (moveCount - 1) -- 0-indexed
                hashes = map (.zobristHash) (take moveCount allResults)
             in GameDefinition
                  { name = name
                  , notation = gameNotation
                  , board = result.board
                  , blackToMove = not result.wasBlackTurn
                  , newAsBlack = newAsBlack
                  , hashes = hashes
                  , moveCount = moveCount
                  }

      -- Create 3 unprocessed games - all starting from initial position
      unprocessedGame1 = makeGameDef (GameName "unprocessed-1") 0 True
      unprocessedGame2 = makeGameDef (GameName "unprocessed-2") 0 False
      unprocessedGame3 = makeGameDef (GameName "unprocessed-3") 0 True

      -- Create 6 in-progress games at different actual positions in the game
      claimedGameStart = makeGameDef (GameName "claimed-game-start") 0 True
      claimedGame = makeGameDef (GameName "claimed-game") 5 False
      claimedGame2 = makeGameDef (GameName "claimed-game-2") 7 True
      runningGameDef = makeGameDef (GameName "running-game") 12 True
      runningGame2Def = makeGameDef (GameName "running-game-2") 15 False
      runningGame3Def = makeGameDef (GameName "running-game-3") 20 False

      -- Get actual moves and captures for running games
      move12 = allResults !! 11 -- The actual 12th move
      move15 = allResults !! 14 -- The actual 15th move
      move20 = allResults !! 19 -- The actual 20th move

      -- Convert EngineGameStatus to Player
      engineStatusToPlayer :: EngineGameStatus -> Player
      engineStatusToPlayer = \case
        EngineKingCaptured -> Black
        EngineWhiteSurrounded -> Black
        EngineNoWhiteMoves -> Black
        EngineKingEscaped -> White
        EngineExitFort -> White
        EngineNoBlackMoves -> White
        EngineOngoing -> error "Game should not be ongoing for completed games"

      -- Create 3 completed games - representing final game state
      finalMoveCount = length allResults
      winner = engineStatusToPlayer finalStatus
      completedGame1 =
        CompletedGame
          { gameDefinition = makeGameDef (GameName "completed-1") finalMoveCount True
          , result = GameResult winner finalMoveCount
          }
      completedGame2 =
        CompletedGame
          { gameDefinition = makeGameDef (GameName "completed-2") finalMoveCount False
          , result = GameResult winner finalMoveCount
          }
      completedGame3 =
        CompletedGame
          { gameDefinition = makeGameDef (GameName "completed-3") finalMoveCount True
          , result = GameResult winner finalMoveCount
          }

      -- Use REAL moves and captures from the actual game
      inProgressGames =
        Map.fromList
          [ (GameName "claimed-game-start", Claimed claimedGameStart)
          , (GameName "claimed-game", Claimed claimedGame)
          , (GameName "claimed-game-2", Claimed claimedGame2)
          , (GameName "running-game", Running runningGameDef move12.move move12.captures)
          , (GameName "running-game-2", Running runningGame2Def move15.move move15.captures)
          , (GameName "running-game-3", Running runningGame3Def move20.move move20.captures)
          ]

  pure $
    mkProcessingStateFromComponents
      [unprocessedGame1, unprocessedGame2, unprocessedGame3]
      inProgressGames
      [completedGame1, completedGame2, completedGame3]

-- | Get all current in-progress games from the STMMap
getAllInProgressGames :: ProcessingState -> STM [GameDefinition]
getAllInProgressGames processingState = do
  inProgressList <- ListT.toList (STMMap.listT processingState.inProgressGames)
  pure $ map extractGameDef inProgressList
 where
  extractGameDef (_, inProgressGame) = case inProgressGame of
    Claimed def -> def
    Running def _ _ -> def

-- | Drain all available items from a TChan without blocking
drainTChan :: (Concurrent :> es, IOE :> es) => TChan a -> Eff es [a]
drainTChan chan = do
  maybeItem <- atomically $ tryReadTChan chan
  case maybeItem of
    Nothing -> pure []
    Just item -> do
      rest <- drainTChan chan
      pure (item : rest)

-- | Specialized runner that processes both unprocessed and in-progress games to completion
runAllGamesToCompletion ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  ) =>
  ProcessingState ->
  Eff es (ProcessingStateSnapshot, [StateUpdate])
runAllGamesToCompletion processingState = do
  eventChan <- atomically newTChan

  -- First, resume all in-progress games using beginGame
  inProgressGames <- atomically $ getAllInProgressGames processingState
  for_ inProgressGames $ \gameDef -> do
    result <- beginGame gameDef processingState eventChan
    completeGame gameDef.name result processingState eventChan

  -- Then process any remaining unprocessed games using the standard gameActor
  gameActor processingState eventChan

  liftIO $ putStrLn "getting chan contents"
  -- Collect all events from the channel
  events <- drainTChan eventChan

  -- Take final snapshot
  finalSnapshot <- atomically $ takeSnapshot processingState

  pure (finalSnapshot, events)

-- | Compare two final states for equivalence
compareFinalStates :: ProcessingStateSnapshot -> ProcessingStateSnapshot -> Bool
compareFinalStates state1 state2 =
  -- Unprocessed games should be empty for both (all games completed)
  null state1.unprocessedGames
    && null state2.unprocessedGames
    &&
    -- Completed games should be equivalent (order-independent, detects duplicates)
    sort state1.completedGames == sort state2.completedGames

-- | Compare two event sequences for equivalence
-- The restored path is allowed to have extra GameClaimed events (due to snapshot conversion)
compareEventSequences :: [StateUpdate] -> [StateUpdate] -> Bool
compareEventSequences directEvents restoredEvents =
  let sortedDirect = sort directEvents
      sortedRestored = sort restoredEvents
      extraInRestored = sortedRestored \\ sortedDirect
      missingInRestored = sortedDirect \\ sortedRestored
      -- No events should be missing in restored path
      noMissingEvents = null missingInRestored
      -- All extra events must be GameClaimed events
      allExtraAreGameClaimed = all isGameClaimed extraInRestored
   in noMissingEvents && allExtraAreGameClaimed
 where
  isGameClaimed (GameClaimed _ _) = True
  isGameClaimed _ = False

-- | Round trip test: verify that snapshot serialization preserves all state
spec_round_trip_snapshot_preservation :: Spec
spec_round_trip_snapshot_preservation =
  describe "Round trip snapshot preservation" $ do
    it "should produce identical results when processing directly vs via snapshot" $ do
      case createComplexLiveState testGameNotation of
        Left err -> error $ "Failed to create complex live state: " <> err
        Right createState -> do
          -- Create temporary directory for snapshot files
          tempDir <- createTempDirectory "/tmp" "round-trip-test"
          let snapshotFile = tempDir </> "test_snapshot.json"

          result <- runEff $
            runErrorNoCallStack @Text $
              runFileSystem $
                runConcurrent $
                  runLabeled @"new" (runSearchTest (TestSearchConfig testGameNotation)) $
                    runLabeled @"old" (runSearchTest (TestSearchConfig testGameNotation)) $ do
                      -- Path 1: Direct processing
                      liftIO $ putStrLn "=== Direct Processing Path ==="
                      directState <- atomically createState
                      directSnapshot <- atomically $ takeSnapshot directState
                      liftIO $
                        putStrLn $
                          "Direct initial state: "
                            <> show (length directSnapshot.unprocessedGames)
                            <> " unprocessed, "
                            <> show (length directSnapshot.completedGames)
                            <> " completed"
                      (directFinalState, directEvents) <-
                        runAllGamesToCompletion directState
                      liftIO $
                        putStrLn $
                          "Direct path completed " <> show (length directEvents) <> " events"

                      -- Path 2: Snapshot round trip
                      liftIO $ putStrLn "=== Snapshot Round Trip Path ==="
                      snapshotState <- atomically createState -- Create identical initial state
                      initialSnapshot <- atomically $ takeSnapshot snapshotState

                      -- Serialize to JSON
                      saveProcessingStateSnapshot snapshotFile initialSnapshot
                      liftIO $ putStrLn "Snapshot saved to file"

                      -- Deserialize from JSON
                      restoredSnapshot <- loadOrCreateStateSnapshot snapshotFile "dummy_file.txt"
                      liftIO $ putStrLn "Snapshot loaded from file"
                      liftIO $
                        putStrLn $
                          "Restored snapshot: "
                            <> show (length restoredSnapshot.unprocessedGames)
                            <> " unprocessed, "
                            <> show (length restoredSnapshot.completedGames)
                            <> " completed"

                      -- Process restored state
                      restoredState <- atomically $ mkProcessingState restoredSnapshot
                      (restoredFinalState, restoredEvents) <-
                        runAllGamesToCompletion restoredState
                      liftIO $
                        putStrLn $
                          "Restored path completed " <> show (length restoredEvents) <> " events"

                      pure (directFinalState, directEvents, restoredFinalState, restoredEvents)

          -- Cleanup
          removeDirectoryRecursive tempDir

          case result of
            Left err -> error $ "Round trip test failed: " <> err
            Right (directFinalState, directEvents, restoredFinalState, restoredEvents) -> do
              liftIO $ putStrLn $ "Direct events count: " <> show (length directEvents)
              liftIO $ putStrLn $ "Restored events count: " <> show (length restoredEvents)
              liftIO $
                putStrLn $
                  "Final states equal: "
                    <> show (compareFinalStates directFinalState restoredFinalState)
              liftIO $
                putStrLn $
                  "Event sequences equal: "
                    <> show (compareEventSequences directEvents restoredEvents)

              -- Find the exact difference
              let sortedDirect = sort directEvents
                  sortedRestored = sort restoredEvents
                  extraInRestored = sortedRestored \\ sortedDirect
                  missingInRestored = sortedDirect \\ sortedRestored
              liftIO $
                putStrLn $
                  "Extra events in restored: " <> show (length extraInRestored)
              liftIO $
                putStrLn $
                  "Missing events in restored: " <> show (length missingInRestored)
              unless (null extraInRestored) $
                liftIO $
                  putStrLn $
                    "First extra: " <> show (viaNonEmpty head extraInRestored)
              unless (null missingInRestored) $
                liftIO $
                  putStrLn $
                    "First missing: " <> show (viaNonEmpty head missingInRestored)

              -- Verify final states are equivalent
              compareFinalStates directFinalState restoredFinalState `shouldBe` True

              -- Verify event sequences are equivalent
              compareEventSequences directEvents restoredEvents `shouldBe` True

              liftIO $ putStrLn "Round trip test completed successfully!"

-- | Generate a test positions file from the known TestTest game at various move counts
generateTestPositionsFile ::
  (IOE :> es, FileSystem :> es) =>
  FilePath ->
  Eff es ()
generateTestPositionsFile filePath = do
  let moves = words testGameNotation
      -- Generate positions at various move counts (first 30 moves)
      moveCounts = [3, 5, 8, 12, 15, 18, 22, 26, 28, 30]
      positions = map (\n -> unwords $ take n moves) moveCounts
      content = unlines positions

  FSBS.writeFile filePath (encodeUtf8 content)

-- | Runner for generateTestPositionsFile that handles all required effects
runGenerateTestPositionsFile :: FilePath -> IO ()
runGenerateTestPositionsFile filePath =
  runEff
    . runFileSystem
    $ generateTestPositionsFile filePath


testGameNotation :: Text
testGameNotation =
  "d11-d9 h6-h3 k7-i7 f8-c8 a4-c4 f4-i4 g1-g2 e5-c5 d1-d3 g5-j5 k4-j4xj5 f5-j5 h11-h4xi4 c5-h5xh4 i7-i5xj5 g7-g9 g2-g5xh5 f7-k7 i5-i9 f6-f8 j6-h6xg6 f8-j8 i9-j9 j8-i8 k6-i6 i8-i11 j9-j11 i11-i10 j11-j10 g9-k9xk8 g11-i11 k9-k10xj10 j4-j10 k10-k9 h6-h10 k9-k10xj10 d9-k9xk10 i10-k10 h10-j10 k10-k11"

-- | Test a single game actor in isolation
spec_single_game_actor :: Spec
spec_single_game_actor =
  describe "Single game actor test" $ do
    it "should run a single actor and complete one game" $ do
      liftIO $ putStrLn "=== Testing single game actor ==="

      -- Setup
      tempDir <- createTempDirectory "/tmp" "single-actor-test"
      let testPositionsFile = tempDir </> "single_position.txt"

      runGenerateTestPositionsFile testPositionsFile

      let config = TestSearchConfig{gameNotation = testGameNotation}

      result <- runEff $
        runErrorNoCallStack @Text $
          runFileSystem $
            runConcurrent $
              runLabeled @"new" (runSearchTest config) $
                runLabeled @"old" (runSearchTest config) $ do
                  -- Load the state manually
                  snapshot <-
                    loadOrCreateStateSnapshot
                      (tempDir </> "test_state.json")
                      testPositionsFile

                  processingState <- atomically $ mkProcessingState snapshot
                  eventChan <- atomically newTChan

                  liftIO $ putStrLn "About to run single game actor..."

                  gameActor processingState eventChan
                  liftIO $ putStrLn "Actor run, collecting events..."
                  drainTChan eventChan

      removeDirectoryRecursive tempDir

      case result of
        Left err -> error $ "Single actor test failed: " <> err
        Right events -> do
          liftIO $
            putStrLn $
              "Single actor test completed with " <> show (length events) <> " events"
          length events `shouldSatisfy` (>= 2) -- Should have at least GameClaimed and GameCompleted
      case result of
        Left err -> error $ "Test failed: " <> err
        Right events -> do
          -- Verify we got the expected number of events
          let claimedEvents = filter isGameClaimed events
              progressedEvents = filter isGameProgressed events
              completedEvents = filter isGameCompleted events

          -- Should have exactly 10 games claimed and completed
          length claimedEvents `shouldBe` 20
          length completedEvents `shouldBe` 20

          -- Should have some progress events (at least one per game)
          length progressedEvents `shouldSatisfy` (>= 20)

          -- All events should be in logical order (claimed before completed for each game)
          events `shouldSatisfy` eventsInLogicalOrder
 where
  isGameClaimed (GameClaimed _ _) = True
  isGameClaimed _ = False

  isGameProgressed (GameProgressed _ _) = True
  isGameProgressed _ = False

  isGameCompleted (GameCompleted _ _) = True
  isGameCompleted _ = False

  -- Check that for each game, GameClaimed comes before GameCompleted
  eventsInLogicalOrder :: [StateUpdate] -> Bool
  eventsInLogicalOrder events =
    let gameNames = getAllGameNames events
     in all (gameEventsInOrder events) gameNames

  getAllGameNames :: [StateUpdate] -> [GameName]
  getAllGameNames events = ordNub [name | GameClaimed name _ <- events]

  gameEventsInOrder :: [StateUpdate] -> GameName -> Bool
  gameEventsInOrder events gameName =
    let gameEvents = filter (isGameEvent gameName) events
        hasClaimed = any (isClaimedForGame gameName) gameEvents
        hasCompleted = any (isCompletedForGame gameName) gameEvents
        claimedIndex = findIndex (isClaimedForGame gameName) gameEvents
        completedIndex = findIndex (isCompletedForGame gameName) gameEvents
     in hasClaimed
          && hasCompleted
          && case (claimedIndex, completedIndex) of
            (Just ci, Just coi) -> ci < coi
            _ -> False

  isGameEvent :: GameName -> StateUpdate -> Bool
  isGameEvent name (GameClaimed n _) = n == name
  isGameEvent name (GameProgressed n _) = n == name
  isGameEvent name (GameCompleted n _) = n == name

  isClaimedForGame :: GameName -> StateUpdate -> Bool
  isClaimedForGame name (GameClaimed n _) = n == name
  isClaimedForGame _ _ = False

  isCompletedForGame :: GameName -> StateUpdate -> Bool
  isCompletedForGame name (GameCompleted n _) = n == name
  isCompletedForGame _ _ = False

-- | End-to-end test for parallel self-play
spec_end_to_end_parallel_self_play :: Spec
spec_end_to_end_parallel_self_play =
  describe "End-to-end parallel self-play" $ do
    it "should run 4 actors processing 10 test games to completion" $ do
      liftIO $ putStrLn "=== Starting end-to-end test ==="

      -- Setup: Create temp directory and test files
      tempDir <- createTempDirectory "/tmp" "self-play-test"
      liftIO $ putStrLn $ "Created temp directory: " <> tempDir

      let testPositionsFile = tempDir </> "test_positions.txt"
      runGenerateTestPositionsFile testPositionsFile
      liftIO $ putStrLn "Generated test positions file"

      -- Run the parallel self-play
      let config = TestSearchConfig{gameNotation = testGameNotation}

      liftIO $ putStrLn "About to start parallel self-play..."

      _ <- runEff $
        runErrorNoCallStack @Text $
          runFileSystem $
            runConcurrent $
              runLabeled @"new" (runSearchTest config) $
                runLabeled @"old" (runSearchTest config) $ do
                  eventChan <- atomically newTChan

                  -- Start self-play in the background
                  selfPlayAsync <- async $
                    runSelfPlayParallel
                      4 -- 4 actors
                      (VersionId "test-new")
                      (VersionId "test-old")
                      tempDir
                      testPositionsFile
                      eventChan

                  -- Add initial logging
                  liftIO $ putStrLn "Starting self-play, will wait for completion..."

                  -- Wait for self-play to complete
                  wait selfPlayAsync

                  -- Collect all events now that processing is done
                  events <- drainTChan eventChan

                  liftIO $
                    putStrLn $
                      "Self-play complete. Total events: " <> show (length events)
                  pure events

      -- Cleanup
      removeDirectoryRecursive tempDir
