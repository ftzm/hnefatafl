module Hnefatafl.SelfPlayTest where

import Data.Map.Strict qualified as Map
import Hnefatafl.Bindings (EngineGameStatus (..), SearchTrustedResult (..), startBoard)
import Hnefatafl.Core.Data (Move (..))
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameDefinition (..),
  GameName (..),
  GameResult (..),
  InProgressGame (..),
  Player (..),
  ProcessingStateSnapshot (..),
  claimNextGame,
  completeGame,
  mkProcessingState,
  takeSnapshot,
  updateGameProgress,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

spec_round_trip_state_snapshot :: Spec
spec_round_trip_state_snapshot =
  describe "ProcessingState and ProcessingStateSnapshot round trip" $ do
    it "should preserve data when converting state to snapshot and back" $ do
      let testGameDef1 = GameDefinition
            { name = GameName "test-game-1"
            , notation = "a1-a2"
            , board = startBoard
            , blackToMove = True
            , newAsBlack = True
            }
      let testGameDef2 = GameDefinition
            { name = GameName "test-game-2"
            , notation = "b1-b2"
            , board = startBoard
            , blackToMove = False
            , newAsBlack = False
            }
      let testInProgressGame = Claimed testGameDef1
      let testCompletedGame = CompletedGame
            { gameDefinition = testGameDef2
            , result = GameResult { winner = Black, moves = 42 }
            }

      let originalSnapshot = ProcessingStateSnapshot
            { unprocessedGames = [testGameDef1, testGameDef2]
            , inProgressGames = Map.singleton (GameName "test-game-1") testInProgressGame
            , completedGames = [testCompletedGame]
            }

      -- Convert snapshot to state and back to snapshot
      roundTripSnapshot <- atomically $ do
        processingState <- mkProcessingState originalSnapshot
        takeSnapshot processingState

      -- Verify the round trip preserved all data
      roundTripSnapshot `shouldBe` originalSnapshot

spec_claim_next_game :: Spec
spec_claim_next_game =
  describe "claimNextGame procState transition" $ do
    it "should move game from unprocessed to in-progress as Claimed" $ do
      let testGameDef = GameDefinition
            { name = GameName "test-game"
            , notation = "a1-a2"
            , board = startBoard
            , blackToMove = True
            , newAsBlack = True
            }
      let initialSnapshot = ProcessingStateSnapshot
            { unprocessedGames = [testGameDef]
            , inProgressGames = Map.empty
            , completedGames = []
            }

      (claimedGame, finalSnapshot) <- atomically $ do
        procState <- mkProcessingState initialSnapshot
        claimedGame <- claimNextGame procState
        snapshot <- takeSnapshot procState
        pure (claimedGame, snapshot)

      claimedGame `shouldBe` Just testGameDef

      finalSnapshot.unprocessedGames `shouldBe` []
      finalSnapshot.inProgressGames `shouldBe` Map.singleton (GameName "test-game") (Claimed testGameDef)
      finalSnapshot.completedGames `shouldBe` []

    it "should return Nothing when no games are available" $ do
      let emptySnapshot = ProcessingStateSnapshot
            { unprocessedGames = []
            , inProgressGames = Map.empty
            , completedGames = []
            }

      result <- atomically $ do
        procState <- mkProcessingState emptySnapshot
        claimNextGame procState

      result `shouldBe` Nothing

spec_complete_game :: Spec
spec_complete_game =
  describe "completeGame state transition" $ do
    it "should move game from in-progress to completed" $ do
      let testGameDef = GameDefinition
            { name = GameName "test-game"
            , notation = "a1-a2"
            , board = startBoard
            , blackToMove = True
            , newAsBlack = True
            }
      let testResult = GameResult { winner = Black, moves = 42 }
      let initialSnapshot = ProcessingStateSnapshot
            { unprocessedGames = []
            , inProgressGames = Map.singleton (GameName "test-game") (Claimed testGameDef)
            , completedGames = []
            }

      finalSnapshot <- atomically $ do
        procState <- mkProcessingState initialSnapshot
        completeGame (GameName "test-game") testResult procState
        takeSnapshot procState

      let expectedCompletedGame = CompletedGame
            { gameDefinition = testGameDef
            , result = testResult
            }

      finalSnapshot.unprocessedGames `shouldBe` []
      finalSnapshot.inProgressGames `shouldBe` Map.empty
      finalSnapshot.completedGames `shouldBe` [expectedCompletedGame]

spec_update_game_progress :: Spec
spec_update_game_progress =
  describe "updateGameProgress state transition" $ do
    it "should update Claimed game to Running with search result" $ do
      let testGameDef = GameDefinition
            { name = GameName "test-game"
            , notation = "a1-a2"
            , board = startBoard
            , blackToMove = True
            , newAsBlack = True
            }
      let searchResult = SearchTrustedResult
            { searchMove = Move 0 1
            , updatedBoard = startBoard
            , updatedZobristHash = 12345
            , gameStatus = EngineOngoing
            }
      let initialSnapshot = ProcessingStateSnapshot
            { unprocessedGames = []
            , inProgressGames = Map.singleton (GameName "test-game") (Claimed testGameDef)
            , completedGames = []
            }

      finalSnapshot <- atomically $ do
        procState <- mkProcessingState initialSnapshot
        updateGameProgress (GameName "test-game") searchResult procState
        takeSnapshot procState

      finalSnapshot.inProgressGames `shouldBe` Map.singleton (GameName "test-game") (Running testGameDef searchResult)

    it "should update Running game with new search result" $ do
      let testGameDef = GameDefinition
            { name = GameName "test-game"
            , notation = "a1-a2"
            , board = startBoard
            , blackToMove = True
            , newAsBlack = True
            }
      let oldResult = SearchTrustedResult
            { searchMove = Move 0 1
            , updatedBoard = startBoard
            , updatedZobristHash = 12345
            , gameStatus = EngineOngoing
            }
      let newResult = SearchTrustedResult
            { searchMove = Move 1 2
            , updatedBoard = startBoard
            , updatedZobristHash = 67890
            , gameStatus = EngineOngoing
            }
      let initialSnapshot = ProcessingStateSnapshot
            { unprocessedGames = []
            , inProgressGames = Map.singleton (GameName "test-game") (Running testGameDef oldResult)
            , completedGames = []
            }

      finalSnapshot <- atomically $ do
        procState <- mkProcessingState initialSnapshot
        updateGameProgress (GameName "test-game") newResult procState
        takeSnapshot procState

      finalSnapshot.inProgressGames `shouldBe` Map.singleton (GameName "test-game") (Running testGameDef newResult)