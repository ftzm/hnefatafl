module Hnefatafl.SelfPlay.UITest where

import Hnefatafl.Bindings (startBoard)
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameSetup (..),
  GameResult (..),
  Player (..),
  StateUpdate (..),
  StateUpdatePayload (..),
 )
import Hnefatafl.SelfPlay.UI (ScoreState (..), mkInitialScoreState, updateScoreState)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Utility to create test completed games
mkTestCompletedGame :: Int -> Int -> Bool -> Player -> Int -> CompletedGame
mkTestCompletedGame gameId playIdx newAsBlack winner moves =
  CompletedGame
    { setup =
        GameSetup
          { id = gameId
          , playIndex = playIdx
          , setupNotation = "test"
          , startingBoard = startBoard
          , startingBlackToMove = True
          , newAsBlack = newAsBlack
          , startingHashes = []
          }
    , moves = []
    , result = GameResult{winner = winner, moves = moves}
    }

-- | Test that building ScoreState from completed games matches incremental updates
spec_score_state_batch_vs_incremental :: Spec
spec_score_state_batch_vs_incremental =
  describe "ScoreState batch vs incremental" $ do
    it "should produce same result when built from snapshot vs incremental updates" $ do
      -- Create test game sets with known results (6 games per ID: 3 per side)
      -- Pair 0: New wins both sides (majority on each side)
      -- newAsBlack=True: 2 Black wins, 1 White win -> New wins
      -- newAsBlack=False: 2 White wins, 1 Black win -> New wins
      let game0_t0 = mkTestCompletedGame 0 0 True Black 10
          game0_t1 = mkTestCompletedGame 0 1 True Black 11
          game0_t2 = mkTestCompletedGame 0 2 True White 12
          game0_f0 = mkTestCompletedGame 0 0 False White 10
          game0_f1 = mkTestCompletedGame 0 1 False White 11
          game0_f2 = mkTestCompletedGame 0 2 False Black 12

          -- Pair 1: Old wins both sides
          -- newAsBlack=True: 2 White wins, 1 Black win -> Old wins
          -- newAsBlack=False: 2 Black wins, 1 White win -> Old wins
          game1_t0 = mkTestCompletedGame 1 0 True White 15
          game1_t1 = mkTestCompletedGame 1 1 True White 14
          game1_t2 = mkTestCompletedGame 1 2 True Black 16
          game1_f0 = mkTestCompletedGame 1 0 False Black 14
          game1_f1 = mkTestCompletedGame 1 1 False Black 15
          game1_f2 = mkTestCompletedGame 1 2 False White 13

          -- Pair 2: Tied (new wins one side, old wins other)
          -- newAsBlack=True: 2 Black wins (20, 22 moves), 1 White win -> New wins, avg 21 moves
          -- newAsBlack=False: 2 Black wins (25, 27 moves), 1 White win -> Old wins, avg 26 moves
          -- moveDiff = old_moves - new_moves = 26 - 21 = 5
          game2_t0 = mkTestCompletedGame 2 0 True Black 20
          game2_t1 = mkTestCompletedGame 2 1 True Black 22
          game2_t2 = mkTestCompletedGame 2 2 True White 30
          game2_f0 = mkTestCompletedGame 2 0 False Black 25
          game2_f1 = mkTestCompletedGame 2 1 False Black 27
          game2_f2 = mkTestCompletedGame 2 2 False White 18

          allCompletedGames =
            [ game0_t0, game0_t1, game0_t2, game0_f0, game0_f1, game0_f2
            , game1_t0, game1_t1, game1_t2, game1_f0, game1_f1, game1_f2
            , game2_t0, game2_t1, game2_t2, game2_f0, game2_f1, game2_f2
            ]

      -- Method 1: Build from snapshot (batch)
      let batchScoreState = mkInitialScoreState allCompletedGames

      -- Method 2: Build incrementally from events
      let initialScoreState =
            ScoreState
              { moveDifferenceSum = 0
              , tiedPairCount = 0
              , pending = mempty
              , newPairWins = 0
              , oldPairWins = 0
              }

          mkEvent g = StateUpdate (g.setup.id, g.setup.newAsBlack, g.setup.playIndex) (GameCompleted g.setup g.moves g.result)
          events = map mkEvent allCompletedGames
          incrementalScoreState = foldl' (flip updateScoreState) initialScoreState events

      -- Verify both methods produce the same result
      batchScoreState `shouldBe` incrementalScoreState

      -- Verify the expected values
      batchScoreState.newPairWins `shouldBe` 1 -- Pair 0: new wins both
      batchScoreState.oldPairWins `shouldBe` 1 -- Pair 1: old wins both
      batchScoreState.moveDifferenceSum `shouldBe` 5 -- Only pair 2 is tied: 26 - 21
      batchScoreState.tiedPairCount `shouldBe` 1 -- Only pair 2 is tied
      batchScoreState.pending `shouldBe` mempty -- All games should be processed

    it "should handle pending games correctly" $ do
      -- Create only some games from a set (not all 6)
      let game0_t0 = mkTestCompletedGame 0 0 True Black 10
          game0_t1 = mkTestCompletedGame 0 1 True Black 11

          batchScoreState = mkInitialScoreState [game0_t0, game0_t1]

          initialScoreState =
            ScoreState
              { moveDifferenceSum = 0
              , tiedPairCount = 0
              , pending = mempty
              , newPairWins = 0
              , oldPairWins = 0
              }

          mkEvent g = StateUpdate (g.setup.id, g.setup.newAsBlack, g.setup.playIndex) (GameCompleted g.setup g.moves g.result)
          events = map mkEvent [game0_t0, game0_t1]
          incrementalScoreState = foldl' (flip updateScoreState) initialScoreState events

      -- Both methods should have the pending games
      batchScoreState `shouldBe` incrementalScoreState
      batchScoreState.newPairWins `shouldBe` 0 -- No complete sets yet
      batchScoreState.oldPairWins `shouldBe` 0 -- No complete sets yet
      batchScoreState.tiedPairCount `shouldBe` 0
      length batchScoreState.pending `shouldBe` 1 -- One game ID with pending results
