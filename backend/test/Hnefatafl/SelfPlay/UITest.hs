module Hnefatafl.SelfPlay.UITest where

import Hnefatafl.Bindings (startBoard)
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameResult (..),
  GameSetup (..),
  Outcome (..),
  Player (..),
  StateUpdate (..),
  StateUpdatePayload (..),
 )
import Hnefatafl.SelfPlay.UI (ScoreState (..), mkInitialScoreState, updateScoreState)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Utility to create test completed games
mkTestCompletedGame :: Int -> Int -> Bool -> Outcome -> Int -> CompletedGame
mkTestCompletedGame gameId playIdx newAsBlack outcome moves =
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
    , result = GameResult{outcome = outcome, moves = moves}
    }

-- | Test that building ScoreState from completed games matches incremental updates
spec_score_state_batch_vs_incremental :: Spec
spec_score_state_batch_vs_incremental =
  describe "ScoreState batch vs incremental" $ do
    it "should produce same result when built from snapshot vs incremental updates" $ do
      -- Create test game sets with known results (6 games per ID: 3 per side)
      -- Position 0: Mixed results
      -- newAsBlack=True: Black wins (new wins), Black wins (new wins), White wins (new loses)
      -- newAsBlack=False: White wins (new wins), White wins (new wins), Black wins (new loses)
      -- Net: 4 new wins, 0 draws, 2 new losses
      let game0_t0 = mkTestCompletedGame 0 0 True (WonBy Black) 10  -- new wins
          game0_t1 = mkTestCompletedGame 0 1 True (WonBy Black) 11  -- new wins
          game0_t2 = mkTestCompletedGame 0 2 True (WonBy White) 12  -- new loses
          game0_f0 = mkTestCompletedGame 0 0 False (WonBy White) 10 -- new wins
          game0_f1 = mkTestCompletedGame 0 1 False (WonBy White) 11 -- new wins
          game0_f2 = mkTestCompletedGame 0 2 False (WonBy Black) 12 -- new loses

          -- Position 1: New engine loses more
          -- newAsBlack=True: White wins (new loses), White wins (new loses), Black wins (new wins)
          -- newAsBlack=False: Black wins (new loses), Black wins (new loses), White wins (new wins)
          -- Net: 2 new wins, 0 draws, 4 new losses
          game1_t0 = mkTestCompletedGame 1 0 True (WonBy White) 15  -- new loses
          game1_t1 = mkTestCompletedGame 1 1 True (WonBy White) 14  -- new loses
          game1_t2 = mkTestCompletedGame 1 2 True (WonBy Black) 16  -- new wins
          game1_f0 = mkTestCompletedGame 1 0 False (WonBy Black) 14 -- new loses
          game1_f1 = mkTestCompletedGame 1 1 False (WonBy Black) 15 -- new loses
          game1_f2 = mkTestCompletedGame 1 2 False (WonBy White) 13 -- new wins

          -- Position 2: Some draws
          -- newAsBlack=True: Black wins (new wins), Draw, White wins (new loses)
          -- newAsBlack=False: White wins (new wins), Draw, Black wins (new loses)
          -- Net: 2 new wins, 2 draws, 2 new losses
          game2_t0 = mkTestCompletedGame 2 0 True (WonBy Black) 20  -- new wins
          game2_t1 = mkTestCompletedGame 2 1 True Draw 22           -- draw
          game2_t2 = mkTestCompletedGame 2 2 True (WonBy White) 30  -- new loses
          game2_f0 = mkTestCompletedGame 2 0 False (WonBy White) 25 -- new wins
          game2_f1 = mkTestCompletedGame 2 1 False Draw 27          -- draw
          game2_f2 = mkTestCompletedGame 2 2 False (WonBy Black) 18 -- new loses

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
              { wins = 0
              , draws = 0
              , losses = 0
              , pending = mempty
              }

          mkEvent g = StateUpdate (g.setup.id, g.setup.newAsBlack, g.setup.playIndex) (GameCompleted g.setup g.moves g.result)
          events = map mkEvent allCompletedGames
          incrementalScoreState = foldl' (flip updateScoreState) initialScoreState events

      -- Verify both methods produce the same result
      batchScoreState `shouldBe` incrementalScoreState

      -- Verify the expected values
      -- Total: 4+2+2 = 8 wins, 0+0+2 = 2 draws, 2+4+2 = 8 losses
      batchScoreState.wins `shouldBe` 8
      batchScoreState.draws `shouldBe` 2
      batchScoreState.losses `shouldBe` 8
      batchScoreState.pending `shouldBe` mempty -- All games should be processed

    it "should handle pending games correctly" $ do
      -- Create only some games from a set (not all 6)
      let game0_t0 = mkTestCompletedGame 0 0 True (WonBy Black) 10
          game0_t1 = mkTestCompletedGame 0 1 True (WonBy Black) 11

          batchScoreState = mkInitialScoreState [game0_t0, game0_t1]

          initialScoreState =
            ScoreState
              { wins = 0
              , draws = 0
              , losses = 0
              , pending = mempty
              }

          mkEvent g = StateUpdate (g.setup.id, g.setup.newAsBlack, g.setup.playIndex) (GameCompleted g.setup g.moves g.result)
          events = map mkEvent [game0_t0, game0_t1]
          incrementalScoreState = foldl' (flip updateScoreState) initialScoreState events

      -- Both methods should have the pending games
      batchScoreState `shouldBe` incrementalScoreState
      batchScoreState.wins `shouldBe` 0 -- No complete positions yet
      batchScoreState.draws `shouldBe` 0
      batchScoreState.losses `shouldBe` 0
      length batchScoreState.pending `shouldBe` 1 -- One position ID with pending results
