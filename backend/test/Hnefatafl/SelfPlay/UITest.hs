module Hnefatafl.SelfPlay.UITest where

import Hnefatafl.Bindings (startBoard)
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameDefinition (..),
  GameName (..),
  GameResult (..),
  Player (..),
  StateUpdate (..),
 )
import Hnefatafl.SelfPlay.UI (ScoreState (..), mkInitialScoreState, updateScoreState)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Utility to create test completed games
mkTestCompletedGame :: Int -> Bool -> Player -> Int -> CompletedGame
mkTestCompletedGame gameId newAsBlack winner moves =
  let gameName = GameName $ show gameId <> " " <> if newAsBlack then "New vs. Old" else "Old vs. New"
   in CompletedGame
        { gameDefinition =
            GameDefinition
              { id = gameId
              , name = gameName
              , notation = "test"
              , board = startBoard
              , blackToMove = True
              , newAsBlack = newAsBlack
              , hashes = []
              , moveCount = 0
              }
        , result = GameResult{winner = winner, moves = moves}
        }

-- | Test that building ScoreState from completed games matches incremental updates
spec_score_state_batch_vs_incremental :: Spec
spec_score_state_batch_vs_incremental =
  describe "ScoreState batch vs incremental" $ do
    it "should produce same result when built from snapshot vs incremental updates" $ do
      -- Create test game pairs with known results
      let -- Pair 0: New wins both games (winBalance +2)
          game0_new = mkTestCompletedGame 0 True Black 10 -- New as Black wins
          game0_old = mkTestCompletedGame 0 False White 12 -- New as White wins

          -- Pair 1: Old wins both games (winBalance -2)
          game1_new = mkTestCompletedGame 1 True White 15 -- Old as White wins
          game1_old = mkTestCompletedGame 1 False Black 14 -- Old as Black wins

          -- Pair 2: Tied (each wins once), test move balance
          -- New wins as Black in 20 moves, Old wins as Black in 25 moves
          -- moveDiff = old_moves - new_moves = 25 - 20 = 5
          game2_new = mkTestCompletedGame 2 True Black 20 -- New as Black wins
          game2_old = mkTestCompletedGame 2 False Black 25 -- Old as Black wins

          allCompletedGames = [game0_new, game0_old, game1_new, game1_old, game2_new, game2_old]

      -- Method 1: Build from snapshot (batch)
      let batchScoreState = mkInitialScoreState allCompletedGames

      -- Method 2: Build incrementally from events
      let initialScoreState =
            ScoreState
              { moveDifferenceSum = 0
              , tiedPairCount = 0
              , unpaired = mempty
              , newPairWins = 0
              , oldPairWins = 0
              }

          -- Create GameCompleted events
          events =
            [ GameCompleted (GameName "0 New vs. Old") game0_new.gameDefinition game0_new.result
            , GameCompleted (GameName "0 Old vs. New") game0_old.gameDefinition game0_old.result
            , GameCompleted (GameName "1 New vs. Old") game1_new.gameDefinition game1_new.result
            , GameCompleted (GameName "1 Old vs. New") game1_old.gameDefinition game1_old.result
            , GameCompleted (GameName "2 New vs. Old") game2_new.gameDefinition game2_new.result
            , GameCompleted (GameName "2 Old vs. New") game2_old.gameDefinition game2_old.result
            ]

          incrementalScoreState = foldl' (flip updateScoreState) initialScoreState events

      -- Verify both methods produce the same result
      batchScoreState `shouldBe` incrementalScoreState

      -- Verify the expected values
      batchScoreState.newPairWins `shouldBe` 1 -- Pair 0: new wins both
      batchScoreState.oldPairWins `shouldBe` 1 -- Pair 1: old wins both
      batchScoreState.moveDifferenceSum `shouldBe` 5 -- Only pair 2 is tied: 25 - 20
      batchScoreState.tiedPairCount `shouldBe` 1 -- Only pair 2 is tied
      batchScoreState.unpaired `shouldBe` mempty -- All games should be paired

    it "should handle unpaired games correctly" $ do
      -- Create only one game from a pair
      let game0_new = mkTestCompletedGame 0 True Black 10

          batchScoreState = mkInitialScoreState [game0_new]

          initialScoreState =
            ScoreState
              { moveDifferenceSum = 0
              , tiedPairCount = 0
              , unpaired = mempty
              , newPairWins = 0
              , oldPairWins = 0
              }

          event = GameCompleted (GameName "0 New vs. Old") game0_new.gameDefinition game0_new.result
          incrementalScoreState = updateScoreState event initialScoreState

      -- Both methods should have the unpaired game
      batchScoreState `shouldBe` incrementalScoreState
      batchScoreState.newPairWins `shouldBe` 0 -- No complete pairs yet
      batchScoreState.oldPairWins `shouldBe` 0 -- No complete pairs yet
      batchScoreState.tiedPairCount `shouldBe` 0
      length batchScoreState.unpaired `shouldBe` 1
