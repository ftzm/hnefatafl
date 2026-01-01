{-# LANGUAGE ExplicitNamespaces #-}

module Hnefatafl.Interpreter.Search.TestTest where

import Data.List ((!!))
import Effectful (Eff, runEff, type (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (runErrorNoCallStack)
import Hnefatafl.Bindings (SearchTrustedResult (..), startBoard, EngineGameStatus (..))
import Hnefatafl.Core.Data (ExternBoard, Move (..), MoveResult (..))
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Interpreter.Search.Test (TestSearchConfig (..), runSearchTest)
import Hnefatafl.Search (SearchTimeout (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtil (realMoveResults, realGameFinalStatus)

-- | Test game notation from TestUtil
testGameNotation :: Text
testGameNotation = "d11-d9 h6-h3 k7-i7 f8-c8 a4-c4 f4-i4 g1-g2 e5-c5 d1-d3 g5-j5 k4-j4xj5 f5-j5 h11-h4xi4 c5-h5xh4 i7-i5xj5 g7-g9 g2-g5xh5 f7-k7 i5-i9 f6-f8 j6-h6xg6 f8-j8 i9-j9 j8-i8 k6-i6 i8-i11 j9-j11 i11-i10 j11-j10 g9-k9xk8 g11-i11 k9-k10xj10 j4-j10 k10-k9 h6-h10 k9-k10xj10 d9-k9xk10 i10-k10 h10-j10 k10-k11"

-- | Expected game length
gameLength :: Int
gameLength = 40

spec_test_search_interpreter :: Spec
spec_test_search_interpreter = do
  describe "Test Search interpreter" $ do
    it "should replay the complete game until game ends or limit reached" $ do
      let config = TestSearchConfig { gameNotation = testGameNotation }

      result <- runEff $
        runErrorNoCallStack @Text $
          runSearchTest config $
            playCompleteGame startBoard True []

      case result of
        Left err -> error $ "Test failed: " <> err
        Right (moves, finalStatus) -> do
          -- Should reach the final status from TestUtil
          finalStatus `shouldBe` realGameFinalStatus
          -- Should complete the entire game
          length moves `shouldBe` gameLength
          -- Each move should be from the predetermined sequence
          let expectedMoves = map (.move) (toList realMoveResults)
          moves `shouldBe` expectedMoves

    it "should handle first move correctly" $ do
      let config = TestSearchConfig { gameNotation = testGameNotation }

      result <- runEff $
        runErrorNoCallStack @Text $
          runSearchTest config $
            send $ SearchTrusted startBoard True [] (SearchTimeout 1000)

      case result of
        Left err -> error $ "Test failed: " <> err
        Right searchResult -> do
          let expectedFirstMove = (.move) . head $ realMoveResults
          searchMove searchResult `shouldBe` expectedFirstMove
          gameStatus searchResult `shouldBe` EngineOngoing

    it "should handle mid-game position correctly" $ do
      let config = TestSearchConfig { gameNotation = testGameNotation }
          -- Take the zobrist hash from the 5th move to test mid-game lookup
          realMoveResultsList = toList realMoveResults
          fifthMoveResult = realMoveResultsList !! 4 -- 0-indexed, so this is the 5th move
          currentHash = fifthMoveResult.zobristHash
          expectedNextMove = (.move) $ realMoveResultsList !! 5 -- The 6th move

      result <- runEff $
        runErrorNoCallStack @Text $
          runSearchTest config $
            send $ SearchTrusted fifthMoveResult.board (not fifthMoveResult.wasBlackTurn) [currentHash] (SearchTimeout 1000)

      case result of
        Left err -> error $ "Test failed: " <> err
        Right searchResult -> do
          searchMove searchResult `shouldBe` expectedNextMove
          gameStatus searchResult `shouldBe` EngineOngoing

-- | Play a complete game by continuously calling search until game ends or limit reached
playCompleteGame ::
  Search :> es =>
  ExternBoard ->
  Bool ->
  [Word64] ->
  Eff es ([Move], EngineGameStatus)
playCompleteGame board isBlackTurn hashes = go board isBlackTurn hashes [] 0
 where
  go currentBoard currentIsBlackTurn currentHashes moves moveCount
    | moveCount >= gameLength = pure (reverse moves, EngineOngoing) -- Safety limit reached
    | otherwise = do
        searchResult <- send $ SearchTrusted currentBoard currentIsBlackTurn currentHashes (SearchTimeout 1000)
        let newMove = searchMove searchResult
            newBoard = updatedBoard searchResult
            newHash = updatedZobristHash searchResult
            newHashes = newHash : currentHashes
            newIsBlackTurn = not currentIsBlackTurn
            currentGameStatus = gameStatus searchResult

        -- Check if game has ended
        case currentGameStatus of
          EngineOngoing -> go newBoard newIsBlackTurn newHashes (newMove : moves) (moveCount + 1)
          finalStatus -> pure (reverse (newMove : moves), finalStatus)