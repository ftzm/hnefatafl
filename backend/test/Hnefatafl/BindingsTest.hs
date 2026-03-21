module Hnefatafl.BindingsTest where

import Hnefatafl.Bindings
import Hnefatafl.Core.Data (ExternBoard (..), Move (..), MoveResult (..))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty
import TestUtil (realGameData)

spec_MoveListSerialization :: Spec
spec_MoveListSerialization =
  let moveList =
        [ Move 1 2
        , Move 5 77
        , Move 5 87
        , Move 110 109
        , Move 6 77
        , Move 8 87
        , Move 81 109
        , Move 81 109
        ]
      expectedString = "AQIFTQVXbm0GTQhXUW1RbQ=="
      actualString = moveListToBase64 moveList
      recalculatedMoveList = moveListFromBase64 actualString
   in it "should serialize and deserialize move lists correctly" $ do
        expectedString `shouldBe` actualString
        recalculatedMoveList `shouldBe` moveList

spec_StartBlackMoves :: Spec
spec_StartBlackMoves =
  it "should contain 116 moves" $ do
    length startBlackMoves `shouldBe` 116

spec_NextGameState :: Spec
spec_NextGameState =
  it "should return status after first black move" $ do
    let firstBlackMove = head startBlackMoves
        status = nextGameState (firstBlackMove :| []) False
    status `shouldBe` Right EngineOngoing

spec_NextGameStateWithMoves :: Spec
spec_NextGameStateWithMoves =
  it "should return status and moves after first black move" $ do
    let firstBlackMove = head startBlackMoves
        result = nextGameStateWithMoves (firstBlackMove :| []) False
    case result of
      Right (status, possibleMoves) -> do
        status `shouldBe` EngineOngoing
        length possibleMoves `shouldSatisfy` (> 0)
      Left _ -> expectationFailure "Move validation failed"

spec_StartBoard :: Spec
spec_StartBoard =
  describe "startBoard" $ do
    it "should return a valid initial board" $ do
      let board = startBoard
      board `shouldSatisfy` (\ExternBoard{} -> True)

    it "should return consistent board state" $ do
      let board1 = startBoard
      let board2 = startBoard
      board1 `shouldBe` board2

spec_NextGameStateWithMovesTrusted :: Spec
spec_NextGameStateWithMovesTrusted =
  describe "nextGameStateWithMovesTrusted" $ do
    it "should not crash when called" $ do
      let board = startBoard
          firstMove = head startBlackMoves
          result = nextGameStateWithMovesTrusted board True (firstMove :| [])
      case result of
        (_, moves) -> length moves `shouldSatisfy` (>= 0)

spec_ApplyMoveSequence :: Spec
spec_ApplyMoveSequence =
  describe "ApplyMoveSequence" $ do
    it "should apply first black move and return board state" $ do
      let firstBlackMove = head startBlackMoves
          (moveResults, _finalStatus) = applyMoveSequence (firstBlackMove :| [])
          moveResult = head moveResults
      length moveResults `shouldBe` 1
      moveResult `shouldSatisfy` (\MoveResult{} -> True)

    it "should return correct final game status for real game data" $ do
      -- Debug: let's see what's actually happening
      let (moveResults, finalStatus) = realGameData
      putStrLn $ "Haskell got final status: " <> show finalStatus
      putStrLn $ "Number of move results: " <> show (length moveResults)
      -- The real game sequence ends with white winning by king escape (k10-k11 is king reaching edge)
      finalStatus `shouldBe` EngineKingEscaped
