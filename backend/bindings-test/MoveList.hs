module MoveList where

import Hnefatafl.Bindings
import Test.Hspec (Spec, it)
import Test.Hspec.Expectations.Pretty

spec_basicMoveListString :: Spec
spec_basicMoveListString =
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
   in it "should be equal z" $ do
        expectedString `shouldBe` actualString
        recalculatedMoveList `shouldBe` moveList

spec_startBlackMoves :: Spec
spec_startBlackMoves =
  it "should contain 116 moves" $ do
    length startBlackMoves `shouldBe` 116

spec_getPossibleMoves :: Spec
spec_getPossibleMoves =
  it "should return moves for white after first black move" $ do
    let firstBlackMove = head startBlackMoves
        possibleMoves = getPossibleMoves (firstBlackMove :| [])
    length possibleMoves `shouldSatisfy` (> 0)
