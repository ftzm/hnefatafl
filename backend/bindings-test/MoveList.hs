module MoveList where

import Hnefatafl.Bindings
import Hnefatafl.Core.Data (Move (..))
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
   in it "should be equal abcdayzzdun" $ do
        expectedString `shouldBe` actualString
        recalculatedMoveList `shouldBe` moveList

spec_startBlackMoves :: Spec
spec_startBlackMoves =
  it "should contain 116 moves" $ do
    length startBlackMoves `shouldBe` 116

spec_nextGameState :: Spec
spec_nextGameState =
  it "should return moves for white after first black move" $ do
    let firstBlackMove = head startBlackMoves
        (status, possibleMoves) = nextGameState (firstBlackMove :| [])
    status `shouldBe` EngineOngoing
    length possibleMoves `shouldSatisfy` (> 0)
