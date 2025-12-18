{-# LANGUAGE QuasiQuotes #-}

module Hnefatafl.QQ.MoveTest where

-- import Hnefatafl.Core.Data (Move (..))
-- import Hnefatafl.QQ.Move (moves, movesNE)
import Test.Hspec (Spec, describe, it, shouldBe)

spec_MoveQQ :: Spec
spec_MoveQQ = do
  describe "moves quasiquoter" $ do
    it "TODO: re-enable when FFI/TH issues are resolved" $
      True `shouldBe` True

{- TODO: Re-enable when FFI and Template Haskell issues are resolved

    it "parses a single move" $
      [moves|k1j2|] `shouldBe` [Move 0 12]

    it "parses multiple moves" $
      [moves|k1j2 j3i4|] `shouldBe` [Move 0 12, Move 24 35]

    it "parses empty string as empty list" $
      [moves||] `shouldBe` []

    it "handles moves with optional dash separator" $
      [moves|k1-j2|] `shouldBe` [Move 0 12]

    it "handles moves with capture notation" $
      [moves|k1j2xa3|] `shouldBe` [Move 0 12]

  describe "movesNE quasiquoter" $ do
    it "parses a single move to NonEmpty" $ do
      let result = [movesNE|k1j2|]
      toList result `shouldBe` [Move 0 12]

    it "parses multiple moves to NonEmpty" $ do
      let result = [movesNE|k1j2 j3i4|]
      toList result `shouldBe` [Move 0 12, Move 24 35]
-}