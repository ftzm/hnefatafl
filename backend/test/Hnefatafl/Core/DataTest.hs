{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hnefatafl.Core.DataTest where

import Data.Aeson (eitherDecode, encode)
import Hnefatafl.Core.Data (Seconds (..), TimeControl (..))
import Refined (unrefine)
import Refined.Unsafe (reallyUnsafeRefine)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec_timeControlParsing :: Spec
spec_timeControlParsing =
  describe "TimeControl JSON parsing" $ do
    it "parses valid time control" $ do
      let json = "{\"initialTime\": 300, \"increment\": 5}"
          result = eitherDecode json :: Either String TimeControl
      result `shouldSatisfy` isRight
      let Right tc = result
      unrefine tc.initialTime `shouldBe` Seconds 300
      unrefine tc.increment `shouldBe` Seconds 5

    it "accepts zero increment" $ do
      let json = "{\"initialTime\": 300, \"increment\": 0}"
          result = eitherDecode json :: Either String TimeControl
      result `shouldSatisfy` isRight
      let Right tc = result
      unrefine tc.increment `shouldBe` Seconds 0

    it "rejects negative initialTime" $ do
      let json = "{\"initialTime\": -1, \"increment\": 5}"
          result = eitherDecode json :: Either String TimeControl
      result `shouldSatisfy` isLeft

    it "rejects zero initialTime" $ do
      let json = "{\"initialTime\": 0, \"increment\": 5}"
          result = eitherDecode json :: Either String TimeControl
      result `shouldSatisfy` isLeft

    it "rejects negative increment" $ do
      let json = "{\"initialTime\": 300, \"increment\": -1}"
          result = eitherDecode json :: Either String TimeControl
      result `shouldSatisfy` isLeft

    it "round-trips through JSON" $ do
      let tc =
            TimeControl
              (reallyUnsafeRefine (Seconds 600))
              (reallyUnsafeRefine (Seconds 10))
          json = encode tc
          result = eitherDecode json :: Either String TimeControl
      result `shouldBe` Right tc
