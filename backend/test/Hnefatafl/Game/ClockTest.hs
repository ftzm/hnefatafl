module Hnefatafl.Game.ClockTest where

import Chronos (Time (..))
import Hnefatafl.Core.Data (ClockState (..), PlayerColor (..))
import Hnefatafl.Game.Online (updateClock)
import Hnefatafl.Game.TestUtil (mkRemaining, mkTC, remainingSec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_updateClock :: TestTree
test_updateClock =
  testGroup
    "updateClock"
    [ testCase "deducts elapsed time from Black" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
        case updateClock (mkTC 300 0) cs Black (Time 2_000_000_000) of
          Just cs' -> remainingSec cs'.blackRemaining @?= 298
          Nothing -> fail "expected Just"
    , testCase "deducts elapsed time from White" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
        case updateClock (mkTC 300 0) cs White (Time 2_000_000_000) of
          Just cs' -> remainingSec cs'.whiteRemaining @?= 298
          Nothing -> fail "expected Just"
    , testCase "adds increment after deduction" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
        case updateClock (mkTC 300 5) cs Black (Time 2_000_000_000) of
          Just cs' -> remainingSec cs'.blackRemaining @?= 303
          Nothing -> fail "expected Just"
    , testCase "updates turnStartedAt to move time" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
            moveTime = Time 5_000_000_000
        case updateClock (mkTC 300 0) cs White moveTime of
          Just cs' -> cs'.turnStartedAt @?= moveTime
          Nothing -> fail "expected Just"
    , testCase "does not affect opponent clock" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 250) (Time 0)
        case updateClock (mkTC 300 5) cs Black (Time 2_000_000_000) of
          Just cs' -> remainingSec cs'.whiteRemaining @?= 300
          Nothing -> fail "expected Just"
    , testCase "returns Nothing when time expired" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
        updateClock (mkTC 300 0) cs Black (Time 301_000_000_000)
          @?= Nothing
    , testCase "exact remaining succeeds with zero left" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
        case updateClock (mkTC 300 0) cs Black (Time 300_000_000_000) of
          Just cs' -> remainingSec cs'.blackRemaining @?= 0
          Nothing -> fail "expected Just"
    , testCase "zero elapsed produces no change" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
        case updateClock (mkTC 300 0) cs Black (Time 0) of
          Just cs' -> remainingSec cs'.blackRemaining @?= 300
          Nothing -> fail "expected Just"
    , testCase "negative elapsed (clock skew) treated as zero" $ do
        let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 5_000_000_000)
        case updateClock (mkTC 300 0) cs Black (Time 0) of
          Just cs' -> remainingSec cs'.blackRemaining @?= 300
          Nothing -> fail "expected Just"
    ]
