module Hnefatafl.Game.ClockTest where

import Chronos (Time (..))
import Hnefatafl.Bindings (startBlackMoves, startBoard)
import Hnefatafl.Core.Data (
  ClockState (..),
  MoveWithCaptures (..),
  PlayerColor (..),
 )
import Hnefatafl.Game.Online (
  Event (..),
  Phase (..),
  State (..),
  TransitionResult (..),
  transition,
  updateClock,
 )
import Hnefatafl.Game.TestUtil (mkRemaining, mkTC, remainingSec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (State)

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

-- | A timed game just before its first move: Black to move, both
-- clocks full, turnStartedAt unset (epoch).
timedInitial :: State
timedInitial =
  let cs = ClockState (mkRemaining 300) (mkRemaining 300) (Time 0)
   in State
        startBoard
        []
        (Active Black (toList startBlackMoves) Nothing (Just (mkTC 300 0, cs)))

clockOf :: State -> Maybe ClockState
clockOf (State _ _ (Active _ _ _ clk)) = snd <$> clk
clockOf _ = Nothing

test_firstMoveClock :: TestTree
test_firstMoveClock =
  testGroup
    "first move clock"
    [ testCase "first move deducts from neither clock" $
        case toList startBlackMoves of
          (vm : _) ->
            case transition timedInitial (MakeMove Black vm.move (Time 2_000_000_000)) of
              Right (TransitionResult ns _) -> case clockOf ns of
                Just cs' -> do
                  remainingSec cs'.blackRemaining @?= 300
                  remainingSec cs'.whiteRemaining @?= 300
                Nothing -> fail "expected a clock on the resulting state"
              Left e -> fail $ "unexpected transition error: " <> show e
          [] -> fail "no opening moves for Black"
    , testCase "first move sets turnStartedAt to the move time" $
        case toList startBlackMoves of
          (vm : _) ->
            case transition timedInitial (MakeMove Black vm.move (Time 2_000_000_000)) of
              Right (TransitionResult ns _) -> case clockOf ns of
                Just cs' -> cs'.turnStartedAt @?= Time 2_000_000_000
                Nothing -> fail "expected a clock on the resulting state"
              Left e -> fail $ "unexpected transition error: " <> show e
          [] -> fail "no opening moves for Black"
    , testCase "second move deducts elapsed from the mover" $
        case toList startBlackMoves of
          (bvm : _) ->
            -- Black opens at t=1s (free); White replies at t=3s, so 2s
            -- is deducted from White's clock.
            case transition timedInitial (MakeMove Black bvm.move (Time 1_000_000_000)) of
              Right (TransitionResult afterBlack _) -> case afterBlack of
                State _ _ (Active White (wvm : _) _ _) ->
                  case transition afterBlack (MakeMove White wvm.move (Time 3_000_000_000)) of
                    Right (TransitionResult afterWhite _) -> case clockOf afterWhite of
                      Just cs' -> do
                        remainingSec cs'.whiteRemaining @?= 298
                        remainingSec cs'.blackRemaining @?= 300
                      Nothing -> fail "expected a clock on the resulting state"
                    Left e -> fail $ "White move failed: " <> show e
                _ -> fail "expected White to move with legal moves"
              Left e -> fail $ "Black move failed: " <> show e
          [] -> fail "no opening moves for Black"
    ]
