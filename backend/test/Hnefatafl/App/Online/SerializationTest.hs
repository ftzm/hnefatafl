module Hnefatafl.App.Online.SerializationTest where

import Chronos (Time (..), Timespan (..))
import Hnefatafl.Api.Types.WS.Online (ClockMs (..), OnlineServerMessage (..))
import Hnefatafl.App.Online.Serialization (
  gameStateMessage,
  notificationsFor,
  remainingToMs,
 )
import Hnefatafl.Bindings (startBlackMoves, startBoard)
import Hnefatafl.Core.Data (
  BlackWinCondition (..),
  ClockState (..),
  GameId (..),
  Outcome (..),
  PlayerColor (..),
  WhiteWinCondition (..),
  mkRemainingTime,
 )
import Hnefatafl.Game.Common (
  DomainEvent (..),
 )
import Hnefatafl.Game.Online qualified as Online
import Hnefatafl.Game.TestUtil (dummyMove, mkRemaining, mkTC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (State)

testGameId :: GameId
testGameId = GameId "test-game"

timedClock :: ClockState
timedClock =
  ClockState
    (mkRemaining 300)
    (mkRemaining 250)
    (Time 0)

timedState :: Online.State
timedState =
  Online.State
    startBoard
    []
    ( Online.Active
        Black
        (toList startBlackMoves)
        Nothing
        (Just (mkTC 300 5, timedClock))
    )

untimedState :: Online.State
untimedState =
  Online.State
    startBoard
    []
    (Online.Active Black (toList startBlackMoves) Nothing Nothing)

finishedState :: Online.State
finishedState =
  Online.State
    startBoard
    []
    (Online.Finished (WhiteWins KingEscaped))

test_remainingToMs :: TestTree
test_remainingToMs =
  testGroup
    "remainingToMs"
    [ testCase "converts seconds to milliseconds" $
        remainingToMs (mkRemaining 300) @?= 300_000
    , testCase "zero remaining maps to zero" $
        remainingToMs (mkRemaining 0) @?= 0
    , testCase "sub-second precision preserved" $
        case mkRemainingTime (Timespan 1_500_000_000) of
          Just rt -> remainingToMs rt @?= 1_500
          Nothing -> fail "mkRemainingTime failed"
    ]

test_gameStateMessage :: TestTree
test_gameStateMessage =
  testGroup
    "gameStateMessage clock fields"
    [ testCase "timed game has Just clock" $
        case gameStateMessage testGameId Black timedState of
          OnlineGameState{_clock = c} ->
            c @?= Just (ClockMs 300_000 250_000)
          other -> fail $ "Expected OnlineGameState, got " <> show other
    , testCase "untimed game has Nothing clock" $
        case gameStateMessage testGameId Black untimedState of
          OnlineGameState{_clock = c} ->
            c @?= Nothing
          other -> fail $ "Expected OnlineGameState, got " <> show other
    , testCase "finished game has Nothing clock" $
        case gameStateMessage testGameId Black finishedState of
          OnlineGameState{_clock = c} ->
            c @?= Nothing
          other -> fail $ "Expected OnlineGameState, got " <> show other
    ]

test_notificationsForClock :: TestTree
test_notificationsForClock =
  testGroup
    "notificationsFor clock fields"
    [ testCase "MovePlayed on timed game includes clock" $ do
        let am = dummyMove Black
            notifications = notificationsFor Black timedState [MovePlayed am]
        case notifications of
          [(_, OnlineMoveMade{_clock = c})] ->
            c @?= Just (ClockMs 300_000 250_000)
          other -> fail $ "Expected one OnlineMoveMade, got " <> show other
    , testCase "MovePlayed on untimed game has Nothing clock" $ do
        let am = dummyMove Black
            notifications = notificationsFor Black untimedState [MovePlayed am]
        case notifications of
          [(_, OnlineMoveMade{_clock = c})] ->
            c @?= Nothing
          other -> fail $ "Expected one OnlineMoveMade, got " <> show other
    , testCase "GameEnded on timed game includes clock" $ do
        let notifications =
              notificationsFor Black timedState [GameEnded (BlackWins KingCaptured)]
        case notifications of
          (_, OnlineGameOver{_clock = c}) : _ ->
            c @?= Just (ClockMs 300_000 250_000)
          other -> fail $ "Expected OnlineGameOver, got " <> show other
    , testCase "GameEnded on untimed game has Nothing clock" $ do
        let notifications =
              notificationsFor Black untimedState [GameEnded (BlackWins KingCaptured)]
        case notifications of
          (_, OnlineGameOver{_clock = c}) : _ ->
            c @?= Nothing
          other -> fail $ "Expected OnlineGameOver, got " <> show other
    , testCase "MovesUndone on timed game includes clock" $ do
        let notifications = notificationsFor Black timedState [MovesUndone 1]
        case notifications of
          (_, OnlineUndoAccepted{_clock = c}) : _ ->
            c @?= Just (ClockMs 300_000 250_000)
          other -> fail $ "Expected OnlineUndoAccepted, got " <> show other
    , testCase "MovesUndone on untimed game has Nothing clock" $ do
        let notifications = notificationsFor Black untimedState [MovesUndone 1]
        case notifications of
          (_, OnlineUndoAccepted{_clock = c}) : _ ->
            c @?= Nothing
          other -> fail $ "Expected OnlineUndoAccepted, got " <> show other
    , testCase "ClockUpdated sends OnlineClockUpdated to actor" $ do
        let notifications = notificationsFor Black timedState [ClockUpdated timedClock]
        case notifications of
          [(target, OnlineClockUpdated{_whiteMs = whiteMs, _blackMs = blackMs})] -> do
            target @?= Black
            whiteMs @?= 300_000
            blackMs @?= 250_000
          other -> fail $ "Expected one OnlineClockUpdated, got " <> show other
    ]