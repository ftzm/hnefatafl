module Hnefatafl.App.Online.Serialization (
  notificationsFor,
  gameStateMessage,
) where

import Hnefatafl.Api.Types (
  ApiBoard,
  ApiGameStatus,
  ValidMovesMap,
  boardFromExtern,
  gameStatusFromDomain,
  historyEntryFromDomain,
  moveFromDomain,
  validMovesMapFromDomain,
 )
import Hnefatafl.Api.Types.WS (
  pendingActionFromDomain,
 )
import Hnefatafl.Api.Types.WS.Online (ClockMs (..), OnlineServerMessage (..))
import Hnefatafl.Core.Data (
  ClockState (..),
  GameId,
  MoveWithCaptures (..),
  PlayerColor (..),
  opponent,
  remainingToMs,
  timeToMs,
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  DomainEvent (..),
  PendingActionType (..),
 )
import Hnefatafl.Game.Online qualified as Online

-- | Derive notifications from domain events. Returns (target, message) pairs.
notificationsFor ::
  PlayerColor ->
  Online.State ->
  [DomainEvent] ->
  [(PlayerColor, OnlineServerMessage)]
notificationsFor actor newState = concatMap $ \case
  MovePlayed am ->
    [(opponent actor, opponentMovedMsg am)]
  GameEnded outcome ->
    let msg =
          OnlineGameOver
            { _status = gameStatusFromDomain (Just outcome)
            , _clock = clockMs
            }
     in [(opponent actor, msg), (actor, msg)]
  MovesUndone n ->
    let msg = undoMsg n
     in [(opponent actor, msg), (actor, msg)]
  DrawOffered color ->
    [(opponent actor, OnlineDrawOffered{_by = color})]
  DrawDeclined ->
    [(opponent actor, OnlineDrawDeclined)]
  UndoRequested color ->
    [(opponent actor, OnlineUndoRequested{_by = color})]
  UndoDeclined ->
    [(opponent actor, OnlineUndoDeclined)]
  OfferCancelled ->
    []
  OfferAutoCancelled DrawOffer offerer ->
    [(offerer, OnlineDrawCancelled)]
  OfferAutoCancelled UndoRequest offerer ->
    [(offerer, OnlineUndoCancelled)]
  ClockUpdated cs ->
    [
      ( actor
      , OnlineClockUpdated
          { _whiteMs = remainingToMs cs.whiteRemaining
          , _blackMs = remainingToMs cs.blackRemaining
          , _turnStartedAtMs = timeToMs cs.turnStartedAt
          }
      )
    ]
 where
  clockMs = clockMsFields newState
  opponentMovedMsg am =
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineMoveMade
          { _move = moveFromDomain (MoveWithCaptures am.move am.captures)
          , _side = am.side
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          , _clock = clockMs
          }
  undoMsg n =
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineUndoAccepted
          { _moveCount = n
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          , _clock = clockMs
          }

-- | Serialize the full game state for initial sync on connect.
gameStateMessage :: GameId -> PlayerColor -> Online.State -> OnlineServerMessage
gameStateMessage gId playerColor s@(Online.State board moves phase) =
  OnlineGameState
    { _gameId = gId
    , _playerColor = playerColor
    , _board = boardFromExtern board
    , _history =
        map
          (\am -> historyEntryFromDomain (MoveWithCaptures am.move am.captures) am.side)
          moves
    , _turn = turn'
    , _status = status'
    , _validMoves = validMoves'
    , _pendingAction = pending'
    , _clock = clockMs
    }
 where
  clockMs = clockMsFields s
  (turn', status', validMoves', pending') = case phase of
    Online.Active{turn, validMoves, pending} ->
      ( turn
      , gameStatusFromDomain Nothing
      , validMovesMapFromDomain validMoves
      , fmap pendingActionFromDomain pending
      )
    Online.Finished outcome ->
      ( Black -- Turn is meaningless post-game; placeholder value.
      , gameStatusFromDomain (Just outcome)
      , validMovesMapFromDomain []
      , Nothing
      )

-- | Extract common state fields from an Online state.
activeStateFields ::
  Online.State -> (PlayerColor, ApiGameStatus, ValidMovesMap, ApiBoard)
activeStateFields (Online.State board _moves phase) =
  case phase of
    Online.Active{turn, validMoves} ->
      ( turn
      , gameStatusFromDomain Nothing
      , validMovesMapFromDomain validMoves
      , boardFromExtern board
      )
    Online.Finished outcome ->
      ( Black -- Turn is meaningless post-game; placeholder value.
      , gameStatusFromDomain (Just outcome)
      , validMovesMapFromDomain []
      , boardFromExtern board
      )

-- | Extract clock millisecond values from game state.
clockMsFields :: Online.State -> Maybe ClockMs
clockMsFields (Online.State _ _ phase) = case phase of
  Online.Active{clock = Just (_, cs)} ->
    Just $
      ClockMs
        (remainingToMs cs.whiteRemaining)
        (remainingToMs cs.blackRemaining)
        (timeToMs cs.turnStartedAt)
  _ -> Nothing
