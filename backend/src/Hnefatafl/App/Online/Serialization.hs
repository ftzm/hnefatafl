module Hnefatafl.App.Online.Serialization (
  notificationMessage,
  actorNotificationMessage,
  gameStateMessage,
) where

import Hnefatafl.Api.Types (
  ApiBoard,
  ApiGameStatus,
  ApiMove,
  boardFromExtern,
  gameStatusFromDomain,
  moveFromDomain,
 )
import Hnefatafl.Api.Types.WS (
  appliedMoveFromDomain,
  pendingActionFromDomain,
 )
import Hnefatafl.Api.Types.WS.Online (OnlineServerMessage (..))
import Hnefatafl.Core.Data (
  GameId,
  MoveWithCaptures (..),
  PlayerColor (..),
 )
import Hnefatafl.Core.Data qualified as Core
import Hnefatafl.Game.Common (AppliedMove (..))
import Hnefatafl.Game.Online qualified as Online

-- | Serialize a notification for the opponent.
notificationMessage :: Online.State -> Online.Notification -> OnlineServerMessage
notificationMessage newState = \case
  Online.OpponentMoved am ->
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineOpponentMoved
          { _move = moveFromDomain (MoveWithCaptures am.move am.captures)
          , _side = am.side
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }
  Online.OpponentResigned color ->
    OnlineGameOver{_status = gameStatusFromDomain (Just (Core.ResignedBy color))}
  Online.OpponentTimedOut color ->
    OnlineGameOver{_status = gameStatusFromDomain (Just (Core.TimedOut color))}
  Online.DrawOffered color ->
    OnlineDrawOffered{_by = color}
  Online.DrawAccepted ->
    OnlineGameOver{_status = gameStatusFromDomain (Just Core.Draw)}
  Online.DrawDeclined ->
    OnlineDrawDeclined
  Online.UndoRequested color ->
    OnlineUndoRequested{_by = color}
  Online.UndoAccepted ->
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineUndoAccepted
          { _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }
  Online.UndoDeclined ->
    OnlineUndoDeclined

-- | Serialize an actor notification.
actorNotificationMessage :: Online.State -> Online.ActorNotification -> OnlineServerMessage
actorNotificationMessage newState = \case
  Online.GameEnded outcome ->
    OnlineGameOver{_status = gameStatusFromDomain (Just outcome)}
  Online.UndoApplied ->
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineUndoAccepted
          { _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }

-- | Serialize the full game state for initial sync on connect.
gameStateMessage :: GameId -> Online.State -> OnlineServerMessage
gameStateMessage gId (Online.State board moves phase) =
  OnlineGameState
    { _gameId = gId
    , _board = boardFromExtern board
    , _history = map appliedMoveFromDomain moves
    , _turn = turn'
    , _status = status'
    , _validMoves = validMoves'
    , _pendingAction = pending'
    }
 where
  (turn', status', validMoves', pending') = case phase of
    Online.Active turn validMoves pending ->
      ( turn
      , gameStatusFromDomain Nothing
      , map moveFromDomain validMoves
      , fmap pendingActionFromDomain pending
      )
    Online.Finished outcome ->
      ( -- Turn is irrelevant when finished; use Black as default
        Black
      , gameStatusFromDomain (Just outcome)
      , []
      , Nothing
      )

-- | Extract common state fields from an Online state.
activeStateFields :: Online.State -> (PlayerColor, ApiGameStatus, [ApiMove], ApiBoard)
activeStateFields (Online.State board _moves phase) =
  case phase of
    Online.Active turn validMoves _pending ->
      ( turn
      , gameStatusFromDomain Nothing
      , map moveFromDomain validMoves
      , boardFromExtern board
      )
    Online.Finished outcome ->
      ( Black
      , gameStatusFromDomain (Just outcome)
      , []
      , boardFromExtern board
      )
