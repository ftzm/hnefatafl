module Hnefatafl.App.Online.Serialization (
  notificationsFor,
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
  opponent,
 )
import Hnefatafl.Game.Common (AppliedMove (..), DomainEvent (..))
import Hnefatafl.Game.Online qualified as Online

-- | Derive notifications from domain events. Returns (target, message) pairs.
notificationsFor ::
  PlayerColor -> Online.State -> [DomainEvent] -> [(PlayerColor, OnlineServerMessage)]
notificationsFor actor newState = concatMap $ \case
  MovePlayed am ->
    [(opponent actor, opponentMovedMsg am)]
  GameEnded outcome ->
    let msg = OnlineGameOver{_status = gameStatusFromDomain (Just outcome)}
     in [(opponent actor, msg), (actor, msg)]
  MovesUndone _ ->
    let msg = undoMsg
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
 where
  opponentMovedMsg am =
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineOpponentMoved
          { _move = moveFromDomain (MoveWithCaptures am.move am.captures)
          , _side = am.side
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }
  undoMsg =
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
      ( Black
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
