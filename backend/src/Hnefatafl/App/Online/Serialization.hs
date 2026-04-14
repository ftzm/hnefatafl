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
 where
  opponentMovedMsg am =
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineMoveMade
          { _move = moveFromDomain (MoveWithCaptures am.move am.captures)
          , _side = am.side
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }
  undoMsg n =
    let (turn', status', validMoves', board') = activeStateFields newState
     in OnlineUndoAccepted
          { _moveCount = n
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }

-- | Serialize the full game state for initial sync on connect.
gameStateMessage :: GameId -> PlayerColor -> Online.State -> OnlineServerMessage
gameStateMessage gId playerColor (Online.State board moves phase) =
  OnlineGameState
    { _gameId = gId
    , _playerColor = playerColor
    , _board = boardFromExtern board
    , _history = map (\am -> historyEntryFromDomain (MoveWithCaptures am.move am.captures) am.side) moves
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
      , validMovesMapFromDomain validMoves
      , fmap pendingActionFromDomain pending
      )
    Online.Finished outcome ->
      ( Black
      , gameStatusFromDomain (Just outcome)
      , validMovesMapFromDomain []
      , Nothing
      )

-- | Extract common state fields from an Online state.
activeStateFields :: Online.State -> (PlayerColor, ApiGameStatus, ValidMovesMap, ApiBoard)
activeStateFields (Online.State board _moves phase) =
  case phase of
    Online.Active turn validMoves _pending ->
      ( turn
      , gameStatusFromDomain Nothing
      , validMovesMapFromDomain validMoves
      , boardFromExtern board
      )
    Online.Finished outcome ->
      ( Black
      , gameStatusFromDomain (Just outcome)
      , validMovesMapFromDomain []
      , boardFromExtern board
      )
