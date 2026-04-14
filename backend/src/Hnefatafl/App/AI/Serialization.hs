module Hnefatafl.App.AI.Serialization (
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
import Hnefatafl.Api.Types.WS.AI (AIServerMessage (..))
import Hnefatafl.Core.Data (
  GameId,
  MoveWithCaptures (..),
  PlayerColor (..),
  opponent,
 )
import Hnefatafl.Game.AI qualified as AI
import Hnefatafl.Game.Common (AppliedMove (..), DomainEvent (..))

-- | Derive notifications for the human player from domain events.
-- The human only needs to hear about things they didn't initiate.
notificationsFor ::
  PlayerColor -> AI.State -> [DomainEvent] -> [AIServerMessage]
notificationsFor humanColor newState = concatMap $ \case
  MovePlayed am
    | am.side /= humanColor -> [engineMovedMsg am]
    | otherwise -> []
  GameEnded outcome ->
    [AIGameOver{_status = gameStatusFromDomain (Just outcome)}]
  MovesUndone n -> [undoMsg n]
  DrawOffered _ -> []
  DrawDeclined -> []
  UndoRequested _ -> []
  UndoDeclined -> []
  OfferCancelled -> []
 where
  engineMovedMsg am =
    let (turn', status', validMoves', board') = activeStateFields humanColor newState
     in AIMoveMade
          { _move = moveFromDomain (MoveWithCaptures am.move am.captures)
          , _side = am.side
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }
  undoMsg n =
    let (turn', status', validMoves', board') = activeStateFields humanColor newState
     in AIUndoAccepted
          { _moveCount = n
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }

-- | Serialize the full game state for initial sync on connect.
gameStateMessage :: GameId -> PlayerColor -> AI.State -> AIServerMessage
gameStateMessage gId humanColor (AI.State board moves phase) =
  AIGameState
    { _gameId = gId
    , _playerColor = humanColor
    , _board = boardFromExtern board
    , _history = map (\am -> historyEntryFromDomain (MoveWithCaptures am.move am.captures) am.side) moves
    , _turn = turn'
    , _status = status'
    , _validMoves = validMoves'
    , _pendingAction = pending'
    }
 where
  engineColor = opponent humanColor
  (turn', status', validMoves', pending') = case phase of
    AI.PlayerTurn validMoves pending ->
      ( humanColor
      , gameStatusFromDomain Nothing
      , validMovesMapFromDomain validMoves
      , fmap pendingActionFromDomain pending
      )
    AI.EngineThinking pending ->
      ( engineColor
      , gameStatusFromDomain Nothing
      , validMovesMapFromDomain []
      , fmap pendingActionFromDomain pending
      )
    AI.Finished outcome ->
      ( humanColor
      , gameStatusFromDomain (Just outcome)
      , validMovesMapFromDomain []
      , Nothing
      )

-- | Extract common state fields from an AI state.
activeStateFields :: PlayerColor -> AI.State -> (PlayerColor, ApiGameStatus, ValidMovesMap, ApiBoard)
activeStateFields humanColor (AI.State board _moves phase) =
  let engineColor = opponent humanColor
   in case phase of
        AI.PlayerTurn validMoves _pending ->
          ( humanColor
          , gameStatusFromDomain Nothing
          , validMovesMapFromDomain validMoves
          , boardFromExtern board
          )
        AI.EngineThinking _pending ->
          ( engineColor
          , gameStatusFromDomain Nothing
          , validMovesMapFromDomain []
          , boardFromExtern board
          )
        AI.Finished outcome ->
          ( humanColor
          , gameStatusFromDomain (Just outcome)
          , validMovesMapFromDomain []
          , boardFromExtern board
          )
