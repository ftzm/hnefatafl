module Hnefatafl.App.AI.Serialization (
  notificationMessage,
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
import Hnefatafl.Api.Types.WS.AI (AIServerMessage (..))
import Hnefatafl.Core.Data (
  GameId,
  MoveWithCaptures (..),
  PlayerColor,
 )
import Hnefatafl.Game.AI qualified as AI
import Hnefatafl.Game.Common (AppliedMove (..))

-- | Convert a player notification to an AIServerMessage.
notificationMessage :: AI.State -> AI.PlayerNotification -> AIServerMessage
notificationMessage newState = \case
  AI.EngineMoved am ->
    let (turn', status', validMoves', board') = activeStateFields newState
     in AIEngineMoved
          { _move = moveFromDomain (MoveWithCaptures am.move am.captures)
          , _side = am.side
          , _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }
  AI.GameEnded outcome ->
    AIGameOver{_status = gameStatusFromDomain (Just outcome)}
  AI.UndoApplied ->
    let (turn', status', validMoves', board') = activeStateFields newState
     in AIUndoApplied
          { _turn = turn'
          , _status = status'
          , _validMoves = validMoves'
          , _board = board'
          }

-- | Serialize the full game state for initial sync on connect.
gameStateMessage :: GameId -> PlayerColor -> AI.State -> AIServerMessage
gameStateMessage gId humanColor (AI.State board moves phase) =
  AIGameState
    { _gameId = gId
    , _humanColor = humanColor
    , _board = boardFromExtern board
    , _history = map appliedMoveFromDomain moves
    , _turn = turnText
    , _status = status'
    , _validMoves = validMoves'
    , _pendingAction = pending'
    }
 where
  (turnText, status', validMoves', pending') = case phase of
    AI.PlayerTurn validMoves pending ->
      ( "player"
      , gameStatusFromDomain Nothing
      , map moveFromDomain validMoves
      , fmap pendingActionFromDomain pending
      )
    AI.EngineThinking pending ->
      ( "engine"
      , gameStatusFromDomain Nothing
      , []
      , fmap pendingActionFromDomain pending
      )
    AI.Finished outcome ->
      ( "player"
      , gameStatusFromDomain (Just outcome)
      , []
      , Nothing
      )

-- | Extract common state fields from an AI state.
activeStateFields :: AI.State -> (Text, ApiGameStatus, [ApiMove], ApiBoard)
activeStateFields (AI.State board _moves phase) =
  case phase of
    AI.PlayerTurn validMoves _pending ->
      ( "player"
      , gameStatusFromDomain Nothing
      , map moveFromDomain validMoves
      , boardFromExtern board
      )
    AI.EngineThinking _pending ->
      ( "engine"
      , gameStatusFromDomain Nothing
      , []
      , boardFromExtern board
      )
    AI.Finished outcome ->
      ( "player"
      , gameStatusFromDomain (Just outcome)
      , []
      , boardFromExtern board
      )
