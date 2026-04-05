module Hnefatafl.App.AI.Serialization (
  notificationToJSON,
  gameStateToJSON,
) where

import Data.Aeson (
  Key,
  Value,
  encode,
  object,
  (.=),
 )
import Hnefatafl.Api.Types (
  ApiMove,
  boardFromExtern,
  gameStatusFromDomain,
  moveFromDomain,
 )
import Hnefatafl.App.Serialization (appliedMoveToJSON, pendingActionToJSON)
import Hnefatafl.Core.Data (
  GameId (..),
  MoveWithCaptures (..),
  PlayerColor (..),
 )
import Hnefatafl.Game.AI qualified as AI
import Hnefatafl.Game.Common (AppliedMove (..))

-- | Serialize a player notification.
notificationToJSON :: AI.State -> AI.PlayerNotification -> LByteString
notificationToJSON newState = \case
  AI.EngineMoved am ->
    encode $
      object $
        [ "type" .= ("engine_moved" :: Text)
        , "move" .= moveFromDomain (MoveWithCaptures am.move am.captures)
        , "side" .= am.side
        ]
          <> stateFields newState
  AI.GameEnded outcome ->
    encode $
      object
        [ "type" .= ("game_over" :: Text)
        , "status" .= gameStatusFromDomain (Just outcome)
        ]
  AI.UndoApplied ->
    encode $
      object $
        ["type" .= ("undo_applied" :: Text)]
          <> stateFields newState

-- | Common state fields included in messages where the board/turn changed.
stateFields :: AI.State -> [(Key, Value)]
stateFields (AI.State board _moves phase) =
  case phase of
    AI.PlayerTurn validMoves _pending ->
      [ "turn" .= ("player" :: Text)
      , "status" .= gameStatusFromDomain Nothing
      , "validMoves" .= map moveFromDomain validMoves
      , "board" .= boardFromExtern board
      ]
    AI.EngineThinking _pending ->
      [ "turn" .= ("engine" :: Text)
      , "status" .= gameStatusFromDomain Nothing
      , "board" .= boardFromExtern board
      ]
    AI.Finished outcome ->
      [ "status" .= gameStatusFromDomain (Just outcome)
      , "board" .= boardFromExtern board
      ]

-- | Serialize the full game state for initial sync on connect.
gameStateToJSON :: GameId -> PlayerColor -> AI.State -> LByteString
gameStateToJSON gId humanColor (AI.State board moves phase) =
  encode $
    object $
      [ "type" .= ("game_state" :: Text)
      , "gameId" .= gId
      , "humanColor" .= humanColor
      , "board" .= boardFromExtern board
      , "history" .= map appliedMoveToJSON moves
      ]
        <> case phase of
          AI.PlayerTurn validMoves pending ->
            [ "turn" .= ("player" :: Text)
            , "status" .= gameStatusFromDomain Nothing
            , "validMoves" .= map moveFromDomain validMoves
            , "pendingAction" .= fmap pendingActionToJSON pending
            ]
          AI.EngineThinking pending ->
            [ "turn" .= ("engine" :: Text)
            , "status" .= gameStatusFromDomain Nothing
            , "validMoves" .= ([] :: [ApiMove])
            , "pendingAction" .= fmap pendingActionToJSON pending
            ]
          AI.Finished outcome ->
            [ "status" .= gameStatusFromDomain (Just outcome)
            , "validMoves" .= ([] :: [ApiMove])
            , "pendingAction" .= (Nothing :: Maybe Value)
            ]
