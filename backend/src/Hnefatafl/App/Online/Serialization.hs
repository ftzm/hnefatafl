module Hnefatafl.App.Online.Serialization (
  notificationToJSON,
  actorNotificationToJSON,
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
  opponent,
 )
import Hnefatafl.Game.Common (AppliedMove (..))
import Hnefatafl.Game.Online qualified as Online

-- | Serialize a notification for the opponent.
notificationToJSON :: Online.State -> Online.Notification -> LByteString
notificationToJSON newState = \case
  Online.OpponentMoved am ->
    encode $
      object $
        [ "type" .= ("opponent_moved" :: Text)
        , "move" .= moveFromDomain (MoveWithCaptures am.move am.captures)
        , "side" .= am.side
        ]
          <> stateFields newState
  Online.OpponentResigned color ->
    encode $
      object
        [ "type" .= ("game_over" :: Text)
        , "reason" .= ("resignation" :: Text)
        , "winner" .= opponent color
        ]
  Online.OpponentTimedOut color ->
    encode $
      object
        [ "type" .= ("game_over" :: Text)
        , "reason" .= ("timeout" :: Text)
        , "winner" .= opponent color
        ]
  Online.DrawOffered color ->
    encode $ object ["type" .= ("draw_offered" :: Text), "by" .= color]
  Online.DrawAccepted ->
    encode $ object ["type" .= ("game_over" :: Text), "reason" .= ("draw" :: Text)]
  Online.DrawDeclined ->
    encode $ object ["type" .= ("draw_declined" :: Text)]
  Online.UndoRequested color ->
    encode $ object ["type" .= ("undo_requested" :: Text), "by" .= color]
  Online.UndoAccepted ->
    encode $
      object $
        ["type" .= ("undo_accepted" :: Text)]
          <> stateFields newState
  Online.UndoDeclined ->
    encode $ object ["type" .= ("undo_declined" :: Text)]

-- | Serialize an actor notification.
actorNotificationToJSON ::
  Online.State -> Online.ActorNotification -> LByteString
actorNotificationToJSON newState = \case
  Online.GameEnded outcome ->
    encode $
      object
        [ "type" .= ("game_over" :: Text)
        , "status" .= gameStatusFromDomain (Just outcome)
        ]
  Online.UndoApplied ->
    encode $
      object $
        ["type" .= ("undo_accepted" :: Text)]
          <> stateFields newState

-- | Common state fields included in messages where the board/turn changed.
-- Includes turn, status, valid moves, and board.
stateFields :: Online.State -> [(Key, Value)]
stateFields (Online.State board _moves phase) =
  case phase of
    Online.Active turn validMoves _pending ->
      [ "turn" .= turn
      , "status" .= gameStatusFromDomain Nothing
      , "validMoves" .= map moveFromDomain validMoves
      , "board" .= boardFromExtern board
      ]
    Online.Finished outcome ->
      [ "status" .= gameStatusFromDomain (Just outcome)
      , "board" .= boardFromExtern board
      ]

-- | Serialize the full game state for initial sync on connect.
gameStateToJSON :: GameId -> Online.State -> LByteString
gameStateToJSON gId (Online.State board moves phase) =
  encode $
    object $
      [ "type" .= ("game_state" :: Text)
      , "gameId" .= gId
      , "board" .= boardFromExtern board
      , "history" .= map appliedMoveToJSON moves
      ]
        <> case phase of
          Online.Active turn validMoves pending ->
            [ "turn" .= turn
            , "status" .= gameStatusFromDomain Nothing
            , "validMoves" .= map moveFromDomain validMoves
            , "pendingAction" .= fmap pendingActionToJSON pending
            ]
          Online.Finished outcome ->
            [ "status" .= gameStatusFromDomain (Just outcome)
            , "validMoves" .= ([] :: [ApiMove])
            , "pendingAction" .= (Nothing :: Maybe Value)
            ]
