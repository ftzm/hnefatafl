module Hnefatafl.App.Serialization (
  appliedMoveToJSON,
  pendingActionToJSON,
) where

import Data.Aeson (Value, object, (.=))
import Hnefatafl.Api.Types (moveFromDomain)
import Hnefatafl.Core.Data (MoveWithCaptures (..))
import Hnefatafl.Game.Common (
  AppliedMove (..),
  PendingAction (..),
  PendingActionType (..),
 )

appliedMoveToJSON :: AppliedMove -> Value
appliedMoveToJSON am =
  object
    [ "move" .= moveFromDomain (MoveWithCaptures am.move am.captures)
    , "side" .= am.side
    ]

pendingActionToJSON :: PendingAction -> Value
pendingActionToJSON pa =
  object
    [ "actionType" .= case pa.actionType of
        DrawOffer -> "draw_offer" :: Text
        UndoRequest -> "undo_request"
    , "offeredBy" .= pa.offeredBy
    ]
