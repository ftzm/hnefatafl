{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.WS (
  -- * Error codes
  WsErrorCode (..),
  transitionErrorToCode,

  -- * Payload types
  AppliedMovePayload (..),
  PendingActionPayload (..),
  appliedMoveFromDomain,
  pendingActionFromDomain,

  -- * Auth
  AuthMessage (..),

  -- * Utilities
  wsErrorCodeOptions,
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson qualified as Aeson
import Data.OpenApi (
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  ToSchema (..),
  genericDeclareNamedSchema,
 )
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.SchemaOptions (fromAesonOptions)
import Hnefatafl.Api.Types (
  ApiMove,
  camelToSnake,
  moveFromDomain,
 )
import Hnefatafl.Core.Data (
  MoveWithCaptures (..),
  PlayerColor,
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  PendingAction (..),
  PendingActionType (..),
  TransitionError,
 )
import Hnefatafl.Game.Common qualified as Common

-------------------------------------------------------------------------------
-- Error codes

data WsErrorCode
  = InvalidMessage
  | InvalidAuth
  | InvalidToken
  | NotYourTurn
  | GameAlreadyFinished
  | InvalidMove
  | NoPendingOffer
  | CannotRespondToOwnOffer
  | ActionAlreadyPending
  | NoMovesToUndo
  | EngineError
  | EngineSearchFailed
  deriving (Show, Eq, Generic)

wsErrorCodeOptions :: Aeson.Options
wsErrorCodeOptions =
  defaultOptions
    { Aeson.constructorTagModifier = camelToSnake
    , Aeson.allNullaryToStringTag = True
    }

instance ToJSON WsErrorCode where toJSON = genericToJSON wsErrorCodeOptions
instance FromJSON WsErrorCode where parseJSON = genericParseJSON wsErrorCodeOptions
instance ToSchema WsErrorCode where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions wsErrorCodeOptions)

transitionErrorToCode :: TransitionError -> WsErrorCode
transitionErrorToCode = \case
  Common.NotYourTurn -> NotYourTurn
  Common.GameAlreadyFinished -> GameAlreadyFinished
  Common.InvalidMove -> InvalidMove
  Common.NoPendingOffer -> NoPendingOffer
  Common.CannotRespondToOwnOffer -> CannotRespondToOwnOffer
  Common.ActionAlreadyPending -> ActionAlreadyPending
  Common.NoMovesToUndo -> NoMovesToUndo
  Common.EngineError -> EngineError

-------------------------------------------------------------------------------
-- Payload types

data AppliedMovePayload = AppliedMovePayload
  { move :: ApiMove
  , side :: PlayerColor
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

appliedMoveFromDomain :: AppliedMove -> AppliedMovePayload
appliedMoveFromDomain am =
  AppliedMovePayload
    { move = moveFromDomain (MoveWithCaptures am.move am.captures)
    , side = am.side
    }

data PendingActionPayload = PendingActionPayload
  { actionType :: Text
  , offeredBy :: PlayerColor
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

pendingActionFromDomain :: PendingAction -> PendingActionPayload
pendingActionFromDomain pa =
  PendingActionPayload
    { actionType = case pa.actionType of
        DrawOffer -> "draw_offer"
        UndoRequest -> "undo_request"
    , offeredBy = pa.offeredBy
    }

-------------------------------------------------------------------------------
-- Auth message (hand-written — standalone record with type discriminator)

newtype AuthMessage = AuthMessage {token :: Text}
  deriving (Show, Eq)

instance FromJSON AuthMessage where
  parseJSON = withObject "AuthMessage" $ \o -> do
    typ <- o .: "type"
    guard (typ == ("auth" :: Text))
    AuthMessage <$> o .: "token"

instance ToJSON AuthMessage where
  toJSON a =
    object
      [ "type" .= ("auth" :: Text)
      , "token" .= a.token
      ]

instance ToSchema AuthMessage where
  declareNamedSchema _ = do
    let stringSchema = mempty{OpenApi._schemaType = Just OpenApiString}
        constTypeField =
          Inline $
            stringSchema{OpenApi._schemaEnum = Just [toJSON ("auth" :: Text)]}
    pure $
      NamedSchema (Just "AuthMessage") $
        mempty
          { OpenApi._schemaType = Just OpenApiObject
          , OpenApi._schemaRequired = ["type", "token"]
          , OpenApi._schemaProperties =
              fromList
                [ ("type", constTypeField)
                , ("token", Inline stringSchema)
                ]
          }
