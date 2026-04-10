{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.WS.AI (
  AIServerMessage (..),
  AIClientMessage (..),
  aiOptions,
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Aeson qualified as Aeson
import Data.OpenApi (
  Discriminator (..),
  NamedSchema (..),
  ToSchema (..),
  genericDeclareNamedSchema,
 )
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.SchemaOptions (fromAesonOptions)
import Hnefatafl.Api.Types (
  ApiBoard,
  ApiGameStatus,
  ApiMove,
  camelToSnake,
 )
import Hnefatafl.Api.Types.WS (
  AppliedMovePayload,
  PendingActionPayload,
 )
import Hnefatafl.Core.Data (
  GameId,
  PlayerColor,
 )

-------------------------------------------------------------------------------
-- Options

aiOptions :: Aeson.Options
aiOptions =
  defaultOptions
    { Aeson.sumEncoding = Aeson.TaggedObject "type" "contents"
    , Aeson.constructorTagModifier = camelToSnake . drop 2
    , Aeson.fieldLabelModifier = drop 1
    }

-------------------------------------------------------------------------------
-- Server → Client

data AIServerMessage
  = AIGameState
      { _gameId :: GameId
      , _humanColor :: PlayerColor
      , _board :: ApiBoard
      , _history :: [AppliedMovePayload]
      , _turn :: Text
      , _status :: ApiGameStatus
      , _validMoves :: [ApiMove]
      , _pendingAction :: Maybe PendingActionPayload
      }
  | AIEngineMoved
      { _move :: ApiMove
      , _side :: PlayerColor
      , _turn :: Text
      , _status :: ApiGameStatus
      , _validMoves :: [ApiMove]
      , _board :: ApiBoard
      }
  | AIGameOver
      { _status :: ApiGameStatus
      }
  | AIUndoApplied
      { _turn :: Text
      , _status :: ApiGameStatus
      , _validMoves :: [ApiMove]
      , _board :: ApiBoard
      }
  deriving (Show, Eq, Generic)

instance ToJSON AIServerMessage where toJSON = genericToJSON aiOptions
instance FromJSON AIServerMessage where parseJSON = genericParseJSON aiOptions
instance ToSchema AIServerMessage where
  declareNamedSchema proxy = do
    NamedSchema name schema <-
      genericDeclareNamedSchema (fromAesonOptions aiOptions) proxy
    pure $
      NamedSchema name $
        schema{OpenApi._schemaDiscriminator = Just (Discriminator "type" mempty)}

-------------------------------------------------------------------------------
-- Client → Server

data AIClientMessage
  = AIMove {_orig :: Word8, _dest :: Word8}
  | AIUndo
  | AIResign
  | AIOfferDraw
  | AIAcceptDraw
  | AIDeclineDraw
  deriving (Show, Eq, Generic)

instance ToJSON AIClientMessage where toJSON = genericToJSON aiOptions
instance FromJSON AIClientMessage where parseJSON = genericParseJSON aiOptions
instance ToSchema AIClientMessage where
  declareNamedSchema proxy = do
    NamedSchema name schema <-
      genericDeclareNamedSchema (fromAesonOptions aiOptions) proxy
    pure $
      NamedSchema name $
        schema{OpenApi._schemaDiscriminator = Just (Discriminator "type" mempty)}
