{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.WS.Online (
  OnlineServerMessage (..),
  OnlineClientMessage (..),
  onlineOptions,
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
  WsErrorCode,
 )
import Hnefatafl.Core.Data (
  GameId,
  PlayerColor,
 )

-------------------------------------------------------------------------------
-- Options

onlineOptions :: Aeson.Options
onlineOptions =
  defaultOptions
    { Aeson.sumEncoding = Aeson.TaggedObject "type" "contents"
    , Aeson.constructorTagModifier = camelToSnake . drop 6
    , Aeson.fieldLabelModifier = drop 1
    }

-------------------------------------------------------------------------------
-- Server → Client

data OnlineServerMessage
  = OnlineGameState
      { _gameId :: GameId
      , _board :: ApiBoard
      , _history :: [AppliedMovePayload]
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: [ApiMove]
      , _pendingAction :: Maybe PendingActionPayload
      }
  | OnlineOpponentMoved
      { _move :: ApiMove
      , _side :: PlayerColor
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: [ApiMove]
      , _board :: ApiBoard
      }
  | OnlineGameOver
      { _status :: ApiGameStatus
      }
  | OnlineDrawOffered
      { _by :: PlayerColor
      }
  | OnlineDrawDeclined
  | OnlineUndoRequested
      { _by :: PlayerColor
      }
  | OnlineUndoAccepted
      { _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: [ApiMove]
      , _board :: ApiBoard
      }
  | OnlineUndoDeclined
  | OnlineError
      { _code :: WsErrorCode
      , _message :: Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON OnlineServerMessage where toJSON = genericToJSON onlineOptions
instance FromJSON OnlineServerMessage where parseJSON = genericParseJSON onlineOptions
instance ToSchema OnlineServerMessage where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema (fromAesonOptions onlineOptions) proxy
    pure $
      NamedSchema name $
        schema{OpenApi._schemaDiscriminator = Just (Discriminator "type" mempty)}

-------------------------------------------------------------------------------
-- Client → Server

data OnlineClientMessage
  = OnlineMove {_orig :: Word8, _dest :: Word8}
  | OnlineResign
  | OnlineOfferDraw
  | OnlineAcceptDraw
  | OnlineDeclineDraw
  | OnlineRequestUndo
  | OnlineAcceptUndo
  | OnlineDeclineUndo
  deriving (Show, Eq, Generic)

instance ToJSON OnlineClientMessage where toJSON = genericToJSON onlineOptions
instance FromJSON OnlineClientMessage where parseJSON = genericParseJSON onlineOptions
instance ToSchema OnlineClientMessage where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema (fromAesonOptions onlineOptions) proxy
    pure $
      NamedSchema name $
        schema{OpenApi._schemaDiscriminator = Just (Discriminator "type" mempty)}
