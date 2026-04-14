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
  ToSchema (..),
  genericDeclareNamedSchema,
 )
import Data.OpenApi.SchemaOptions (fromAesonOptions)
import Hnefatafl.Api.Types (
  ApiBoard,
  ApiGameStatus,
  ApiMove,
  HistoryEntry,
  Position,
  ValidMovesMap,
  lcFirst,
 )
import Hnefatafl.Api.Types.WS (
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
    , Aeson.constructorTagModifier = lcFirst . drop 2
    , Aeson.fieldLabelModifier = drop 1
    }

-------------------------------------------------------------------------------
-- Server → Client

data AIServerMessage
  = AIGameState
      { _gameId :: GameId
      , _playerColor :: PlayerColor
      , _board :: ApiBoard
      , _history :: [HistoryEntry]
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: ValidMovesMap
      , _pendingAction :: Maybe PendingActionPayload
      }
  | AIMoveMade
      { _move :: ApiMove
      , _side :: PlayerColor
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: ValidMovesMap
      , _board :: ApiBoard
      }
  | AIGameOver
      { _status :: ApiGameStatus
      }
  | AIUndoAccepted
      { _moveCount :: Int
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: ValidMovesMap
      , _board :: ApiBoard
      }
  deriving (Show, Eq, Generic)

instance ToJSON AIServerMessage where toJSON = genericToJSON aiOptions
instance FromJSON AIServerMessage where parseJSON = genericParseJSON aiOptions
instance ToSchema AIServerMessage where
  declareNamedSchema =
    genericDeclareNamedSchema (fromAesonOptions aiOptions)

-------------------------------------------------------------------------------
-- Client → Server

data AIClientMessage
  = AIMove {_from :: Position, _to :: Position}
  | AIUndo
  | AIResign
  | AIOfferDraw
  | AIAcceptDraw
  | AIDeclineDraw
  deriving (Show, Eq, Generic)

instance ToJSON AIClientMessage where toJSON = genericToJSON aiOptions
instance FromJSON AIClientMessage where parseJSON = genericParseJSON aiOptions
instance ToSchema AIClientMessage where
  declareNamedSchema =
    genericDeclareNamedSchema (fromAesonOptions aiOptions)
