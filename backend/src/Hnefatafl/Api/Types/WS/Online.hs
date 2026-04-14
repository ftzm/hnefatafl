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

onlineOptions :: Aeson.Options
onlineOptions =
  defaultOptions
    { Aeson.sumEncoding = Aeson.TaggedObject "type" "contents"
    , Aeson.constructorTagModifier = lcFirst . drop 6
    , Aeson.fieldLabelModifier = drop 1
    }

-------------------------------------------------------------------------------
-- Server → Client

data OnlineServerMessage
  = OnlineGameState
      { _gameId :: GameId
      , _playerColor :: PlayerColor
      , _board :: ApiBoard
      , _history :: [HistoryEntry]
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: ValidMovesMap
      , _pendingAction :: Maybe PendingActionPayload
      }
  | OnlineMoveMade
      { _move :: ApiMove
      , _side :: PlayerColor
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: ValidMovesMap
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
      { _moveCount :: Int
      , _turn :: PlayerColor
      , _status :: ApiGameStatus
      , _validMoves :: ValidMovesMap
      , _board :: ApiBoard
      }
  | OnlineUndoDeclined
  | OnlineOpponentJoined
  | OnlineOpponentLeft
  deriving (Show, Eq, Generic)

instance ToJSON OnlineServerMessage where toJSON = genericToJSON onlineOptions
instance FromJSON OnlineServerMessage where
  parseJSON = genericParseJSON onlineOptions
instance ToSchema OnlineServerMessage where
  declareNamedSchema =
    genericDeclareNamedSchema (fromAesonOptions onlineOptions)

-------------------------------------------------------------------------------
-- Client → Server

data OnlineClientMessage
  = OnlineMove {_from :: Position, _to :: Position}
  | OnlineResign
  | OnlineOfferDraw
  | OnlineAcceptDraw
  | OnlineDeclineDraw
  | OnlineRequestUndo
  | OnlineAcceptUndo
  | OnlineDeclineUndo
  deriving (Show, Eq, Generic)

instance ToJSON OnlineClientMessage where toJSON = genericToJSON onlineOptions
instance FromJSON OnlineClientMessage where
  parseJSON = genericParseJSON onlineOptions
instance ToSchema OnlineClientMessage where
  declareNamedSchema =
    genericDeclareNamedSchema (fromAesonOptions onlineOptions)
