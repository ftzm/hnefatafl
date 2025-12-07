{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hnefatafl.Interpreter.Storage.SQLite.Type (
  PlayerIdDb (..),
  PlayerType (..),
  HumanPlayerDb (..),
  EnginePlayerDb (..),
  GameIdDb (..),
  GameStatusDb (..),
  GameDb (..),
  MoveIdDb (..),
  PlayerColorDb (..),
  MoveDb (..),
  GameParticipantTokenIdDb (..),
  GameParticipantTokenDb (..),
  DomainMapping (..),
) where

import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Hnefatafl.Core.Data

--------------------------------------------------------------------------------
-- DB types
-- Internal types for SQLite storage implementation

newtype PlayerIdDb = PlayerIdDb Text
  deriving (Show, Eq)
  deriving newtype (ToField, FromField)

instance DomainMapping PlayerIdDb PlayerId where
  toDomain (PlayerIdDb txt) = PlayerId txt
  fromDomain (PlayerId txt) = PlayerIdDb txt

data PlayerType = HumanType | EngineType
  deriving (Show, Eq)

instance FromField PlayerType where
  fromField f =
    fromField @Text f >>= \case
      "human" -> pure HumanType
      "engine" -> pure EngineType
      _ -> returnError ConversionFailed f "Invalid player type"

data HumanPlayerDb = HumanPlayerDb
  { playerId :: PlayerIdDb
  , name :: Text
  , email :: Maybe Text
  }
  deriving (Show, Generic, ToRow, FromRow)

instance DomainMapping HumanPlayerDb HumanPlayer where
  toDomain HumanPlayerDb{playerId, name, email} = HumanPlayer (toDomain playerId) name email
  fromDomain HumanPlayer{playerId, name, email} = HumanPlayerDb (fromDomain playerId) name email

data EnginePlayerDb = EnginePlayerDb
  { playerId :: PlayerIdDb
  , version :: Text
  }
  deriving (Show, Generic, ToRow, FromRow)

instance DomainMapping EnginePlayerDb EnginePlayer where
  toDomain EnginePlayerDb{playerId, version} = EnginePlayer (toDomain playerId) version
  fromDomain EnginePlayer{playerId, version} = EnginePlayerDb (fromDomain playerId) version

newtype GameIdDb = GameIdDb Text
  deriving (Show, Eq)
  deriving newtype (ToField, FromField)

instance DomainMapping GameIdDb GameId where
  toDomain (GameIdDb txt) = GameId txt
  fromDomain (GameId txt) = GameIdDb txt

newtype GameStatusDb = GameStatusDb GameStatus
  deriving (Show, Eq)

instance DomainMapping GameStatusDb GameStatus where
  toDomain = coerce
  fromDomain = coerce

instance ToField GameStatusDb where
  toField (GameStatusDb status) = toField $ case status of
    Ongoing -> "ongoing" :: Text
    BlackWonKingCaptured -> "black_won_king_captured"
    BlackWonWhiteSurrounded -> "black_won_white_surrounded"
    BlackWonNoWhiteMoves -> "black_won_no_white_moves"
    BlackWonResignation -> "black_won_resignation"
    BlackWonTimeout -> "black_won_timeout"
    WhiteWonKingEscaped -> "white_won_king_escaped"
    WhiteWonExitFort -> "white_won_exit_fort"
    WhiteWonNoBlackMoves -> "white_won_no_black_moves"
    WhiteWonResignation -> "white_won_resignation"
    WhiteWonTimeout -> "white_won_timeout"
    Draw -> "draw"
    Abandoned -> "abandoned"

instance FromField GameStatusDb where
  fromField f =
    fromField @Text f >>= \case
      "ongoing" -> pure $ GameStatusDb Ongoing
      "black_won_king_captured" -> pure $ GameStatusDb BlackWonKingCaptured
      "black_won_white_surrounded" -> pure $ GameStatusDb BlackWonWhiteSurrounded
      "black_won_no_white_moves" -> pure $ GameStatusDb BlackWonNoWhiteMoves
      "black_won_resignation" -> pure $ GameStatusDb BlackWonResignation
      "black_won_timeout" -> pure $ GameStatusDb BlackWonTimeout
      "white_won_king_escaped" -> pure $ GameStatusDb WhiteWonKingEscaped
      "white_won_exit_fort" -> pure $ GameStatusDb WhiteWonExitFort
      "white_won_no_black_moves" -> pure $ GameStatusDb WhiteWonNoBlackMoves
      "white_won_resignation" -> pure $ GameStatusDb WhiteWonResignation
      "white_won_timeout" -> pure $ GameStatusDb WhiteWonTimeout
      "draw" -> pure $ GameStatusDb Draw
      "abandoned" -> pure $ GameStatusDb Abandoned
      -- Legacy support for old values
      "white_won" -> pure $ GameStatusDb WhiteWonKingEscaped  -- Default to most common white victory
      "black_won" -> pure $ GameStatusDb BlackWonKingCaptured  -- Default to most common black victory
      _ -> returnError ConversionFailed f "Invalid game status"

data GameDb = GameDb
  { gameId :: GameIdDb
  , name :: Maybe Text
  , whitePlayerId :: Maybe PlayerIdDb
  , blackPlayerId :: Maybe PlayerIdDb
  , startTime :: UTCTime
  , endTime :: Maybe UTCTime
  , gameStatus :: GameStatusDb
  , createdAt :: UTCTime
  }
  deriving (Show, Generic, ToRow, FromRow)

instance DomainMapping GameDb Game where
  toDomain
    GameDb
      { gameId
      , name
      , whitePlayerId
      , blackPlayerId
      , startTime
      , endTime
      , gameStatus
      , createdAt
      } =
      Game
        { gameId = toDomain gameId
        , name = name
        , whitePlayerId = toDomain <$> whitePlayerId
        , blackPlayerId = toDomain <$> blackPlayerId
        , startTime = startTime
        , endTime = endTime
        , gameStatus = toDomain gameStatus
        , createdAt = createdAt
        }
  fromDomain
    Game
      { gameId
      , name
      , whitePlayerId
      , blackPlayerId
      , startTime
      , endTime
      , gameStatus
      , createdAt
      } =
      GameDb
        { gameId = fromDomain gameId
        , name = name
        , whitePlayerId = fromDomain <$> whitePlayerId
        , blackPlayerId = fromDomain <$> blackPlayerId
        , startTime = startTime
        , endTime = endTime
        , gameStatus = fromDomain gameStatus
        , createdAt = createdAt
        }

newtype MoveIdDb = MoveIdDb Text
  deriving (Show, Eq)
  deriving newtype (ToField, FromField)

data PlayerColorDb = WhiteColorDb | BlackColorDb
  deriving (Show, Eq)

instance ToField PlayerColorDb where
  toField WhiteColorDb = toField ("white" :: Text)
  toField BlackColorDb = toField ("black" :: Text)

instance FromField PlayerColorDb where
  fromField f =
    fromField @Text f >>= \case
      "white" -> pure WhiteColorDb
      "black" -> pure BlackColorDb
      _ -> returnError ConversionFailed f "Invalid player color"

data MoveDb = MoveDb
  { moveId :: MoveIdDb
  , moveNumber :: Int
  , playerColor :: PlayerColorDb
  , fromPosition :: Word8
  , toPosition :: Word8
  , blackLower :: Int64
  , blackUpper :: Int64
  , whiteLower :: Int64
  , whiteUpper :: Int64
  , king :: Word8
  , timestamp :: UTCTime
  }
  deriving (Show, Generic, ToRow, FromRow)

instance DomainMapping MoveIdDb MoveId where
  toDomain = coerce
  fromDomain = coerce

instance DomainMapping PlayerColorDb PlayerColor where
  toDomain WhiteColorDb = White
  toDomain BlackColorDb = Black
  fromDomain White = WhiteColorDb
  fromDomain Black = BlackColorDb

instance DomainMapping MoveDb GameMove where
  toDomain
    MoveDb
      { moveId
      , moveNumber
      , playerColor
      , fromPosition
      , toPosition
      , blackLower
      , blackUpper
      , whiteLower
      , whiteUpper
      , king
      , timestamp
      } =
      GameMove
        { moveId = toDomain moveId
        , moveNumber = moveNumber
        , playerColor = toDomain playerColor
        , move = Move fromPosition toPosition
        , boardStateAfter =
            ExternBoard
              { black = Layer blackLower blackUpper
              , white = Layer whiteLower whiteUpper
              , king = king
              }
        , timestamp = timestamp
        }
  fromDomain GameMove{moveId, moveNumber, playerColor, move, boardStateAfter, timestamp} =
    MoveDb
      { moveId = fromDomain moveId
      , moveNumber = moveNumber
      , playerColor = fromDomain playerColor
      , fromPosition = orig move
      , toPosition = dest move
      , blackLower = boardStateAfter.black.lower
      , blackUpper = boardStateAfter.black.upper
      , whiteLower = boardStateAfter.white.lower
      , whiteUpper = boardStateAfter.white.upper
      , king = boardStateAfter.king
      , timestamp = timestamp
      }

newtype GameParticipantTokenIdDb = GameParticipantTokenIdDb Text
  deriving (Show, Eq)
  deriving newtype (ToField, FromField)

instance DomainMapping GameParticipantTokenIdDb GameParticipantTokenId where
  toDomain = coerce
  fromDomain = coerce

data GameParticipantTokenDb = GameParticipantTokenDb
  { tokenId :: GameParticipantTokenIdDb
  , gameId :: GameIdDb
  , token :: Text
  , role :: PlayerColorDb
  , createdAt :: UTCTime
  , isActive :: Bool
  }
  deriving (Show, Generic, ToRow, FromRow)