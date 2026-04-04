{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hnefatafl.Interpreter.Storage.SQLite.Type (
  PlayerIdDb (..),
  PlayerType (..),
  HumanPlayerDb (..),
  EnginePlayerDb (..),
  GameIdDb (..),
  OutcomeDb (..),
  GameType (..),
  GameDb (..),
  GameJoinRow (..),
  PlayerColorDb (..),
  MoveDb (..),
  GameParticipantTokenIdDb (..),
  GameParticipantTokenDb (..),
  PendingActionTypeDb (..),
  PendingActionDb (..),
  DomainMapping (..),
) where

import Chronos
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField
import Hnefatafl.Core.Data
import Hnefatafl.Game.Common (PendingAction (..), PendingActionType (..))

-- ToField/FromField instances for Chronos Time
instance ToField Time where
  toField = SQLText . encodeIso8601 . timeToDatetime

instance FromField Time where
  fromField f@(Field (SQLText t) _) =
    case decode_lenient t of
      Just time -> Ok $ datetimeToTime time
      Nothing ->
        returnError ConversionFailed f ("couldn't parse UTCTime field: " ++ toString t)
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

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

newtype OutcomeDb = OutcomeDb (Maybe Outcome)
  deriving (Show, Eq)

instance DomainMapping OutcomeDb (Maybe Outcome) where
  toDomain = coerce
  fromDomain = coerce

instance ToField OutcomeDb where
  toField (OutcomeDb outcome) = toField $ case outcome of
    Nothing -> "ongoing" :: Text
    Just (BlackWins KingCaptured) -> "black_won_king_captured"
    Just (BlackWins WhiteSurrounded) -> "black_won_white_surrounded"
    Just (BlackWins NoWhiteMoves) -> "black_won_no_white_moves"
    Just (WhiteWins KingEscaped) -> "white_won_king_escaped"
    Just (WhiteWins ExitFort) -> "white_won_exit_fort"
    Just (WhiteWins NoBlackMoves) -> "white_won_no_black_moves"
    Just (ResignedBy White) -> "white_won_resignation"
    Just (ResignedBy Black) -> "black_won_resignation"
    Just (TimedOut White) -> "white_won_timeout"
    Just (TimedOut Black) -> "black_won_timeout"
    Just Draw -> "draw"
    Just Abandoned -> "abandoned"

instance FromField OutcomeDb where
  fromField f =
    fromField @Text f >>= \case
      "ongoing" -> pure $ OutcomeDb Nothing
      "black_won_king_captured" -> pure $ OutcomeDb $ Just $ BlackWins KingCaptured
      "black_won_white_surrounded" -> pure $ OutcomeDb $ Just $ BlackWins WhiteSurrounded
      "black_won_no_white_moves" -> pure $ OutcomeDb $ Just $ BlackWins NoWhiteMoves
      "black_won_resignation" -> pure $ OutcomeDb $ Just $ ResignedBy Black
      "black_won_timeout" -> pure $ OutcomeDb $ Just $ TimedOut Black
      "white_won_king_escaped" -> pure $ OutcomeDb $ Just $ WhiteWins KingEscaped
      "white_won_exit_fort" -> pure $ OutcomeDb $ Just $ WhiteWins ExitFort
      "white_won_no_black_moves" -> pure $ OutcomeDb $ Just $ WhiteWins NoBlackMoves
      "white_won_resignation" -> pure $ OutcomeDb $ Just $ ResignedBy White
      "white_won_timeout" -> pure $ OutcomeDb $ Just $ TimedOut White
      "draw" -> pure $ OutcomeDb $ Just Draw
      "abandoned" -> pure $ OutcomeDb $ Just Abandoned
      -- Legacy support for old values
      "white_won" -> pure $ OutcomeDb $ Just $ WhiteWins KingEscaped
      "black_won" -> pure $ OutcomeDb $ Just $ BlackWins KingCaptured
      _ -> returnError ConversionFailed f "Invalid game status"

data GameType = HotseatType | AIType | OnlineType
  deriving (Show, Eq)

instance ToField GameType where
  toField HotseatType = toField ("hotseat" :: Text)
  toField AIType = toField ("ai" :: Text)
  toField OnlineType = toField ("online" :: Text)

instance FromField GameType where
  fromField f =
    fromField @Text f >>= \case
      "hotseat" -> pure HotseatType
      "ai" -> pure AIType
      "online" -> pure OnlineType
      _ -> returnError ConversionFailed f "Invalid game type"

data GameDb = GameDb
  { gameId :: GameIdDb
  , name :: Maybe Text
  , gameType :: GameType
  , startTime :: Time
  , endTime :: Maybe Time
  , outcome :: OutcomeDb
  , createdAt :: Time
  }
  deriving (Show, Generic, ToRow, FromRow)

-- | Row type for the LEFT JOIN query across game + all variant tables
data GameJoinRow = GameJoinRow
  { gameId :: GameIdDb
  , name :: Maybe Text
  , gameType :: GameType
  , startTime :: Time
  , endTime :: Maybe Time
  , outcome :: OutcomeDb
  , createdAt :: Time
  , -- hotseat
    hotseatOwnerId :: Maybe PlayerIdDb
  , -- ai
    aiPlayerId :: Maybe PlayerIdDb
  , aiPlayerColor :: Maybe PlayerColorDb
  , aiEngineId :: Maybe PlayerIdDb
  , -- online
    onlineWhitePlayerId :: Maybe PlayerIdDb
  , onlineWhiteName :: Maybe Text
  , onlineBlackPlayerId :: Maybe PlayerIdDb
  , onlineBlackName :: Maybe Text
  }
  deriving (Show, Generic, FromRow)

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
  { playerColor :: PlayerColorDb
  , fromPosition :: Word8
  , toPosition :: Word8
  , blackLower :: Word64
  , blackUpper :: Word64
  , whiteLower :: Word64
  , whiteUpper :: Word64
  , king :: Word8
  , capturesLower :: Word64
  , capturesUpper :: Word64
  , timestamp :: Time
  }
  deriving (Show, Generic, ToRow, FromRow)

instance DomainMapping PlayerColorDb PlayerColor where
  toDomain WhiteColorDb = White
  toDomain BlackColorDb = Black
  fromDomain White = WhiteColorDb
  fromDomain Black = BlackColorDb

instance DomainMapping MoveDb GameMove where
  toDomain
    MoveDb
      { playerColor
      , fromPosition
      , toPosition
      , blackLower
      , blackUpper
      , whiteLower
      , whiteUpper
      , king
      , capturesLower
      , capturesUpper
      , timestamp
      } =
      GameMove
        { playerColor = toDomain playerColor
        , move = Move fromPosition toPosition
        , boardStateAfter =
            ExternBoard
              { black = Layer blackLower blackUpper
              , white = Layer whiteLower whiteUpper
              , king = king
              }
        , captures = Layer capturesLower capturesUpper
        , timestamp = timestamp
        }
  fromDomain GameMove{playerColor, move, boardStateAfter, captures, timestamp} =
    MoveDb
      { playerColor = fromDomain playerColor
      , fromPosition = orig move
      , toPosition = dest move
      , blackLower = boardStateAfter.black.lower
      , blackUpper = boardStateAfter.black.upper
      , whiteLower = boardStateAfter.white.lower
      , whiteUpper = boardStateAfter.white.upper
      , king = boardStateAfter.king
      , capturesLower = captures.lower
      , capturesUpper = captures.upper
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
  , createdAt :: Time
  , isActive :: Bool
  }
  deriving (Show, Generic, ToRow, FromRow)

data PendingActionTypeDb = DrawOfferDb | UndoRequestDb
  deriving (Show, Eq)

instance ToField PendingActionTypeDb where
  toField DrawOfferDb = toField ("draw_offer" :: Text)
  toField UndoRequestDb = toField ("undo_request" :: Text)

instance FromField PendingActionTypeDb where
  fromField f =
    fromField @Text f >>= \case
      "draw_offer" -> pure DrawOfferDb
      "undo_request" -> pure UndoRequestDb
      _ -> returnError ConversionFailed f "Invalid pending action type"

instance DomainMapping PendingActionTypeDb PendingActionType where
  toDomain DrawOfferDb = DrawOffer
  toDomain UndoRequestDb = UndoRequest
  fromDomain DrawOffer = DrawOfferDb
  fromDomain UndoRequest = UndoRequestDb

data PendingActionDb = PendingActionDb
  { actionType :: PendingActionTypeDb
  , offeredBy :: PlayerColorDb
  }
  deriving (Show, Generic, ToRow, FromRow)

instance DomainMapping PendingActionDb PendingAction where
  toDomain PendingActionDb{actionType, offeredBy} =
    PendingAction (toDomain actionType) (toDomain offeredBy)
  fromDomain PendingAction{actionType, offeredBy} =
    PendingActionDb (fromDomain actionType) (fromDomain offeredBy)
