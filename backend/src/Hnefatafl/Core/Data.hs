{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hnefatafl.Core.Data (
  -- * Player Types
  PlayerId (..),
  HumanPlayer (..),
  EnginePlayer (..),
  Player (..),

  -- * Game Types
  GameId (..),
  BlackWinCondition (..),
  WhiteWinCondition (..),
  Outcome (..),
  Participant (..),
  GameMode (..),
  Game (..),

  -- * Board and Move Types
  Layer (..),
  ExternBoard (..),
  Move (..),
  MoveWithCaptures (..),
  MoveResult (..),
  PlayerColor (..),
  opponent,
  GameMove (..),

  -- * Time Control Types
  Seconds (..),
  TimeControl (..),
  ClockState (..),
  RemainingTime,
  mkRemainingTime,
  deduct,
  addIncrement,
  toTimespan,
  secondsToRemainingTime,
  secondsToTimespan,
  nanosPerMillisecond,
  remainingToMicroseconds,
  remainingToMs,
  timeToMs,

  -- * Game Participant Token Types
  GameParticipantTokenId (..),
  GameParticipantToken (..),

  -- * Utilities
  DomainMapping (..),
) where

import Chronos (Time, Timespan (..), getTime)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
  withText,
 )
import Data.Aeson qualified as Aeson
import Data.Char (toLower)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.SchemaOptions (fromAesonOptions)
import Language.Haskell.TH.Syntax (Lift)
import Refined (NonNegative, Positive, Refined, unrefine)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype PlayerId = PlayerId Text
  deriving (Show, Eq)

data HumanPlayer = HumanPlayer
  { playerId :: PlayerId
  , name :: Text
  , email :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data EnginePlayer = EnginePlayer
  { playerId :: PlayerId
  , version :: Text
  }
  deriving (Show, Eq, Generic)

data Player = EnginePlayerTag EnginePlayer | HumanPlayerTag HumanPlayer
  deriving (Show, Eq)

newtype GameId = GameId Text
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Hashable)
  deriving anyclass (ToSchema)

-- | Conditions under which Black wins
data BlackWinCondition = KingCaptured | WhiteSurrounded | NoWhiteMoves
  deriving (Show, Eq)

-- | Conditions under which White wins
data WhiteWinCondition = KingEscaped | ExitFort | NoBlackMoves
  deriving (Show, Eq)

data Outcome
  = BlackWins BlackWinCondition
  | WhiteWins WhiteWinCondition
  | ResignedBy PlayerColor
  | TimedOut PlayerColor
  | Draw
  | Abandoned
  deriving (Show, Eq)

data Participant
  = RegisteredPlayer PlayerId
  | AnonymousPlayer Text
  deriving (Show, Eq)

data GameMode
  = Hotseat (Maybe PlayerId)
  | VsAI (Maybe PlayerId) PlayerColor PlayerId
  | Online (Maybe Participant) (Maybe Participant)
  deriving (Show, Eq)

data Game = Game
  { gameId :: GameId
  , name :: Maybe Text
  , mode :: GameMode
  , startTime :: Time
  , endTime :: Maybe Time
  , outcome :: Maybe Outcome
  , createdAt :: Time
  }
  deriving (Show, Eq, Generic)

-- | Represents a layer of the board using bit manipulation
data Layer = Layer
  { lower :: Word64
  , upper :: Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON Layer where
  toJSON (Layer lower upper) = toJSON (show @Text lower <> " " <> show @Text upper)

instance FromJSON Layer where
  parseJSON = withText "Layer" $ \t ->
    case words t of
      [lowerStr, upperStr] ->
        case (readMaybe (toString lowerStr), readMaybe (toString upperStr)) of
          (Just lower, Just upper) -> return $ Layer lower upper
          _ -> fail "Invalid Word64 values in Layer"
      _ -> fail "Layer must be two space-separated Word64 values"

-- | External board representation with piece positions
data ExternBoard = ExternBoard
  { black :: Layer
  , white :: Layer
  , king :: Word8
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A move from one position to another
data Move = Move
  { orig :: Word8
  , dest :: Word8
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A move paired with the captures it would produce
data MoveWithCaptures = MoveWithCaptures
  { move :: Move
  , captures :: Layer
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of applying a move, including captures and board state
data MoveResult = MoveResult
  { move :: Move
  , board :: ExternBoard
  , captures :: Layer
  , wasBlackTurn :: Bool
  , zobristHash :: Word64
  }
  deriving (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PlayerColor = White | Black
  deriving (Show, Eq, Generic)

playerColorOptions :: Aeson.Options
playerColorOptions =
  defaultOptions
    { Aeson.constructorTagModifier = map toLower
    , Aeson.allNullaryToStringTag = True
    }

instance ToJSON PlayerColor where toJSON = genericToJSON playerColorOptions
instance FromJSON PlayerColor where
  parseJSON = genericParseJSON playerColorOptions
instance ToSchema PlayerColor where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions playerColorOptions)

opponent :: PlayerColor -> PlayerColor
opponent White = Black
opponent Black = White

-- | A game move with full context including board state
data GameMove = GameMove
  { playerColor :: PlayerColor
  , move :: Move
  , boardStateAfter :: ExternBoard
  , captures :: Layer
  , timestamp :: Time
  }
  deriving (Show, Eq, Generic)

newtype Seconds = Seconds {unSeconds :: Int}
  deriving (Show, Eq, Ord, Generic, Lift)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema)

data TimeControl = TimeControl
  { initialTime :: Refined Positive Seconds
  , increment :: Refined NonNegative Seconds
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema (Refined Positive Seconds) where
  declareNamedSchema _ = do
    schema <- OpenApi._namedSchemaSchema <$> declareNamedSchema (Proxy @Seconds)
    pure $ OpenApi.NamedSchema Nothing $ schema{OpenApi._schemaMinimum = Just 1}

instance ToSchema (Refined NonNegative Seconds) where
  declareNamedSchema _ = do
    schema <- OpenApi._namedSchemaSchema <$> declareNamedSchema (Proxy @Seconds)
    pure $ OpenApi.NamedSchema Nothing $ schema{OpenApi._schemaMinimum = Just 0}

instance ToSchema TimeControl where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions defaultOptions)

-- | Non-negative duration of remaining time. Constructor is hidden;
-- use 'mkRemainingTime', 'deduct', and 'addIncrement'.
newtype RemainingTime = RemainingTime Timespan
  deriving (Show, Eq, Ord)

mkRemainingTime :: Timespan -> Maybe RemainingTime
mkRemainingTime ts
  | getTimespan ts >= 0 = Just (RemainingTime ts)
  | otherwise = Nothing

-- | Subtract elapsed time. Returns Nothing if time expired.
deduct :: Timespan -> RemainingTime -> Maybe RemainingTime
deduct elapsed (RemainingTime r)
  | elapsed > r = Nothing
  | otherwise =
      Just (RemainingTime (Timespan (getTimespan r - getTimespan elapsed)))

-- | Add a non-negative increment to remaining time.
addIncrement :: Refined NonNegative Seconds -> RemainingTime -> RemainingTime
addIncrement inc (RemainingTime r) = RemainingTime (r <> secondsToTimespan inc)

toTimespan :: RemainingTime -> Timespan
toTimespan (RemainingTime ts) = ts

secondsToRemainingTime :: Refined Positive Seconds -> RemainingTime
secondsToRemainingTime = RemainingTime . secondsToTimespan

secondsToTimespan :: Refined p Seconds -> Timespan
secondsToTimespan s =
  Timespan (fromIntegral (unSeconds (unrefine s)) * nanosPerSecond)

nanosPerSecond :: Int64
nanosPerSecond = 1_000_000_000

nanosPerMillisecond :: Int64
nanosPerMillisecond = 1_000_000

-- | Convert remaining time to microseconds.
remainingToMicroseconds :: RemainingTime -> Int
remainingToMicroseconds rt =
  fromIntegral (getTimespan (toTimespan rt) `div` 1000)

-- | Convert remaining time to milliseconds.
remainingToMs :: RemainingTime -> Int
remainingToMs rt =
  fromIntegral (getTimespan (toTimespan rt) `div` nanosPerMillisecond)

-- | Convert an absolute time to Unix milliseconds.
timeToMs :: Time -> Int
timeToMs t = fromIntegral (getTime t `div` nanosPerMillisecond)

data ClockState = ClockState
  { whiteRemaining :: RemainingTime
  , blackRemaining :: RemainingTime
  , turnStartedAt :: Time
  }
  deriving (Show, Eq, Generic)

newtype GameParticipantTokenId = GameParticipantTokenId Text
  deriving (Show, Eq)

data GameParticipantToken = GameParticipantToken
  { tokenId :: GameParticipantTokenId
  , gameId :: GameId
  , token :: Text
  , role :: PlayerColor
  }
  deriving (Show, Eq, Generic)

class DomainMapping a b | a -> b where
  toDomain :: a -> b
  fromDomain :: b -> a
