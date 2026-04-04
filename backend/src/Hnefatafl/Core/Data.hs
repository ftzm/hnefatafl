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

  -- * Game Participant Token Types
  GameParticipantTokenId (..),
  GameParticipantToken (..),

  -- * Utilities
  DomainMapping (..),
) where

import Chronos (Time)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  withText,
 )
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
  deriving (Show, Eq)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Hashable)

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
  deriving (Show, Eq)

instance ToJSON PlayerColor where
  toJSON White = toJSON ("white" :: Text)
  toJSON Black = toJSON ("black" :: Text)

instance FromJSON PlayerColor where
  parseJSON = withText "PlayerColor" $ \case
    "white" -> pure White
    "black" -> pure Black
    _ -> fail "expected \"white\" or \"black\""

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
