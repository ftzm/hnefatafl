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
  GameStatus (..),
  Game (..),

  -- * Board and Move Types
  Layer (..),
  ExternBoard (..),
  Move (..),
  MoveResult (..),
  PlayerColor (..),
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

data GameStatus
  = Ongoing
  | -- Black victory conditions
    BlackWonKingCaptured
  | BlackWonWhiteSurrounded
  | BlackWonNoWhiteMoves
  | BlackWonResignation
  | BlackWonTimeout
  | -- White victory conditions
    WhiteWonKingEscaped
  | WhiteWonExitFort
  | WhiteWonNoBlackMoves
  | WhiteWonResignation
  | WhiteWonTimeout
  | -- Other outcomes
    Draw
  | Abandoned
  deriving (Show, Eq)

data Game = Game
  { gameId :: GameId
  , name :: Maybe Text
  , whitePlayerId :: Maybe PlayerId -- Nothing for anonymous players
  , blackPlayerId :: Maybe PlayerId -- Nothing for anonymous players
  , startTime :: Time
  , endTime :: Maybe Time
  , gameStatus :: GameStatus
  , createdAt :: Time
  }
  deriving (Show, Eq, Generic)

-- | Represents a layer of the board using bit manipulation
data Layer = Layer
  { lower :: Word64
  , upper :: Word64
  }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A move from one position to another
data Move = Move
  { orig :: Word8
  , dest :: Word8
  }
  deriving (Show, Read, Eq, Generic)
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
