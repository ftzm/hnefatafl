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
  { lower :: Int64
  , upper :: Int64
  }
  deriving (Show, Read, Eq)

-- | External board representation with piece positions
data ExternBoard = ExternBoard
  { black :: Layer
  , white :: Layer
  , king :: Word8
  }
  deriving (Show, Read, Eq)

-- | A move from one position to another
data Move = Move
  { orig :: Word8
  , dest :: Word8
  }
  deriving (Show, Read, Eq, Generic)

-- | Result of applying a move, including captures and board state
data MoveResult = MoveResult
  { move :: Move
  , board :: ExternBoard
  , captures :: Layer
  , wasBlackTurn :: Bool
  }
  deriving (Show, Read, Eq)

data PlayerColor = White | Black
  deriving (Show, Eq)

-- | A game move with full context including board state
data GameMove = GameMove
  { playerColor :: PlayerColor
  , move :: Move
  , boardStateAfter :: ExternBoard
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
