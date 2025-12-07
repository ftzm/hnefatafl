{-# LANGUAGE FunctionalDependencies #-}

module Hnefatafl.Core.Data (
  -- * Player Types
  PlayerId (..),
  generatePlayerId,
  HumanPlayer (..),
  EnginePlayer (..),
  Player (..),

  -- * Game Types
  GameId (..),
  generateGameId,
  GameStatus (..),
  Game (..),

  -- * Board and Move Types
  Layer (..),
  ExternBoard (..),
  Move (..),
  MoveResult (..),
  MoveId (..),
  generateMoveId,
  PlayerColor (..),
  GameMove (..),

  -- * Game Participant Token Types
  GameParticipantTokenId (..),
  generateGameParticipantTokenId,
  GameParticipantToken (..),

  -- * Utilities
  DomainMapping (..),
) where

import Data.Time (UTCTime)
import Data.ULID

newtype PlayerId = PlayerId Text
  deriving (Show, Eq)

-- | Generate a new PlayerId using ULID
generatePlayerId :: IO PlayerId
generatePlayerId = PlayerId . show <$> getULID

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

-- | Generate a new GameId using ULID
generateGameId :: IO GameId
generateGameId = GameId . show <$> getULID

data GameStatus
  = Ongoing
  -- Black victory conditions
  | BlackWonKingCaptured
  | BlackWonWhiteSurrounded
  | BlackWonNoWhiteMoves
  | BlackWonResignation
  | BlackWonTimeout
  -- White victory conditions
  | WhiteWonKingEscaped
  | WhiteWonExitFort
  | WhiteWonNoBlackMoves
  | WhiteWonResignation
  | WhiteWonTimeout
  -- Other outcomes
  | Draw
  | Abandoned
  deriving (Show, Eq)

data Game = Game
  { gameId :: GameId
  , name :: Maybe Text
  , whitePlayerId :: Maybe PlayerId -- Nothing for anonymous players
  , blackPlayerId :: Maybe PlayerId -- Nothing for anonymous players
  , startTime :: UTCTime
  , endTime :: Maybe UTCTime
  , gameStatus :: GameStatus
  , createdAt :: UTCTime
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
  deriving (Show, Read, Eq)

-- | Result of applying a move, including captures and board state
data MoveResult = MoveResult
  { move :: Move
  , board :: ExternBoard
  , captures :: Layer
  , wasBlackTurn :: Bool
  }
  deriving (Show, Read, Eq)

newtype MoveId = MoveId Text
  deriving (Show, Eq)

-- | Generate a new MoveId using ULID
generateMoveId :: IO MoveId
generateMoveId = MoveId . show <$> getULID

data PlayerColor = White | Black
  deriving (Show, Eq)

-- | A game move with full context including board state
data GameMove = GameMove
  { moveId :: MoveId
  , moveNumber :: Int
  , playerColor :: PlayerColor
  , move :: Move
  , boardStateAfter :: ExternBoard
  , timestamp :: UTCTime
  }
  deriving (Show, Eq, Generic)

newtype GameParticipantTokenId = GameParticipantTokenId Text
  deriving (Show, Eq)

-- | Generate a new GameParticipantTokenId using ULID
generateGameParticipantTokenId :: IO GameParticipantTokenId
generateGameParticipantTokenId = GameParticipantTokenId . show <$> getULID

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
