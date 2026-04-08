{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types (
  -- * Primitives
  Position (..),

  -- * Board
  ApiBoard (..),
  boardFromExtern,
  positionsFromLayer,

  -- * Moves
  ApiMove (..),
  moveFromDomain,
  moveToDomain,

  -- * Game status
  GameEndReason (..),
  ApiGameStatus (..),
  gameStatusFromDomain,
  gameStatusOptions,
  gameEndReasonOptions,

  -- * History
  ApiGameMove (..),

  -- * Responses
  ApiGameState (..),
  ActionResponse (..),

  -- * Utilities
  camelToSnake,
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Aeson qualified as Aeson
import Data.Char (isUpper, toLower)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.OpenApi.SchemaOptions (fromAesonOptions)
import Hnefatafl.Board (layerPositions)
import Hnefatafl.Core.Data (
  ExternBoard (..),
  GameId,
  Layer,
  Move (..),
  MoveWithCaptures (..),
  PlayerColor (..),
  opponent,
 )
import Hnefatafl.Core.Data qualified as Core

-- | Convert CamelCase to snake_case
camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x : xs) = toLower x : go xs
 where
  go [] = []
  go (c : cs)
    | isUpper c = '_' : toLower c : go cs
    | otherwise = c : go cs

newtype Position = Position Int
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving anyclass (ToSchema)

positionsFromLayer :: Layer -> [Position]
positionsFromLayer = map Position . layerPositions

data ApiBoard = ApiBoard
  { black :: [Position]
  , white :: [Position]
  , king :: Position
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

boardFromExtern :: ExternBoard -> ApiBoard
boardFromExtern eb =
  ApiBoard
    { black = positionsFromLayer eb.black
    , white = positionsFromLayer eb.white
    , king = Position (fromIntegral eb.king)
    }

data ApiMove = ApiMove
  { orig :: Position
  , dest :: Position
  , captures :: [Position]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

moveFromDomain :: MoveWithCaptures -> ApiMove
moveFromDomain mc =
  ApiMove
    { orig = Position (fromIntegral mc.move.orig)
    , dest = Position (fromIntegral mc.move.dest)
    , captures = positionsFromLayer mc.captures
    }

moveToDomain :: ApiMove -> Move
moveToDomain m =
  let Position o = m.orig
      Position d = m.dest
   in Move (fromIntegral o) (fromIntegral d)

-------------------------------------------------------------------------------
-- Game end reason

data GameEndReason
  = KingCaptured
  | WhiteSurrounded
  | NoMoves
  | KingEscaped
  | ExitFort
  | Resignation
  | Timeout
  | Draw
  | Abandoned
  deriving (Show, Eq, Generic)

gameEndReasonOptions :: Aeson.Options
gameEndReasonOptions =
  defaultOptions
    { Aeson.constructorTagModifier = camelToSnake
    , Aeson.allNullaryToStringTag = True
    }

instance ToJSON GameEndReason where toJSON = genericToJSON gameEndReasonOptions
instance FromJSON GameEndReason where parseJSON = genericParseJSON gameEndReasonOptions
instance ToSchema GameEndReason where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions gameEndReasonOptions)

-------------------------------------------------------------------------------
-- Game status

data ApiGameStatus
  = Ongoing
  | Finished {_winner :: Maybe PlayerColor, _reason :: GameEndReason}
  deriving (Show, Eq, Generic)

gameStatusOptions :: Aeson.Options
gameStatusOptions =
  defaultOptions
    { Aeson.sumEncoding = Aeson.TaggedObject "state" "contents"
    , Aeson.constructorTagModifier = map toLower
    , Aeson.fieldLabelModifier = drop 1 -- strip leading underscore
    }

instance ToJSON ApiGameStatus where toJSON = genericToJSON gameStatusOptions
instance FromJSON ApiGameStatus where parseJSON = genericParseJSON gameStatusOptions
instance ToSchema ApiGameStatus where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions gameStatusOptions)

-------------------------------------------------------------------------------
-- Domain mapping

gameStatusFromDomain :: Maybe Core.Outcome -> ApiGameStatus
gameStatusFromDomain = \case
  Nothing -> Ongoing
  Just (Core.BlackWins Core.KingCaptured) -> Finished{_winner = Just Black, _reason = KingCaptured}
  Just (Core.BlackWins Core.WhiteSurrounded) -> Finished{_winner = Just Black, _reason = WhiteSurrounded}
  Just (Core.BlackWins Core.NoWhiteMoves) -> Finished{_winner = Just Black, _reason = NoMoves}
  Just (Core.WhiteWins Core.KingEscaped) -> Finished{_winner = Just White, _reason = KingEscaped}
  Just (Core.WhiteWins Core.ExitFort) -> Finished{_winner = Just White, _reason = ExitFort}
  Just (Core.WhiteWins Core.NoBlackMoves) -> Finished{_winner = Just White, _reason = NoMoves}
  Just (Core.ResignedBy color) -> Finished{_winner = Just (opponent color), _reason = Resignation}
  Just (Core.TimedOut color) -> Finished{_winner = Just (opponent color), _reason = Timeout}
  Just Core.Draw -> Finished{_winner = Nothing, _reason = Draw}
  Just Core.Abandoned -> Finished{_winner = Nothing, _reason = Abandoned}

-------------------------------------------------------------------------------
-- History and responses

data ApiGameMove = ApiGameMove
  { playerColor :: PlayerColor
  , move :: ApiMove
  , captures :: [Position]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ApiGameState = ApiGameState
  { gameId :: GameId
  , board :: ApiBoard
  , turn :: PlayerColor
  , status :: ApiGameStatus
  , history :: [ApiGameMove]
  , validMoves :: [ApiMove]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ActionResponse = ActionResponse
  { turn :: PlayerColor
  , status :: ApiGameStatus
  , validMoves :: [ApiMove]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
