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

  -- * History
  ApiGameMove (..),

  -- * Responses
  ApiGameState (..),
  ActionResponse (..),
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  object,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Hnefatafl.Board (layerPositions)
import Hnefatafl.Core.Data (
  ExternBoard (..),
  GameId,
  GameStatus (..),
  Layer,
  Move (..),
  MoveWithCaptures (..),
  PlayerColor (..),
 )

newtype Position = Position Int
  deriving (Show, Eq)
  deriving newtype (ToJSON, FromJSON)

positionsFromLayer :: Layer -> [Position]
positionsFromLayer = map Position . layerPositions

data ApiBoard = ApiBoard
  { black :: [Position]
  , white :: [Position]
  , king :: Position
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON)

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

data GameEndReason
  = KingCaptured
  | WhiteSurrounded
  | NoMoves
  | KingEscaped
  | ExitFort
  | Resignation
  | Timeout
  | DrawAgreed
  | GameAbandoned
  deriving (Show, Eq)

instance ToJSON GameEndReason where
  toJSON = \case
    KingCaptured -> toJSON ("king_captured" :: Text)
    WhiteSurrounded -> toJSON ("white_surrounded" :: Text)
    NoMoves -> toJSON ("no_moves" :: Text)
    KingEscaped -> toJSON ("king_escaped" :: Text)
    ExitFort -> toJSON ("exit_fort" :: Text)
    Resignation -> toJSON ("resignation" :: Text)
    Timeout -> toJSON ("timeout" :: Text)
    DrawAgreed -> toJSON ("draw" :: Text)
    GameAbandoned -> toJSON ("abandoned" :: Text)

instance FromJSON GameEndReason where
  parseJSON = withText "GameEndReason" $ \case
    "king_captured" -> pure KingCaptured
    "white_surrounded" -> pure WhiteSurrounded
    "no_moves" -> pure NoMoves
    "king_escaped" -> pure KingEscaped
    "exit_fort" -> pure ExitFort
    "resignation" -> pure Resignation
    "timeout" -> pure Timeout
    "draw" -> pure DrawAgreed
    "abandoned" -> pure GameAbandoned
    _ -> fail "invalid game end reason"

data ApiGameStatus
  = ApiOngoing
  | ApiFinished (Maybe PlayerColor) GameEndReason
  deriving (Show, Eq)

instance ToJSON ApiGameStatus where
  toJSON ApiOngoing = object ["state" .= ("ongoing" :: Text)]
  toJSON (ApiFinished winner reason) =
    object
      [ "state" .= ("finished" :: Text)
      , "winner" .= winner
      , "reason" .= reason
      ]

instance FromJSON ApiGameStatus where
  parseJSON = withObject "ApiGameStatus" $ \o -> do
    st <- o .: "state"
    case st :: Text of
      "ongoing" -> pure ApiOngoing
      "finished" -> ApiFinished <$> o .: "winner" <*> o .: "reason"
      _ -> fail "invalid game status state"

gameStatusFromDomain :: GameStatus -> ApiGameStatus
gameStatusFromDomain = \case
  Ongoing -> ApiOngoing
  BlackWonKingCaptured -> ApiFinished (Just Black) KingCaptured
  BlackWonWhiteSurrounded -> ApiFinished (Just Black) WhiteSurrounded
  BlackWonNoWhiteMoves -> ApiFinished (Just Black) NoMoves
  BlackWonResignation -> ApiFinished (Just Black) Resignation
  BlackWonTimeout -> ApiFinished (Just Black) Timeout
  WhiteWonKingEscaped -> ApiFinished (Just White) KingEscaped
  WhiteWonExitFort -> ApiFinished (Just White) ExitFort
  WhiteWonNoBlackMoves -> ApiFinished (Just White) NoMoves
  WhiteWonResignation -> ApiFinished (Just White) Resignation
  WhiteWonTimeout -> ApiFinished (Just White) Timeout
  Draw -> ApiFinished Nothing DrawAgreed
  Abandoned -> ApiFinished Nothing GameAbandoned

data ApiGameMove = ApiGameMove
  { playerColor :: PlayerColor
  , move :: ApiMove
  , captures :: [Position]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ApiGameState = ApiGameState
  { gameId :: GameId
  , board :: ApiBoard
  , turn :: PlayerColor
  , status :: ApiGameStatus
  , history :: [ApiGameMove]
  , validMoves :: [ApiMove]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActionResponse = ActionResponse
  { turn :: PlayerColor
  , status :: ApiGameStatus
  , validMoves :: [ApiMove]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
