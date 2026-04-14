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
  MoveDestination (..),
  ValidMovesMap (..),
  validMovesMapFromDomain,

  -- * Game status
  GameEndReason (..),
  GameWinner (..),
  ApiGameStatus (..),
  gameStatusFromDomain,
  gameEndReasonOptions,

  -- * History
  HistoryEntry (..),
  historyEntryFromDomain,

  -- * Responses
  ApiGameState (..),
  ActionResponse (..),

  -- * Utilities
  camelToSnake,
  lcFirst,
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
  object,
  withObject,
  (.=),
 )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isUpper, toLower)
import Data.Map.Strict qualified as Map
import Data.OpenApi (
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  ToSchema (..),
  declareSchemaRef,
  genericDeclareNamedSchema,
 )
import Data.OpenApi qualified as OpenApi
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

-- | Lowercase the first character of a string
lcFirst :: String -> String
lcFirst [] = []
lcFirst (x : xs) = toLower x : xs

newtype Position = Position Int
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema Position where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Position") $
        mempty
          { OpenApi._schemaType = Just OpenApiInteger
          , OpenApi._schemaMinimum = Just 0
          , OpenApi._schemaMaximum = Just 120
          }

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
  { moveFrom :: Position
  , moveTo :: Position
  , captures :: [Position]
  }
  deriving (Show, Eq, Generic)

apiMoveOptions :: Aeson.Options
apiMoveOptions =
  defaultOptions
    { Aeson.fieldLabelModifier = \case
        "moveFrom" -> "from"
        "moveTo" -> "to"
        other -> other
    }

instance ToJSON ApiMove where toJSON = genericToJSON apiMoveOptions
instance FromJSON ApiMove where parseJSON = genericParseJSON apiMoveOptions
instance ToSchema ApiMove where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions apiMoveOptions)

moveFromDomain :: MoveWithCaptures -> ApiMove
moveFromDomain mc =
  ApiMove
    { moveFrom = Position (fromIntegral mc.move.orig)
    , moveTo = Position (fromIntegral mc.move.dest)
    , captures = positionsFromLayer mc.captures
    }

moveToDomain :: ApiMove -> Move
moveToDomain m =
  let Position o = m.moveFrom
      Position d = m.moveTo
   in Move (fromIntegral o) (fromIntegral d)

-------------------------------------------------------------------------------
-- Valid moves map

-- | A destination entry in the valid moves map.
data MoveDestination = MoveDestination
  { to :: Position
  , captures :: [Position]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Valid moves grouped by origin position. Serializes as a JSON object
-- keyed by origin position (as string), with values as arrays of
-- @{to, captures}@ objects.
newtype ValidMovesMap = ValidMovesMap (Map.Map Position [MoveDestination])
  deriving (Show, Eq)

instance ToJSON ValidMovesMap where
  toJSON (ValidMovesMap m) =
    object
      [ Key.fromString (show p) .= dests
      | (Position p, dests) <- Map.toList m
      ]

instance FromJSON ValidMovesMap where
  parseJSON = withObject "ValidMovesMap" $ \obj -> do
    pairs <- forM (KM.toList obj) $ \(k, v) -> do
      pos <- case readMaybe (Key.toString k) of
        Just n -> pure (Position n)
        Nothing -> fail $ "Invalid position key: " <> Key.toString k
      dests <- parseJSON v
      pure (pos, dests)
    pure $ ValidMovesMap (Map.fromList pairs)

instance ToSchema ValidMovesMap where
  declareNamedSchema _ = do
    destRef <- declareSchemaRef (Proxy @[MoveDestination])
    pure $
      NamedSchema (Just "ValidMovesMap") $
        mempty
          { OpenApi._schemaType = Just OpenApiObject
          , OpenApi._schemaDescription =
              Just "Valid moves keyed by origin position"
          , OpenApi._schemaAdditionalProperties =
              Just (OpenApi.AdditionalPropertiesSchema destRef)
          }

validMovesMapFromDomain :: [MoveWithCaptures] -> ValidMovesMap
validMovesMapFromDomain moves =
  ValidMovesMap $
    Map.fromListWith
      (<>)
      [ ( Position (fromIntegral mc.move.orig)
        , [ MoveDestination
              { to = Position (fromIntegral mc.move.dest)
              , captures = positionsFromLayer mc.captures
              }
          ]
        )
      | mc <- moves
      ]

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
-- Game winner

-- | The winner of a finished game: a player color or a draw.
data GameWinner = PlayerWinner PlayerColor | DrawResult
  deriving (Show, Eq)

instance ToJSON GameWinner where
  toJSON (PlayerWinner White) = toJSON ("white" :: Text)
  toJSON (PlayerWinner Black) = toJSON ("black" :: Text)
  toJSON DrawResult = toJSON ("draw" :: Text)

instance FromJSON GameWinner where
  parseJSON = Aeson.withText "GameWinner" $ \case
    "white" -> pure (PlayerWinner White)
    "black" -> pure (PlayerWinner Black)
    "draw" -> pure DrawResult
    other -> fail $ "Unknown winner: " <> toString other

instance ToSchema GameWinner where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "GameWinner") $
        mempty
          { OpenApi._schemaType = Just OpenApiString
          , OpenApi._schemaEnum = Just [toJSON ("white" :: Text), toJSON ("black" :: Text), toJSON ("draw" :: Text)]
          }

-------------------------------------------------------------------------------
-- Game status

data ApiGameStatus
  = Ongoing
  | Finished GameWinner GameEndReason
  deriving (Show, Eq)

instance ToJSON ApiGameStatus where
  toJSON Ongoing = object ["state" .= ("ongoing" :: Text)]
  toJSON (Finished w r) =
    object
      [ "state" .= ("finished" :: Text)
      , "winner" .= w
      , "reason" .= r
      ]

instance FromJSON ApiGameStatus where
  parseJSON = withObject "ApiGameStatus" $ \o -> do
    state' <- o Aeson..: "state" :: Parser Text
    case state' of
      "ongoing" -> pure Ongoing
      "finished" -> Finished <$> o Aeson..: "winner" <*> o Aeson..: "reason"
      other -> fail $ "Unknown state: " <> toString other

instance ToSchema ApiGameStatus where
  declareNamedSchema _ = do
    winnerRef <- declareSchemaRef (Proxy @GameWinner)
    reasonRef <- declareSchemaRef (Proxy @GameEndReason)
    let stringSchema = mempty{OpenApi._schemaType = Just OpenApiString}
        ongoingSchema =
          mempty
            { OpenApi._schemaType = Just OpenApiObject
            , OpenApi._schemaRequired = ["state"]
            , OpenApi._schemaProperties =
                fromList
                  [ ( "state"
                    , Inline $
                        stringSchema{OpenApi._schemaEnum = Just [toJSON ("ongoing" :: Text)]}
                    )
                  ]
            , OpenApi._schemaAdditionalProperties =
                Just (OpenApi.AdditionalPropertiesAllowed False)
            }
        finishedSchema =
          mempty
            { OpenApi._schemaType = Just OpenApiObject
            , OpenApi._schemaRequired = ["state", "winner", "reason"]
            , OpenApi._schemaProperties =
                fromList
                  [ ( "state"
                    , Inline $
                        stringSchema{OpenApi._schemaEnum = Just [toJSON ("finished" :: Text)]}
                    )
                  , ("winner", winnerRef)
                  , ("reason", reasonRef)
                  ]
            , OpenApi._schemaAdditionalProperties =
                Just (OpenApi.AdditionalPropertiesAllowed False)
            }
    pure $
      NamedSchema (Just "ApiGameStatus") $
        mempty
          { OpenApi._schemaOneOf = Just [Inline ongoingSchema, Inline finishedSchema]
          }

-------------------------------------------------------------------------------
-- Domain mapping

gameStatusFromDomain :: Maybe Core.Outcome -> ApiGameStatus
gameStatusFromDomain = \case
  Nothing -> Ongoing
  Just (Core.BlackWins Core.KingCaptured) -> Finished (PlayerWinner Black) KingCaptured
  Just (Core.BlackWins Core.WhiteSurrounded) -> Finished (PlayerWinner Black) WhiteSurrounded
  Just (Core.BlackWins Core.NoWhiteMoves) -> Finished (PlayerWinner Black) NoMoves
  Just (Core.WhiteWins Core.KingEscaped) -> Finished (PlayerWinner White) KingEscaped
  Just (Core.WhiteWins Core.ExitFort) -> Finished (PlayerWinner White) ExitFort
  Just (Core.WhiteWins Core.NoBlackMoves) -> Finished (PlayerWinner White) NoMoves
  Just (Core.ResignedBy color) -> Finished (PlayerWinner (opponent color)) Resignation
  Just (Core.TimedOut color) -> Finished (PlayerWinner (opponent color)) Timeout
  Just Core.Draw -> Finished DrawResult Draw
  Just Core.Abandoned -> Finished DrawResult Abandoned

-------------------------------------------------------------------------------
-- History and responses

data HistoryEntry = HistoryEntry
  { color :: PlayerColor
  , move :: ApiMove
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

historyEntryFromDomain :: MoveWithCaptures -> PlayerColor -> HistoryEntry
historyEntryFromDomain mc side =
  HistoryEntry
    { color = side
    , move = moveFromDomain mc
    }

data ApiGameState = ApiGameState
  { gameId :: GameId
  , board :: ApiBoard
  , turn :: PlayerColor
  , status :: ApiGameStatus
  , history :: [HistoryEntry]
  , validMoves :: ValidMovesMap
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ActionResponse = ActionResponse
  { turn :: PlayerColor
  , status :: ApiGameStatus
  , validMoves :: ValidMovesMap
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
