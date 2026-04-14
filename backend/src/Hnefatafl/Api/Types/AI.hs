{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.AI (
  CreateGameRequest (..),
  CreateAiGameResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hnefatafl.Core.Data (
  GameId,
  PlayerColor,
 )

data CreateGameRequest = CreateGameRequest
  { playerColor :: PlayerColor
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateAiGameResponse = CreateAiGameResponse
  { gameId :: GameId
  , token :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
