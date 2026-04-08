{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.AI (
  CreateGameRequest (..),
  CreateGameResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hnefatafl.Core.Data (
  GameId,
  PlayerColor,
 )

data CreateGameRequest = CreateGameRequest
  { humanColor :: PlayerColor
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateGameResponse = CreateGameResponse
  { gameId :: GameId
  , token :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
