{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.AI (
  CreateGameRequest (..),
  CreateGameResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Hnefatafl.Core.Data (
  GameId,
  PlayerColor,
 )

data CreateGameRequest = CreateGameRequest
  { humanColor :: PlayerColor
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data CreateGameResponse = CreateGameResponse
  { gameId :: GameId
  , token :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
