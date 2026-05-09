{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.Online (
  CreateOnlineGameRequest (..),
  CreateOnlineGameResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hnefatafl.Core.Data (
  GameId,
  TimeControl,
 )

data CreateOnlineGameRequest = CreateOnlineGameRequest
  { timeControl :: Maybe TimeControl
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateOnlineGameResponse = CreateOnlineGameResponse
  { gameId :: GameId
  , whiteToken :: Text
  , blackToken :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
