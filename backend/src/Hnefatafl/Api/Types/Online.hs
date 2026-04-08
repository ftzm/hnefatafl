{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.Online (
  CreateGameResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hnefatafl.Core.Data (
  GameId,
 )

data CreateGameResponse = CreateGameResponse
  { gameId :: GameId
  , whiteToken :: Text
  , blackToken :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
