{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Api.Types.Online (
  CreateOnlineGameResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hnefatafl.Core.Data (
  GameId,
 )

data CreateOnlineGameResponse = CreateOnlineGameResponse
  { gameId :: GameId
  , whiteToken :: Text
  , blackToken :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
