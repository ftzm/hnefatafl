{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Api.Routes (
  Routes (..),
  HnefataflAPI,
  VersionResponse (..),
  HealthResponse (..),
  SearchTrustedInput (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Hnefatafl.Api.Routes.AI (AIRoutes)
import Hnefatafl.Api.Routes.Hotseat (HotseatRoutes)
import Hnefatafl.Api.Routes.Online (OnlineRoutes)
import Hnefatafl.Bindings (SearchTrustedResult)
import Hnefatafl.Core.Data (ExternBoard)
import Hnefatafl.Search (SearchTimeout)
import Servant (
  GenericMode (type (:-)),
  Get,
  JSON,
  NamedRoutes,
  Post,
  ReqBody,
  ToServantApi,
 )
import Servant.API ((:>))

data VersionResponse = VersionResponse
  { versionNumber :: Text
  , buildDate :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data HealthResponse = HealthResponse
  { status :: Text
  , timestamp :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data SearchTrustedInput = SearchTrustedInput
  { board :: ExternBoard
  , blackToMove :: Bool
  , hashes :: [Word64]
  , timeout :: SearchTimeout
  , enableAdminEndings :: Bool
  }
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] VersionResponse
  , health :: mode :- "health" :> Get '[JSON] HealthResponse
  , searchTrusted ::
      mode
        :- "searchTrusted"
          :> ReqBody '[JSON] SearchTrustedInput
          :> Post '[JSON] SearchTrustedResult
  , hotseat :: mode :- "hotseat" :> NamedRoutes HotseatRoutes
  , online :: mode :- "online" :> NamedRoutes OnlineRoutes
  , ai :: mode :- "ai" :> NamedRoutes AIRoutes
  }
  deriving stock (Generic)

type HnefataflAPI = ToServantApi Routes
