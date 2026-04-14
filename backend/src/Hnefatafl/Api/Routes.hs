{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Api.Routes (
  Routes (..),
  HnefataflAPI,
  DocumentedAPI,
  VersionResponse (..),
  HealthResponse (..),
  SearchTrustedInput (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (OpenApi, ToSchema)
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
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data HealthResponse = HealthResponse
  { status :: Text
  , timestamp :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
  , openapi :: mode :- "openapi.json" :> Get '[JSON] OpenApi
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

-- | Full API type (served)
type HnefataflAPI = ToServantApi Routes

-- | API without the /openapi.json endpoint (used for spec generation,
-- since OpenApi can't describe itself)
data DocumentedRoutes mode = DocumentedRoutes
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

type DocumentedAPI = ToServantApi DocumentedRoutes
