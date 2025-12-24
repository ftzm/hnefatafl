{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Server (
  HnefataflAPI,
  Routes (..),
  VersionResponse (..),
  HealthResponse (..),
  SearchTrustedInput (..),
  server,
  runServer,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, runEff)
import Effectful qualified as E
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.QSem (newQSem)
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runWarpServerSettings)
import Hnefatafl.Bindings (SearchTrustedResult)
import Hnefatafl.Core.Data (ExternBoard)
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Interpreter.Search.Local (runSearchLocal)
import Hnefatafl.Search (SearchTimeout)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Servant (
  GenericMode (type (:-)),
  Get,
  JSON,
  Post,
  ReqBody,
  ServerError,
  ToServantApi,
 )
import Servant.API ((:>))
import Servant.Server.Generic (AsServerT, genericServerT)

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

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] VersionResponse
  , health :: mode :- "health" :> Get '[JSON] HealthResponse
  , searchTrusted :: mode :- "searchTrusted" :> ReqBody '[JSON] SearchTrustedInput :> Post '[JSON] SearchTrustedResult
  }
  deriving stock (Generic)

type HnefataflAPI = ToServantApi Routes

server :: (Search E.:> es, IOE E.:> es) => Routes (AsServerT (Eff es))
server =
  Routes
    { version = versionHandler
    , health = healthHandler
    , searchTrusted = searchTrustedHandler
    }

versionHandler :: IOE E.:> es => Eff es VersionResponse
versionHandler =
  pure $
    VersionResponse
      { versionNumber = "0.0.0.2"
      , buildDate = "2025-12-21"
      }

healthHandler :: IOE E.:> es => Eff es HealthResponse
healthHandler = do
  currentTime <- liftIO getCurrentTime
  pure $
    HealthResponse
      { status = "OK"
      , timestamp = show currentTime
      }

data SearchTrustedInput = SearchTrustedInput
  { board :: ExternBoard
  , blackToMove :: Bool
  , hashes :: [Word64]
  , timeout :: SearchTimeout
  }
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

searchTrustedHandler ::
  (Search E.:> es, IOE E.:> es) =>
  SearchTrustedInput -> Eff es SearchTrustedResult
searchTrustedHandler input =
  send $ SearchTrusted input.board input.blackToMove input.hashes input.timeout

runServer :: Int -> IO ()
runServer port = do
  putTextLn $ "Starting Hnefatafl server on port " <> show port
  let settings = defaultSettings & setPort port
  result <-
    runEff $
      runErrorNoCallStack @ServerError $
        runConcurrent $
          do
            qsem <- newQSem 4  -- Allow 4 concurrent searches
            runSearchLocal qsem $
              runWarpServerSettings @HnefataflAPI settings (genericServerT server) id
  case result of
    Left err -> putTextLn $ "Server error: " <> show err
    Right () -> pure ()
