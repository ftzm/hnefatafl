module Hnefatafl.Api.Handlers (
  server,
) where

import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error)
import Hnefatafl.Api.Handlers.Hotseat (hotseatServer)
import Hnefatafl.Api.Routes (
  HealthResponse (..),
  Routes (..),
  SearchTrustedInput (..),
  VersionResponse (..),
 )
import Hnefatafl.Bindings (SearchTrustedResult)
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Effect.Storage (Storage)
import Servant (ServerError)
import Servant.Server.Generic (AsServerT)
import Version qualified

server ::
  ( Search :> es
  , Storage :> es
  , Clock :> es
  , IdGen :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  Routes (AsServerT (Eff es))
server =
  Routes
    { version = versionHandler
    , health = healthHandler
    , searchTrusted = searchTrustedHandler
    , hotseat = hotseatServer
    }

versionHandler :: IOE :> es => Eff es VersionResponse
versionHandler =
  pure $
    VersionResponse
      { versionNumber = Version.version
      , buildDate = Version.buildDate
      }

healthHandler :: IOE :> es => Eff es HealthResponse
healthHandler = do
  currentTime <- liftIO getCurrentTime
  pure $
    HealthResponse
      { status = "OK"
      , timestamp = show currentTime
      }

searchTrustedHandler ::
  (Search :> es, IOE :> es) =>
  SearchTrustedInput -> Eff es SearchTrustedResult
searchTrustedHandler input =
  send $
    SearchTrusted
      input.board
      input.blackToMove
      input.hashes
      input.timeout
      input.enableAdminEndings
