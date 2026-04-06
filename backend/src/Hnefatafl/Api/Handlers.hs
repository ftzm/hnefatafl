module Hnefatafl.Api.Handlers (
  server,
) where

import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error)
import Hnefatafl.Api.Handlers.AI (aiServer)
import Hnefatafl.Api.Handlers.Hotseat (hotseatServer)
import Hnefatafl.Api.Handlers.Online (onlineServer)
import Hnefatafl.Api.Routes (
  HealthResponse (..),
  Routes (..),
  SearchTrustedInput (..),
  VersionResponse (..),
 )
import Hnefatafl.App.AI qualified as AI
import Hnefatafl.App.Online qualified as Online
import Hnefatafl.Bindings (SearchTrustedResult)
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Log (KatipE)
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Servant (ServerError)
import Servant.Server.Generic (AsServerT)
import Version qualified

server ::
  ( Search :> es
  , Storage :> es
  , Clock :> es
  , IdGen :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  Online.GameSessions ->
  AI.GameSessions ->
  Routes (AsServerT (Eff es))
server onlineSessions aiSessions =
  Routes
    { version = versionHandler
    , health = healthHandler
    , searchTrusted = searchTrustedHandler
    , hotseat = hotseatServer
    , online = onlineServer onlineSessions
    , ai = aiServer aiSessions
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
