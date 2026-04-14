module Hnefatafl.Api.Handlers.Online (
  onlineServer,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Katip (KatipE, katipAddNamespace)
import Hnefatafl.Api.Routes.Online (OnlineRoutes (..))
import Hnefatafl.Api.Types.Online (
  CreateOnlineGameResponse (..),
 )
import Hnefatafl.App.Online qualified as Online
import Hnefatafl.Core.Data (
  Game (..),
  GameParticipantToken (..),
 )
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Hnefatafl.Metrics (HMetrics)
import Network.WebSockets (Connection)
import Servant (ServerError)
import Servant.Server.Generic (AsServerT)

onlineServer ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , Error ServerError :> es
  , HMetrics :> es
  , IOE :> es
  ) =>
  Online.GameSessions ->
  OnlineRoutes (AsServerT (Eff es))
onlineServer sessions =
  OnlineRoutes
    { create = createHandler
    , ws = \conn -> handleWebSocket sessions conn
    }

createHandler ::
  (Storage :> es, Clock :> es, IdGen :> es, KatipE :> es, Trace :> es, HMetrics :> es) =>
  Eff es CreateOnlineGameResponse
createHandler = katipAddNamespace "online" $ do
  result <- Online.createGame
  pure
    CreateOnlineGameResponse
      { gameId = result.game.gameId
      , whiteToken = result.whiteToken.token
      , blackToken = result.blackToken.token
      }

handleWebSocket ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  , IOE :> es
  ) =>
  Online.GameSessions ->
  Connection ->
  Eff es ()
handleWebSocket sessions conn =
  Online.handleWebSocket sessions conn
