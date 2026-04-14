module Hnefatafl.Api.Handlers.AI (
  aiServer,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Katip (KatipE, katipAddNamespace)
import Hnefatafl.Api.Routes.AI (AIRoutes (..))
import Hnefatafl.Api.Types.AI (
  CreateGameRequest (..),
  CreateGameResponse (..),
 )
import Hnefatafl.App.AI qualified as AI
import Hnefatafl.Core.Data (
  Game (..),
  GameParticipantToken (..),
 )
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Search (Search)
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Hnefatafl.Metrics (HMetrics)
import Servant (ServerError)
import Servant.Server.Generic (AsServerT)

aiServer ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Search :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , Error ServerError :> es
  , HMetrics :> es
  , IOE :> es
  ) =>
  AI.GameSessions ->
  AIRoutes (AsServerT (Eff es))
aiServer sessions =
  AIRoutes
    { create = createHandler
    , ws = \conn -> AI.handleWebSocket sessions conn
    }

createHandler ::
  (Storage :> es, Clock :> es, IdGen :> es, KatipE :> es, Trace :> es, HMetrics :> es) =>
  CreateGameRequest -> Eff es CreateGameResponse
createHandler req = katipAddNamespace "ai" $ do
  result <- AI.createGame req.playerColor
  pure
    CreateGameResponse
      { gameId = result.game.gameId
      , token = result.token.token
      }
