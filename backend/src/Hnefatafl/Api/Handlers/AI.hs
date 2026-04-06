module Hnefatafl.Api.Handlers.AI (
  aiServer,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
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
import Hnefatafl.Effect.WebSocket (WebSocket)
import Servant (ServerError)
import Servant.Server.Generic (AsServerT)

aiServer ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Search :> es
  , Concurrent :> es
  , WebSocket :> es
  , Error ServerError :> es
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
  (Storage :> es, Clock :> es, IdGen :> es) =>
  CreateGameRequest -> Eff es CreateGameResponse
createHandler req = do
  result <- AI.createGame req.humanColor
  pure
    CreateGameResponse
      { gameId = result.game.gameId
      , token = result.token.token
      }
