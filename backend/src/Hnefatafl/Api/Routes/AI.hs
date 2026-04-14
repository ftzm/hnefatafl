{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Api.Routes.AI (
  AIRoutes (..),
) where

import Hnefatafl.Api.Types.AI (CreateAiGameResponse, CreateGameRequest)
import Hnefatafl.Servant.WebSocket (WebSocket)
import Servant (
  GenericMode (type (:-)),
  JSON,
  Post,
  ReqBody,
 )
import Servant.API ((:>))

data AIRoutes mode = AIRoutes
  { create ::
      mode :- ReqBody '[JSON] CreateGameRequest :> Post '[JSON] CreateAiGameResponse
  , ws :: mode :- "ws" :> WebSocket
  }
  deriving stock (Generic)
