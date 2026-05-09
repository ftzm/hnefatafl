{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Api.Routes.Online (
  OnlineRoutes (..),
) where

import Hnefatafl.Api.Types.Online (
  CreateOnlineGameRequest,
  CreateOnlineGameResponse,
 )
import Hnefatafl.Servant.WebSocket (WebSocket)
import Servant (
  GenericMode (type (:-)),
  JSON,
  Post,
  ReqBody,
 )
import Servant.API ((:>))

data OnlineRoutes mode = OnlineRoutes
  { create ::
      mode
        :- ReqBody '[JSON] CreateOnlineGameRequest :> Post '[JSON] CreateOnlineGameResponse
  , ws :: mode :- "ws" :> WebSocket
  }
  deriving stock (Generic)
