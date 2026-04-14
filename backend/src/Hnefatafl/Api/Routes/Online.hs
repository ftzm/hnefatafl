{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Api.Routes.Online (
  OnlineRoutes (..),
) where

import Hnefatafl.Api.Types.Online (CreateOnlineGameResponse)
import Hnefatafl.Servant.WebSocket (WebSocket)
import Servant (
  GenericMode (type (:-)),
  JSON,
  Post,
 )
import Servant.API ((:>))

data OnlineRoutes mode = OnlineRoutes
  { create :: mode :- Post '[JSON] CreateOnlineGameResponse
  , ws :: mode :- "ws" :> WebSocket
  }
  deriving stock (Generic)
