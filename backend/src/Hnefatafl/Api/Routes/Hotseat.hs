{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Api.Routes.Hotseat (
  HotseatRoutes (..),
) where

import Hnefatafl.Api.Types (ActionResponse, ApiGameState, ApiMove)
import Hnefatafl.Core.Data (GameId, PlayerColor)
import Servant (Capture, GenericMode (type (:-)), Get, JSON, Post, ReqBody)
import Servant.API ((:>))

data HotseatRoutes mode = HotseatRoutes
  { create :: mode :- Post '[JSON] ApiGameState
  , get :: mode :- Capture "id" GameId :> Get '[JSON] ApiGameState
  , move ::
      mode
        :- Capture "id" GameId
          :> "move"
          :> ReqBody '[JSON] ApiMove
          :> Post '[JSON] ActionResponse
  , undo :: mode :- Capture "id" GameId :> "undo" :> Post '[JSON] ActionResponse
  , resign ::
      mode
        :- Capture "id" GameId
          :> "resign"
          :> ReqBody '[JSON] PlayerColor
          :> Post '[JSON] ActionResponse
  , draw :: mode :- Capture "id" GameId :> "draw" :> Post '[JSON] ActionResponse
  }
  deriving stock (Generic)
