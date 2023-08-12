{-# LANGUAGE DataKinds #-}

module Routes where

import Lucid (Html)
import Servant
import Servant.API.WebSocket
import ServantLucid
import Data.UUID (UUID)

data Routes route = Routes
  { board :: route :- "board" :> Get '[HTML] (Html ())
  , quickGame :: route :- "quickgame" :> Get '[HTML] (Html ())
  , quickGameWs :: route :- "quickgamews" :> WebSocket
  , startHotseat :: route :- "start-hotseat" :> Get '[HTML] (Html ())
  , hotseat :: route :- "hotseat" :> Capture "hotseatId" UUID :> Get '[HTML] (Html ())
  , hotseatWs :: route :- "hotseatws" :> Capture "hotseatId" UUID :> WebSocket
  , startAi :: route :- "start-ai" :> Capture "humanIsBlack" Bool :> Get '[HTML] (Html ())
  , ai :: route :- "ai" :> Capture "aiId" UUID :> Get '[HTML] (Html ())
  , aiWs :: route :- "aiws" :> Capture "aiId" UUID :> WebSocket
  , home :: route :- Get '[HTML] (Html ())
  , static :: route :- Raw
  }
  deriving (Generic)
