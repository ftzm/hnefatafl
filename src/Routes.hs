{-# LANGUAGE DataKinds #-}

module Routes where

import Servant
import Lucid (Html)
import ServantLucid
import Servant.API.WebSocket

data Routes route = Routes
    { board :: route :- "board" :> Get '[HTML] (Html ())
    , quickGame :: route :- "quickgame" :> Get '[HTML] (Html ())
    , quickGameWs :: route :- "quickgamews" :> WebSocket
    , css :: route :- Raw
    }
  deriving (Generic)
