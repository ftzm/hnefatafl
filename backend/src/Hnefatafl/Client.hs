{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Client (
  HnefataflClient,
  createClient,
) where

import Hnefatafl.Api.Routes (Routes)
import Hnefatafl.Servant.WebSocket ()
-- HasClient instance for WebSocket
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

type HnefataflClient = Routes (AsClientT ClientM)

createClient :: HnefataflClient
createClient = genericClient @Routes
