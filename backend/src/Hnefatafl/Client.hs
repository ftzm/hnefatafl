{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Client (
  HnefataflClient,
  createClient,
) where

import Hnefatafl.Server (Routes)
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

type HnefataflClient = Routes (AsClientT ClientM)

createClient :: HnefataflClient
createClient = genericClient @Routes