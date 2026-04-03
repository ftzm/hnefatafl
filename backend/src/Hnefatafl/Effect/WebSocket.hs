{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.WebSocket (
  module Hnefatafl.Effect.WebSocket,
) where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Network.WebSockets (Connection)

data WebSocket :: Effect where
  SendData :: Connection -> LByteString -> WebSocket m ()
  ReceiveData :: Connection -> WebSocket m LByteString

makeEffect ''WebSocket
