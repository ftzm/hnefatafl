{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.WebSocket.IO (
  runWebSocketIO,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Hnefatafl.Effect.WebSocket
import Network.WebSockets qualified as WS

runWebSocketIO :: IOE :> es => Eff (WebSocket : es) a -> Eff es a
runWebSocketIO = interpret $ \_ -> \case
  SendData conn msg -> liftIO $ WS.sendTextData conn msg
  ReceiveData conn -> liftIO $ WS.receiveData conn
