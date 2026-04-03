{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- | Servant combinator for WebSocket endpoints.
-- Adapted from servant-websockets / bu-src/ServantWebSocket.hs.
module Hnefatafl.Servant.WebSocket (
  WebSocket,
) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai (Request, Response, ResponseReceived)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, ConnectionOptions (..), acceptRequest)
import Network.WebSockets.Connection (PendingConnection)
import Hnefatafl.Servant.WebSocket.Pong (pingHandler, pongHandler, withInterruptiblePingThread)
import Servant (Handler, HasContextEntry, getContextEntry)
import Servant.Client.Core (HasClient (..), RunClient)
import Servant.Server (HasServer (..), ServerError (..), ServerT, runHandler)
import Servant.Server.Internal.Delayed (runDelayed)
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Server.Internal.Router (leafRouter)

data WebSocket

instance (HasContextEntry ctx ConnectionOptions) => HasServer WebSocket ctx where
  type ServerT WebSocket m = Connection -> m ()

  route Proxy context app = leafRouter $ \env request respond ->
    runResourceT $
      runDelayed app env request >>= liftIO . go request respond
   where
    connectionOptions = getContextEntry context

    go ::
      Request ->
      (RouteResult Response -> IO ResponseReceived) ->
      RouteResult (Connection -> Handler ()) ->
      IO ResponseReceived
    go request respond routeResult = case routeResult of
      (Route app') -> do
        currentTime <- getPOSIXTime
        lastPongRef <- newIORef currentTime
        websocketsOr
          (connectionOptions{connectionOnPong = pongHandler lastPongRef})
          (runApp lastPongRef app')
          (backupApp respond)
          request
          (respond . Route)
      (Fail e) -> respond $ Fail e
      (FailFatal e) -> respond $ FailFatal e

    pingIntervalSeconds :: Int
    pingIntervalSeconds = 30

    pongTimeoutSeconds :: Integer
    pongTimeoutSeconds = 10

    runApp ::
      IORef NominalDiffTime ->
      (Connection -> Handler a) ->
      PendingConnection ->
      IO ()
    runApp lastPongRef a pendingConnection = do
      connection <- acceptRequest pendingConnection
      withInterruptiblePingThread connection pingIntervalSeconds
        (pingHandler lastPongRef (fromIntegral pingIntervalSeconds) pongTimeoutSeconds) $
        void (runHandler $ a connection)

    backupApp respond _ _ =
      respond $
        Fail
          ServerError
            { errHTTPCode = 426
            , errReasonPhrase = "Upgrade Required"
            , errBody = mempty
            , errHeaders = mempty
            }

  hoistServerWithContext _ _ nat svr conn = nat $ svr conn

-- | WebSocket endpoints have no meaningful HTTP client representation.
instance (RunClient m) => HasClient m WebSocket where
  type Client m WebSocket = ()
  clientWithRoute _ _ _ = ()
  hoistClientMonad _ _ _ _ = ()
