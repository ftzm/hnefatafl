{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- slight modification of servant-websockets

module ServantWebSocket where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai (Request, Response, ResponseReceived)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, ConnectionOptions (..), acceptRequest, defaultConnectionOptions)
import Network.WebSockets.Connection (PendingConnection)
import Servant (Handler, HasContextEntry, getContextEntry)
import Servant.Server (HasServer (..), ServerError (..), ServerT, runHandler)
import Servant.Server.Internal.Delayed (runDelayed)
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Server.Internal.Router (leafRouter)
import WebSocketPong (pongHandler)

data WebSocket

instance (HasContextEntry ctx ConnectionOptions) => HasServer WebSocket ctx where
  type ServerT WebSocket m = Connection -> IORef NominalDiffTime -> m ()

  route Proxy context app = leafRouter $ \env request respond ->
    runResourceT $
      runDelayed app env request >>= liftIO . go request respond
   where
    connectionOptions = getContextEntry context

    go ::
      Request ->
      (RouteResult Response -> IO ResponseReceived) ->
      RouteResult (Connection -> IORef NominalDiffTime -> Handler ()) ->
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

    runApp ::
      IORef NominalDiffTime ->
      (Connection -> IORef NominalDiffTime -> Handler a) ->
      PendingConnection ->
      IO ()
    runApp lastPongRef a pendingConnection = do
      connection <- acceptRequest pendingConnection
      void (runHandler $ a connection lastPongRef)

    backupApp respond _ _ =
      respond $
        Fail
          ServerError
            { errHTTPCode = 426
            , errReasonPhrase = "Upgrade Required"
            , errBody = mempty
            , errHeaders = mempty
            }

  hoistServerWithContext _ _ nat svr conn lastPong = nat $ svr conn lastPong
