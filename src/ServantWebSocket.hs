{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module ServantWebSocket where

import Control.Monad                              (void, (>=>))
import Control.Monad.IO.Class                     (liftIO)
import Control.Monad.Trans.Resource               (runResourceT)
import Data.Proxy                                 (Proxy (..))
import Network.Wai.Handler.WebSockets             (websocketsOr)
import Network.WebSockets                         (Connection, PendingConnection, acceptRequest, defaultConnectionOptions)
import Servant.Server                             (HasServer (..), ServerError (..), ServerT, runHandler)
import Servant.Server.Internal.Router             (leafRouter)
import Servant.Server.Internal.RouteResult        (RouteResult (..))
import Servant.Server.Internal.Delayed            (runDelayed)


data WebSocket

instance HasServer WebSocket ctx where

  type ServerT WebSocket m = Connection -> m ()

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ _ nat svr = nat . svr
#endif

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route app') =
      websocketsOr defaultConnectionOptions (runApp app') (backupApp respond) request (respond . Route)
    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runApp a = acceptRequest >=> \c -> void (runHandler $ a c)

    backupApp respond _ _ = respond $ Fail ServerError { errHTTPCode = 426
                                                       , errReasonPhrase = "Upgrade Required"
                                                       , errBody = mempty
                                                       , errHeaders = mempty
                                                       }
