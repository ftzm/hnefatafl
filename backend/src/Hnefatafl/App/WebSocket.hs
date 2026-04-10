module Hnefatafl.App.WebSocket (
  safeSend,
  encodeAuthMsg,
  decodeAuthToken,
  guardWebSocket,
) where

import Control.Exception (SomeAsyncException (..), someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Data.Aeson (
  decode,
  encode,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (Parser, parseMaybe)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Exception (catch, throwIO)
import Hnefatafl.Api.Types.WS (WsError (..), WsErrorCode (..))
import Hnefatafl.Effect.WebSocket (WebSocket, sendData)
import Hnefatafl.Exception (DomainException (..), IsDomainException (..))
import Network.WebSockets (Connection, ConnectionException)
import System.IO (hPutStrLn)

-- | Send data on a connection held in an MVar. The MVar ensures only one
-- thread writes to the connection at a time. Silently catches connection
-- exceptions.
safeSend ::
  (Concurrent :> es, WebSocket :> es) =>
  MVar Connection -> LByteString -> Eff es ()
safeSend connVar msg =
  MVar.withMVar connVar $ \conn ->
    sendData conn msg
      `catch` \(_ :: ConnectionException) -> pure ()

encodeAuthMsg :: Text -> LByteString
encodeAuthMsg token = encode $ object ["type" .= ("auth" :: Text), "token" .= token]

decodeAuthToken :: LByteString -> Maybe Text
decodeAuthToken msg = do
  obj <- decode msg
  flip parseMaybe obj $ withObject "auth" $ \o -> do
    typ <- o .: "type" :: Parser Text
    guard (typ == "auth")
    o .: "token"

-- | Catch-all exception guard for WebSocket handlers. Catches all synchronous
-- exceptions, logs structured data for domain exceptions, and sends an error
-- message to the client. Async and connection exceptions are always re-thrown.
guardWebSocket ::
  (IOE :> es, WebSocket :> es) =>
  Connection -> Eff es () -> Eff es ()
guardWebSocket conn action =
  action `catch` \(ex :: SomeException) ->
    case () of
      _
        | isJust (fromException @SomeAsyncException ex) -> throwIO ex
        | isJust (fromException @ConnectionException ex) -> throwIO ex
        | otherwise -> do
            let ctx = someExceptionContext ex
            case fromException @DomainException ex of
              Just (DomainException e) ->
                liftIO $
                  hPutStrLn stderr $
                    "Domain exception ["
                      <> toString (domainErrorLabel e)
                      <> "]: "
                      <> displayException e
                      <> "\n"
                      <> toString (show (domainContext e) :: Text)
                      <> "\n"
                      <> displayExceptionContext ctx
              Nothing ->
                liftIO $
                  hPutStrLn stderr $
                    "Unhandled exception: "
                      <> displayException ex
                      <> "\n"
                      <> displayExceptionContext ctx
            sendData conn (encode $ WsError InternalError "internal error")
              `catch` \(_ :: SomeException) -> pure ()
