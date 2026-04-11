module Hnefatafl.App.WebSocket (
  safeSend,
  encodeAuthMsg,
  decodeAuthToken,
  guardWebSocket,
  tryNonFatal,
  runMessageLoop,
  withGameContext,
) where

import Control.Exception (SomeAsyncException (..))
import Data.Aeson (
  FromJSON,
  decode,
  encode,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (Parser, parseMaybe)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Exception (catch, throwIO)
import Effectful.Katip (KatipE, katipAddContext, katipAddNamespace)
import Hnefatafl.Api.Types.WS (WsError (..), WsErrorCode (..))
import Hnefatafl.Core.Data (GameId, PlayerColor)
import Hnefatafl.Effect.WebSocket (WebSocket, receiveData, sendData)
import Hnefatafl.Exception (
  DomainException (..),
  IsDomainException (..),
  logCaughtException,
 )
import Katip (sl)
import Network.WebSockets (Connection, ConnectionException)

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
-- exceptions, logs structured data for domain exceptions via Katip, and
-- sends a final error message to the client before the handler exits.
-- Async and connection exceptions are always re-thrown.
--
-- This is the outer guard, run around the full handler. It catches
-- anything that escapes 'tryNonFatal' inside the receive loop — either
-- fatal exceptions (unmodeled or domain-marked fatal) or exceptions
-- raised outside the loop (e.g. during connection setup).
guardWebSocket ::
  (WebSocket :> es, KatipE :> es) =>
  Connection -> Eff es () -> Eff es ()
guardWebSocket conn action =
  action `catch` \(ex :: SomeException) ->
    case () of
      _
        | isJust (fromException @SomeAsyncException ex) -> throwIO ex
        | isJust (fromException @ConnectionException ex) -> throwIO ex
        | otherwise -> do
            logCaughtException ex
            sendData conn (encode $ WsError InternalError "internal error")
              `catch` \(_ :: SomeException) -> pure ()

-- | Run a single WebSocket receive-loop iteration, absorbing non-fatal
-- domain exceptions so the session stays alive. The rule is:
-- a caught exception is only non-fatal when it is a 'DomainException'
-- whose 'domainFatal' returns False. Everything else — unmodeled
-- exceptions, async exceptions, connection exceptions, and domain
-- exceptions marked fatal — propagates out to the outer 'guardWebSocket'
-- which tears down the session.
--
-- Non-domain exceptions are treated as fatal on purpose: they represent
-- failure modes we have not classified (programming bugs, unwrapped
-- I/O errors, library exceptions), and resuming would risk acting on
-- unknown state.
tryNonFatal ::
  (WebSocket :> es, Concurrent :> es, KatipE :> es) =>
  MVar Connection -> Eff es () -> Eff es ()
tryNonFatal connVar action =
  action `catch` \(ex :: SomeException) ->
    case fromException @DomainException ex of
      Just (DomainException e) | not (domainFatal e) -> do
        logCaughtException ex
        safeSend connVar (encode $ WsError InternalError "internal error")
      _ -> throwIO ex

-- | Run a receive loop over a WebSocket connection: read messages, decode
-- them as JSON of type @msg@, and pass each decoded message to the
-- supplied handler. Each iteration is wrapped in 'tryNonFatal', so a
-- non-fatal exception during decode or handler execution is logged,
-- reported to the client, and the loop continues. Fatal exceptions
-- propagate out to the caller (typically 'guardWebSocket').
--
-- Decode failures are handled in-band: the client receives a generic
-- @invalid_message@ error and the loop continues.
runMessageLoop ::
  ( FromJSON msg
  , WebSocket :> es
  , Concurrent :> es
  , KatipE :> es
  ) =>
  MVar Connection ->
  (msg -> Eff es ()) ->
  Eff es ()
runMessageLoop connVar handle = do
  conn <- MVar.readMVar connVar
  forever $ tryNonFatal connVar $ do
    raw <- receiveData conn
    case decode raw of
      Nothing -> safeSend connVar (encode $ WsError InvalidMessage "invalid message")
      Just msg -> handle msg

-- | Add the @game@ namespace together with @gameId@ and @player@ context
-- fields for the enclosed action.
withGameContext ::
  KatipE :> es => GameId -> PlayerColor -> Eff es a -> Eff es a
withGameContext gameId color =
  katipAddNamespace "game"
    . katipAddContext (sl "gameId" (show @Text gameId))
    . katipAddContext (sl "player" (show @Text color))
