{-# LANGUAGE BlockArguments #-}

module Hnefatafl.App.TestUtil (
  -- * Test WebSocket connections
  TestConnPair (..),
  mkTestConnPair,
  clientSend,
  clientSendMsg,
  clientSendAuth,
  clientRecv,
  clientRecvJSON,

  -- * Effect stack runners
  runHotseatTest,
  runOnlineTest,
) where

import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM (
  TQueue,
  newTQueueIO,
  readTQueue,
  writeTQueue,
 )
import Data.Aeson (ToJSON, encode)
import Data.Aeson qualified as Aeson
import Database.SQLite.Simple (Connection)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Katip (KatipE, runKatipE)
import Hnefatafl.App.WebSocket (encodeAuthMsg)
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.IdGen.UUIDv7 (runIdGenUUIDv7)
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
import Hnefatafl.Interpreter.Trace.NoOp (runTraceNoOp)
import Hnefatafl.Interpreter.WebSocket.IO (runWebSocketIO)
import Hnefatafl.Logging (withNoLogEnv)
import Network.WebSockets (
  DataMessage (..),
  Message (..),
  defaultConnectionOptions,
 )
import Network.WebSockets.Connection (Connection (..))
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
-- Test WebSocket connections

-- | A pair of TQueues representing a WebSocket connection.
-- The test writes to @clientToServer@ and reads from @serverToClient@.
data TestConnPair = TestConnPair
  { clientToServer :: TQueue Message
  , serverToClient :: TQueue Message
  , connection :: Network.WebSockets.Connection.Connection
  }

-- | Create a test connection pair backed by TQueues.
mkTestConnPair :: IO TestConnPair
mkTestConnPair = do
  c2s <- newTQueueIO
  s2c <- newTQueueIO
  hb <- MVar.newEmptyMVar
  sc <- newIORef False
  let conn =
        Network.WebSockets.Connection.Connection
          { connectionOptions = defaultConnectionOptions
          , connectionType = unsafeCoerce (0 :: Int)
          , connectionProtocol = unsafeCoerce (0 :: Int)
          , connectionHeartbeat = hb
          , connectionParse = Just <$> atomically (readTQueue c2s)
          , connectionWrite = \msgs -> atomically $ mapM_ (writeTQueue s2c) msgs
          , connectionSentClose = sc
          }
  pure TestConnPair{clientToServer = c2s, serverToClient = s2c, connection = conn}

-- | Send raw bytes from the client to the server.
clientSend :: TestConnPair -> LByteString -> IO ()
clientSend tc msg =
  atomically $
    writeTQueue tc.clientToServer (DataMessage False False False (Text msg Nothing))

-- | Send a typed message from the client to the server.
clientSendMsg :: ToJSON a => TestConnPair -> a -> IO ()
clientSendMsg tc = clientSend tc . encode

-- | Send an auth message with the given token.
clientSendAuth :: TestConnPair -> Text -> IO ()
clientSendAuth tc token = clientSend tc (encodeAuthMsg token)

-- | Receive raw bytes that the server sent to the client.
clientRecv :: TestConnPair -> IO LByteString
clientRecv tc = atomically $ do
  msg <- readTQueue tc.serverToClient
  case msg of
    DataMessage _ _ _ (Text bs _) -> pure bs
    other -> error $ "clientRecv: unexpected message type: " <> show other

-- | Receive and decode a JSON value from the server.
clientRecvJSON :: TestConnPair -> IO Aeson.Value
clientRecvJSON tc = do
  bs <- clientRecv tc
  case Aeson.decode bs of
    Just v -> pure v
    Nothing -> error $ "clientRecvJSON: failed to decode: " <> show bs

-------------------------------------------------------------------------------
-- Effect stack runners

-- | Run a hotseat test with Storage, Clock, IdGen, and KatipE effects.
-- Uses withSharedDB for a fresh in-memory SQLite DB per test.
-- Logs are silently dropped via withNoLogEnv.
runHotseatTest ::
  MVar Database.SQLite.Simple.Connection ->
  ( forall es.
    ( IOE :> es
    , Storage :> es
    , Clock :> es
    , IdGen :> es
    , KatipE :> es
    , Trace :> es
    ) =>
    Eff es a
  ) ->
  IO a
runHotseatTest connVar action = do
  result <- withNoLogEnv "test" $ \logEnv ->
    runEff
      . runErrorNoCallStack @String
      . runConcurrent
      . runKatipE logEnv
      . runTraceNoOp
      . runStorageSQLite connVar
      . runClockIO
      . runIdGenUUIDv7
      $ action
  case result of
    Left err -> error $ toText $ "runHotseatTest: " <> err
    Right a -> pure a

-- | Run an online/AI test with Storage, Clock, IdGen, Concurrent, WebSocket,
-- and KatipE effects. Logs are silently dropped via withNoLogEnv.
runOnlineTest ::
  MVar Database.SQLite.Simple.Connection ->
  ( forall es.
    ( IOE :> es
    , Storage :> es
    , Clock :> es
    , IdGen :> es
    , Concurrent :> es
    , WebSocket :> es
    , KatipE :> es
    , Trace :> es
    ) =>
    Eff es a
  ) ->
  IO a
runOnlineTest connVar action = do
  result <- withNoLogEnv "test" $ \logEnv ->
    runEff
      . runErrorNoCallStack @String
      . runKatipE logEnv
      . runTraceNoOp
      . runConcurrent
      . runStorageSQLite connVar
      . runClockIO
      . runIdGenUUIDv7
      . runWebSocketIO
      $ action
  case result of
    Left err -> error $ toText $ "runOnlineTest: " <> err
    Right a -> pure a
