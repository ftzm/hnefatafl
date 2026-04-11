{-# LANGUAGE DataKinds #-}

module Hnefatafl.Server (
  runServer,
) where

import Control.Concurrent.QSem (newQSem)
import Control.Exception.Backtrace (
  BacktraceMechanism (..),
  setBacktraceMechanismState,
 )
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Database.SQLite.Simple (close, open)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Katip (logTM, runKatipE)
import Effectful.Servant (runWarpServerSettingsContext)
import Hnefatafl.Api.Handlers (server)
import Hnefatafl.Api.Routes (HnefataflAPI)
import Hnefatafl.Exception (guardExceptions)
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.IdGen.UUIDv7 (runIdGenUUIDv7)
import Hnefatafl.Interpreter.Search.Local (runSearchLocal)
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
import Hnefatafl.Interpreter.Trace.OTel (runTraceOTel, setKatipTraceId)
import Hnefatafl.Interpreter.WebSocket.IO (runWebSocketIO)
import Hnefatafl.Logging (closeLogEnv, mkJsonLogEnv)
import Katip (Severity (..), ls)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.WebSockets (ConnectionOptions, defaultConnectionOptions)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace (
  initializeGlobalTracerProvider,
  makeTracer,
  shutdownTracerProvider,
  tracerOptions,
 )
import Servant (
  Context (EmptyContext, (:.)),
  ServerError,
  hoistServerWithContext,
 )
import Servant.Server.Generic (genericServerT)
import StmContainers.Map qualified as STMMap

runServer :: Int -> Severity -> IO ()
runServer port logLevel = runResourceT $ do
  liftIO $ setBacktraceMechanismState IPEBacktrace True
  (_, conn) <- allocate (open "db.db") close
  (_, logEnv) <- allocate (mkJsonLogEnv "hnefatafl" logLevel) closeLogEnv
  (_, provider) <-
    allocate initializeGlobalTracerProvider shutdownTracerProvider
  let tracer = makeTracer provider "hnefatafl" tracerOptions
  otelMiddleware <- liftIO newOpenTelemetryWaiMiddleware
  connectionVar <- liftIO $ newMVar conn
  let settings = defaultSettings & setPort port
      ctx = defaultConnectionOptions :. EmptyContext
  onlineSessions <- liftIO STMMap.newIO
  aiSessions <- liftIO STMMap.newIO
  qsem <- liftIO $ newQSem 20
  result <-
    liftIO
      . runEff
      . runErrorNoCallStack @ServerError
      . runConcurrent
      . runKatipE logEnv
      . runTraceOTel tracer
      . runStorageSQLite connectionVar
      . runClockIO
      . runIdGenUUIDv7
      . runSearchLocal qsem
      . runWebSocketIO
      $ do
        $(logTM) InfoS $
          ls @Text ("Starting Hnefatafl server on port " <> show port)
        runWarpServerSettingsContext @HnefataflAPI
          settings
          ctx
          ( hoistServerWithContext
              (Proxy @HnefataflAPI)
              (Proxy @'[ConnectionOptions])
              (setKatipTraceId . guardExceptions)
              (genericServerT (server onlineSessions aiSessions))
          )
          otelMiddleware
  case result of
    Left err ->
      liftIO . runEff . runKatipE logEnv $
        $(logTM) CriticalS $
          ls @Text ("Server error: " <> show err)
    Right () -> pure ()
