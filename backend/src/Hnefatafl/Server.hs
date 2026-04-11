{-# LANGUAGE DataKinds #-}

module Hnefatafl.Server (
  runServer,
) where

import Control.Concurrent.QSem (newQSem)
import Control.Exception.Backtrace (
  BacktraceMechanism (..),
  setBacktraceMechanismState,
 )
import Database.SQLite.Simple (close, open)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runWarpServerSettingsContext)
import Hnefatafl.Api.Handlers (server)
import Hnefatafl.Api.Routes (HnefataflAPI)
import Hnefatafl.Effect.Log (Severity (..), logTM, ls, runKatipE)
import Hnefatafl.Exception (guardExceptions)
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.IdGen.UUIDv7 (runIdGenUUIDv7)
import Hnefatafl.Interpreter.Log.JSON (withJsonLogEnv)
import Hnefatafl.Interpreter.Search.Local (runSearchLocal)
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
import Hnefatafl.Interpreter.WebSocket.IO (runWebSocketIO)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.WebSockets (ConnectionOptions, defaultConnectionOptions)
import Servant (
  Context (EmptyContext, (:.)),
  ServerError,
  hoistServerWithContext,
 )
import Servant.Server.Generic (genericServerT)
import StmContainers.Map qualified as STMMap

runServer :: Int -> Severity -> IO ()
runServer port logLevel = do
  setBacktraceMechanismState IPEBacktrace True
  conn <- open "db.db"
  connectionVar <- newMVar conn
  let settings = defaultSettings & setPort port
  onlineSessions <- STMMap.newIO
  aiSessions <- STMMap.newIO
  qsem <- newQSem 20
  let ctx = defaultConnectionOptions :. EmptyContext
  withJsonLogEnv "hnefatafl" logLevel $ \logEnv -> do
    result <-
      runEff
        . runErrorNoCallStack @ServerError
        . runConcurrent
        . runKatipE logEnv
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
                guardExceptions
                (genericServerT (server onlineSessions aiSessions))
            )
            id
    close conn
    case result of
      Left err ->
        runEff . runKatipE logEnv $
          $(logTM) CriticalS $
            ls @Text ("Server error: " <> show err)
      Right () -> pure ()
