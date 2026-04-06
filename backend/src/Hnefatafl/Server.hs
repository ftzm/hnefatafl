{-# LANGUAGE DataKinds #-}

module Hnefatafl.Server (
  runServer,
) where

import Control.Concurrent.QSem (newQSem)
import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Database.SQLite.Simple (close, open)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runWarpServerSettingsContext)
import Hnefatafl.Api.Handlers (server)
import Hnefatafl.Api.Routes (HnefataflAPI)
import Hnefatafl.Effect.Log (runLog)
import Hnefatafl.Exception (guardExceptions)
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.IdGen.UUIDv7 (runIdGenUUIDv7)
import Hnefatafl.Interpreter.Log.JSON (withJsonLogEnv)
import Hnefatafl.Interpreter.Search.Local (runSearchLocal)
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
import Hnefatafl.Interpreter.WebSocket.IO (runWebSocketIO)
import Network.WebSockets (ConnectionOptions)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.WebSockets (defaultConnectionOptions)
import Servant (Context (EmptyContext, (:.)), ServerError, hoistServerWithContext)
import Servant.Server.Generic (genericServerT)
import StmContainers.Map qualified as STMMap

runServer :: Int -> IO ()
runServer port = do
  setBacktraceMechanismState IPEBacktrace True
  putTextLn $ "Starting Hnefatafl server on port " <> show port
  conn <- open "db.db"
  connectionVar <- newMVar conn
  let settings = defaultSettings & setPort port
  onlineSessions <- STMMap.newIO
  aiSessions <- STMMap.newIO
  qsem <- newQSem 20
  let ctx = defaultConnectionOptions :. EmptyContext
  withJsonLogEnv "hnefatafl" $ \logEnv -> do
    result <-
      runEff
        . runErrorNoCallStack @ServerError
        . runConcurrent
        . runLog logEnv
        . runStorageSQLite connectionVar
        . runClockIO
        . runIdGenUUIDv7
        . runSearchLocal qsem
        . runWebSocketIO
        $ runWarpServerSettingsContext @HnefataflAPI
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
      Left err -> putTextLn $ "Server error: " <> show err
      Right () -> pure ()
