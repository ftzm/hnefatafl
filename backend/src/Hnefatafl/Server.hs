module Hnefatafl.Server (
  runServer,
) where

import Database.SQLite.Simple (close, open)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Control.Concurrent.QSem (newQSem)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runWarpServerSettingsContext)
import Hnefatafl.Api.Handlers (server)
import Hnefatafl.Api.Routes (HnefataflAPI)
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.IdGen.UUIDv7 (runIdGenUUIDv7)
import Hnefatafl.Interpreter.Search.Local (runSearchLocal)
import Hnefatafl.Interpreter.WebSocket.IO (runWebSocketIO)
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.WebSockets (defaultConnectionOptions)
import Servant (Context (EmptyContext, (:.)), ServerError)
import Servant.Server.Generic (genericServerT)
import StmContainers.Map qualified as STMMap

runServer :: Int -> IO ()
runServer port = do
  putTextLn $ "Starting Hnefatafl server on port " <> show port
  conn <- open "db.db"
  connectionVar <- newMVar conn
  let settings = defaultSettings & setPort port
  gameSessions <- STMMap.newIO
  qsem <- newQSem 20
  let ctx = defaultConnectionOptions :. EmptyContext
  result <-
    runEff
      . runErrorNoCallStack @ServerError
      . runErrorNoCallStack @String
      . runConcurrent
      . runStorageSQLite connectionVar
      . runClockIO
      . runIdGenUUIDv7
      . runSearchLocal qsem
      . runWebSocketIO
      $ runWarpServerSettingsContext @HnefataflAPI settings ctx (genericServerT (server gameSessions)) id
  close conn
  case result of
    Left err -> putTextLn $ "Server error: " <> show err
    Right (Left err) -> putTextLn $ "Server error: " <> toText err
    Right (Right ()) -> pure ()
