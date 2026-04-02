module Hnefatafl.Server (
  runServer,
) where

import Database.SQLite.Simple (close, open)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.QSem (newQSem)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runWarpServerSettings)
import Hnefatafl.Api.Handlers (server)
import Hnefatafl.Api.Routes (HnefataflAPI)
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.IdGen.UUIDv7 (runIdGenUUIDv7)
import Hnefatafl.Interpreter.Search.Local (runSearchLocal)
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Servant (ServerError)
import Servant.Server.Generic (genericServerT)

runServer :: Int -> IO ()
runServer port = do
  putTextLn $ "Starting Hnefatafl server on port " <> show port
  conn <- open "db.db"
  connectionVar <- newMVar conn
  let settings = defaultSettings & setPort port
  result <-
    runEff $
      runErrorNoCallStack @ServerError $
        runErrorNoCallStack @String $
          runConcurrent $
            runStorageSQLite connectionVar $
              runClockIO $
                runIdGenUUIDv7 $
                  do
                    qsem <- newQSem 20
                    runSearchLocal qsem $
                      runWarpServerSettings @HnefataflAPI settings (genericServerT server) id
  close conn
  case result of
    Left err -> putTextLn $ "Server error: " <> show err
    Right (Left err) -> putTextLn $ "Server error: " <> toText err
    Right (Right ()) -> pure ()
