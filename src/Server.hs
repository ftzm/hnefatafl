module Server where

import Servant

import Command (Command)
import Data.Aeson (decode)
import Data.Aeson.Text (encodeToLazyText)
import Html.QuickGame
import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets qualified as WS
import QuickGame (handleCommand, initialGameState)
import Routes
import Servant.Server.Generic (AsServer, genericServe)

handlers :: Routes AsServer
handlers =
  Routes
    { board = return quickGamePage
    , quickGame = quickGameHandler
    , quickGameWs = quickGameWsHandler
    , css = serveDirectoryWebApp "."
    }

quickGameHandler :: Handler (Html ())
quickGameHandler = return quickGamePage

quickGameWsHandler :: WS.Connection -> Handler ()
quickGameWsHandler c =
  liftIO $ WS.withPingThread c 30 (pure ()) $ do
    gsr <- newIORef initialGameState
    forever $ do
      gs <- readIORef gsr
      input <- WS.receiveData c
      _ <- case decode @Command input of
        Nothing -> putStrLn "invalid command"
        Just command -> do
          putStrLn $ show @String command
          case handleCommand gs command of
            Left error -> print error
            Right (newGsO, events) -> do
              unless (null events) (WS.sendTextData c . toStrict . encodeToLazyText $ events)
              traverse_ (writeIORef gsr) newGsO
      pure ()

app :: Application
app = genericServe handlers

runApp :: IO ()
runApp = run 8080 app
