module Server where

import Servant

import Board.Board (Team (..))
import Board.Constant (startBoard)
import Command (Command (..))
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi (newChan, readChan, writeChan)
import DB.Game (AIGame (..), Game (..), createAI, createHotseat, insertGame, selectAI, selectHotseat, updateGame)
import Event (Event (..))

import Data.Aeson (decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (open)
import GHC.Conc (threadDelay)
import Game.AI (runLoop)
import Game.AI qualified as AI
import Game.Hotseat qualified as Hotseat
import Html.AI (aiPage)
import Html.Home (homePage)
import Html.Hotseat (hotseatPage)
import Html.QuickGame
import Lucid (Html)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setPort, setTimeout)
import Network.WebSockets (Connection, ControlMessage (..), Message (..), WebSocketsData (..))
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection (send)
import QuickGame qualified
import Routes
import Servant.Server.Generic (AsServer, genericServe)
import Control.Exception (handle)

handlers :: Routes AsServer
handlers =
  Routes
    { board = return quickGamePage
    , quickGame = quickGameHandler
    , quickGameWs = quickGameWsHandler
    , startHotseat = startHotseatHandler
    , hotseat = hotseatHandler
    , hotseatWs = hotseatWsHandler
    , startAi = startAIHandler
    , ai = aiHandler
    , aiWs = aiWsHandler'
    , home = return homePage
    , static = serveDirectoryWebApp "."
    }

quickGameHandler :: Handler (Html ())
quickGameHandler = return quickGamePage

quickGameWsHandler :: WS.Connection -> Handler ()
quickGameWsHandler c =
  liftIO $ WS.withPingThread c 30 (pure ()) $ do
    gsr <- newIORef QuickGame.initialGameState
    forever $ do
      gs <- readIORef gsr
      input <- WS.receiveData c
      _ <- case decode @Command input of
        Nothing -> putStrLn $ "invalid command: " <> show input
        Just command -> do
          putStrLn $ show @String command
          case QuickGame.handleCommand gs command of
            Left err -> print err
            Right (newGsO, events) -> do
              unless (null events) (WS.sendTextData c . toStrict . encodeToLazyText $ events)
              traverse_ (writeIORef gsr) newGsO
      pure ()

startHotseatHandler :: Handler (Html ())
startHotseatHandler = do
  uuid <- liftIO nextRandom
  putStrLn $ "new game id: " <> show uuid
  conn <- liftIO $ open "db.db"
  let newGame = Game uuid startBoard True
  liftIO $ createHotseat conn newGame
  throwError err302{errHeaders = [("Location", "/hotseat/" <> show uuid)]}

gameToGameState :: Game -> Hotseat.GameState
gameToGameState game =
  Hotseat.GameState
    { board = game.board
    , isBlackTurn = game.isBlackTurn
    , moves = if game.isBlackTurn then Hotseat.getBlackMoves game.board else Hotseat.getWhiteMoves game.board
    , messages = []
    }

gameStateToGame :: UUID -> Hotseat.GameState -> Game
gameStateToGame i gs =
  Game
    { id = i
    , board = gs.board
    , isBlackTurn = gs.isBlackTurn
    }

hotseatHandler :: UUID -> Handler (Html ())
hotseatHandler hotseatId = do
  conn <- liftIO $ open "db.db"
  hotseatGame <- liftIO $ selectHotseat conn hotseatId
  return $ hotseatPage hotseatId $ gameToGameState hotseatGame

hotseatWsHandler :: UUID -> WS.Connection -> Handler ()
hotseatWsHandler hotseatId c =
  liftIO $ WS.withPingThread c 30 (pure ()) $ do
    conn <- liftIO $ open "db.db"
    hotseatGame <- liftIO $ selectHotseat conn hotseatId
    gsr <- newIORef $ gameToGameState hotseatGame
    forever $ do
      gs <- readIORef gsr
      input <- WS.receiveData c
      _ <- case decode @Command input of
        Nothing -> putStrLn $ "invalid command: " <> show input
        Just command -> do
          putStrLn $ show @String command
          case Hotseat.handleCommand gs command of
            Left err -> print err
            Right (newGsO, events) -> do
              unless (null events) (WS.sendTextData c . toStrict . encodeToLazyText $ events)
              for_ newGsO $ \newGs -> do
                writeIORef gsr newGs
                updateGame conn hotseatId (Hotseat.board newGs) (Hotseat.isBlackTurn newGs)
      pure ()

startAIHandler :: Bool -> Handler (Html ())
startAIHandler humanIsBlack = do
  uuid <- liftIO nextRandom
  putStrLn $ "new game id: " <> show uuid
  conn <- liftIO $ open "db.db"
  let newGame = Game uuid startBoard True
  liftIO $ createAI conn newGame humanIsBlack
  throwError err302{errHeaders = [("Location", "/ai/" <> show uuid)]}

aiHandler :: UUID -> Handler (Html ())
aiHandler aiId = do
  conn <- liftIO $ open "db.db"
  aiGame <- liftIO $ selectAI conn aiId
  return $ aiPage aiId $ aiGameToGameState aiGame

aiGameToGameState :: AIGame -> AI.GameState
aiGameToGameState game =
  AI.GameState
    { board = game.board
    , isBlackTurn = game.isBlackTurn
    , moves = if game.isBlackTurn then Hotseat.getBlackMoves game.board else Hotseat.getWhiteMoves game.board
    , humanIsBlack = game.humanIsBlack
    }

aiWsHandler :: UUID -> WS.Connection -> Handler ()
aiWsHandler hotseatId c =
  liftIO $ WS.withPingThread c 10 (putStrLn "ws ping") $ do
    conn <- liftIO $ open "db.db"
    aiGame <- liftIO $ selectAI conn hotseatId
    gsr <- newIORef $ aiGameToGameState aiGame
    threadDelay 1000000

-- forever $ do
--   gs <- readIORef gsr
--   input <- WS.receiveData c
--   _ <- case decode @Command input of
--     Nothing -> putStrLn $ "invalid command: " <> show input
--     Just command -> do
--       putStrLn $ show @String command
--       case AI.handleCommand gs command of
--         Left err -> print err
--         Right (newGsO, events) -> do
--           unless (null events) (WS.sendTextData c . toStrict . encodeToLazyText $ events)
--           for_ newGsO $ \newGs -> do
--             writeIORef gsr newGs
--             updateGame conn hotseatId (AI.board newGs) (AI.isBlackTurn newGs)
--   pure ()

withAsync' :: IO a -> IO b -> IO b
withAsync' action inner = withAsync action $ const inner

aiWsHandler' :: UUID -> WS.Connection -> Handler ()
aiWsHandler' aiId c = liftIO $ do
  conn <- open "db.db"
  aiGame <- selectAI conn aiId
  let gs = aiGameToGameState aiGame
  gsr <- newIORef gs
  (commandIn, commandOut) <- newChan
  (eventIn, eventOut) <- newChan

  let
    aiTeam = if aiGame.humanIsBlack then White else Black
    saveGame gameState = updateGame conn aiId (AI.board gameState) (AI.isBlackTurn gameState)
    aiLoop = runLoop saveGame aiTeam gsr commandIn commandOut eventIn
    handleCommands = forever $ do
      putStrLn "handleCommands"
      input <- WS.receiveData c
      case decode @Command input of
        Nothing -> putStrLn $ "invalid command: " <> show input
        Just command -> writeChan commandIn command
    handleEvents = forever $ do
      putStrLn "handleEvents"
      event <- readChan eventOut
      WS.sendTextData c . toStrict . encodeToLazyText $ event

  aiLoop
    `race_` handleEvents
    `race_` handleCommands
    `race_` (pingThread c 10 (putStrLn "ping") >> putStrLn "pingExit")
  putStrLn "finishRequest"


sendPing :: WebSocketsData a => Connection -> a -> IO ()
sendPing conn = send conn . ControlMessage . Ping . toLazyByteString

pingThread :: Connection -> Int -> IO () -> IO ()
pingThread conn n action
  | n <= 0 = return ()
  | otherwise = go 1
 where
  go :: Int -> IO ()
  go i = do
    threadDelay (n * 1000 * 1000)
    (\(e :: SomeException) -> putStrLn "crashed ping") `handle` sendPing conn (T.pack $ show i)
    action
    go (i + 1)

app :: Application
app = genericServe handlers

runApp :: IO ()
runApp = run 8080 app
