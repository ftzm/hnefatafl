module Server where

import Servant

import Board.Board (Board, Team (..))
import Board.Constant (startBoard)
import Command (Command (..))
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi (newChan, readChan, writeChan)
import DB.Game (AIGame (..), Game (..), VsGame (..), createAI, createHotseat, createVs, insertGame, selectAI, selectHotseat, selectVs, updateGame)
import Event (Event (..), Resolution)

import Control.Concurrent (modifyMVar, modifyMVar_)
import Control.Concurrent.STM (TChan, dupTChan)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO)
import Control.Exception (handle)
import Control.Lens
import Data.Aeson (decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Generics.Labels
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (open)
import GHC.Conc (threadDelay)
import Game.AI (runLoop)
import Game.AI qualified as AI
import Game.Hotseat qualified as Hotseat
import Game.Vs qualified as VS
import Html.AI (aiPage)
import Html.Home (homePage)
import Html.Hotseat (hotseatPage)
import Html.QuickGame
import Lucid (Html)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setPort, setTimeout)
import Network.WebSockets (Connection, ConnectionOptions (..), ControlMessage (..), Message (..), WebSocketsData (..))
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection (send)
import QuickGame qualified
import Routes
import Servant.Server.Generic (AsServer, AsServerT, genericServe, genericServeTWithContext)
import WebSocketPong (pingHandler, withInterruptiblePingThread)

newtype AppState = AppState {connections :: Map UUID (TChan VsComms)}

type AppM = ReaderT AppState Handler

-- natural transformation
nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s

handlers :: Routes (AsServerT AppM)
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
    , startVs = startVsHandler
    , home = return homePage
    , static = serveDirectoryWebApp "."
    }

quickGameHandler :: AppM (Html ())
quickGameHandler = return quickGamePage

quickGameWsHandler :: WS.Connection -> IORef NominalDiffTime -> AppM ()
quickGameWsHandler c _ =
  -- liftIO $ withPingThread c 30 (pure ()) $ do
  liftIO $ do
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

startHotseatHandler :: AppM (Html ())
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

hotseatHandler :: UUID -> AppM (Html ())
hotseatHandler hotseatId = do
  conn <- liftIO $ open "db.db"
  hotseatGame <- liftIO $ selectHotseat conn hotseatId
  return $ hotseatPage hotseatId $ gameToGameState hotseatGame

hotseatWsHandler :: UUID -> WS.Connection -> IORef NominalDiffTime -> AppM ()
hotseatWsHandler hotseatId c _ =
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

startAIHandler :: Bool -> AppM (Html ())
startAIHandler humanIsBlack = do
  uuid <- liftIO nextRandom
  putStrLn $ "new game id: " <> show uuid
  conn <- liftIO $ open "db.db"
  let newGame = Game uuid startBoard True
  liftIO $ createAI conn newGame humanIsBlack
  throwError err302{errHeaders = [("Location", "/ai/" <> show uuid)]}

aiHandler :: UUID -> AppM (Html ())
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

withAsync' :: IO a -> IO b -> IO b
withAsync' action inner = withAsync action $ const inner

aiWsHandler' :: UUID -> WS.Connection -> IORef NominalDiffTime -> AppM ()
aiWsHandler' aiId c lastPongRef =
  liftIO $
    (\(_ :: SomeException) -> putStrLn "exit handler via exception") `handle` do
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

      -- withInterruptiblePingThread c 10 (pingHandler lastPongRef 10 10) $
      --   aiLoop
      --     `race_` handleEvents
      --     `race_` handleCommands
      withInterruptiblePingThread c 10 (pingHandler lastPongRef 10 30) $
        -- threadDelay 1000000000
        -- handleEvents
        handleCommands
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
    (\(_ :: SomeException) -> putStrLn "crashed ping") `handle` sendPing conn (T.pack $ show i)
    action
    go (i + 1)

app :: Application
app = genericServeTWithContext (nt appState) handlers $ connectionOptions :. EmptyContext
 where
  appState = AppState mempty
  connectionOptions :: ConnectionOptions =
    WS.defaultConnectionOptions
      { connectionOnPong = putStrLn "received pong"
      }

runApp :: IO ()
runApp = run 8080 app

--------------------------------------------------------------------------------
-- Vs

data GameState = GameState
  { board :: Board
  , team :: Team
  , resolution :: Maybe Resolution
  }

data VsComms
  = Connected Team
  | Disconnected Team
  | GamestateUpdate GameState

data VsConnection = VsConnection
  { blackClients :: Int
  , whiteClients :: Int
  , broadcaster :: TChan VsComms
  }
  deriving (Generic)

mkVsConnection :: IO VsConnection
mkVsConnection = VsConnection 0 0 <$> newBroadcastTChanIO

connectToVsConnection ::
  UUID ->
  Team ->
  MVar (Map UUID VsConnection) ->
  IO (TChan VsComms)
connectToVsConnection gameId team connectionsVar =
  modifyMVar connectionsVar $ \connections -> do
    let existingEntryO = M.lookup gameId connections
    case existingEntryO of
      Nothing -> mkVsConnection >>= flip register connections
      Just entry -> register entry connections
 where
  register entry connections = do
    outChan <- atomically $ dupTChan entry.broadcaster
    let updatedEntry = case team of
          White -> entry & #whiteClients +~ 1
          Black -> entry & #blackClients +~ 1
    let updatedConnections = M.insert gameId updatedEntry connections
    pure (updatedConnections, outChan)

disconnectFromVs :: UUID -> Team -> MVar (Map UUID VsConnection) -> IO ()
disconnectFromVs gameId team connectionsVar = modifyMVar_ connectionsVar $ \connections ->
  pure $ M.alter f gameId connections
 where
  f Nothing = Nothing
  f (Just entry) =
    let updatedEntry = case team of
          White -> entry & #whiteClients -~ 1
          Black -> entry & #blackClients -~ 1
     in if updatedEntry.whiteClients + updatedEntry.blackClients == 0
          then Nothing
          else Just updatedEntry

----------------------------------------

startVsHandler :: Bool -> AppM (Html ())
startVsHandler initiatorIsBlack = do
  uuid <- liftIO nextRandom
  putStrLn $ "new game id: " <> show uuid
  conn <- liftIO $ open "db.db"
  let newGame = Game uuid startBoard True

  whiteId <- liftIO nextRandom
  blackId <- liftIO nextRandom
  liftIO $ createVs conn newGame whiteId blackId

  let teamUrlPart = if initiatorIsBlack then "black/" else "white/"
  throwError
    err302
      { errHeaders =
          [("Location", "/vs/" <> teamUrlPart <> show uuid)]
      }

vsGameToGameState :: VsGame -> VS.GameState
vsGameToGameState game =
  VS.GameState
    { board = game.board
    , isBlackTurn = game.isBlackTurn
    , moves =
        if game.isBlackTurn
          then Hotseat.getBlackMoves game.board
          else Hotseat.getWhiteMoves game.board
    , messages = []
    }

--vsHandler :: Team -> UUID -> AppM (Html ())
--vsHandler team aiId = do
--  conn <- liftIO $ open "db.db"
--  vsGame <- liftIO $ selectVs conn (team == Black) aiId
--  return $ vsPage aiId $ vsGameToGameState vsGame

--------------------------------------------------------------------------------
