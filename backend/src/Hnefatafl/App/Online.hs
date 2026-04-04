{-# LANGUAGE BlockArguments #-}

module Hnefatafl.App.Online (
  GameSession (..),
  GameSessions,
  CreateGameResult (..),
  ClientMessage (..),
  toEvent,
  createGame,
  getOrCreateSession,
  processEvent,
  connectPlayer,
  disconnectPlayer,
  handleWebSocket,
) where

import Chronos (Time)
import Data.Aeson (
  FromJSON (..),
  decode,
  withObject,
  (.:),
 )
import Data.Aeson.Types (Parser)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Concurrent.STM qualified as STM
import Effectful.Exception (catch, finally, onException)
import Hnefatafl.App.Online.Serialization (
  actorNotificationToJSON,
  gameStateToJSON,
  notificationToJSON,
 )
import Hnefatafl.App.Session (getOrInsert)
import Hnefatafl.App.Storage (gameMoveToAppliedMoves, persistenceCommandsToTx)
import Hnefatafl.App.WebSocket (decodeAuthToken, errorToJSON)
import Hnefatafl.Core.Data (
  Game (..),
  GameId (..),
  GameMode (..),
  GameParticipantToken (..),
  GameParticipantTokenId (..),
  PlayerColor (..),
 )
import Hnefatafl.Core.Data qualified as Data
import Hnefatafl.Effect.Clock (Clock, now)
import Hnefatafl.Effect.IdGen (IdGen, generateId)
import Hnefatafl.Effect.Storage (
  Storage,
  StorageTx,
  createGameParticipantToken,
  getGame,
  getMovesForGame,
  getPendingAction,
  getTokenByText,
  insertGame,
  runTransaction,
 )
import Hnefatafl.Effect.WebSocket (WebSocket, receiveData, sendData)
import Hnefatafl.Game.Common (
  currentBoard,
  opponent,
 )
import Hnefatafl.Game.Online qualified as Online
import Network.WebSockets (Connection, ConnectionException)
import StmContainers.Map qualified as STMMap

-------------------------------------------------------------------------------
-- Types

newtype ConnectionId = ConnectionId Text
  deriving (Eq)

data GameSession = GameSession
  { gameState :: Online.State
  , whiteConn :: Maybe (ConnectionId, Connection)
  , blackConn :: Maybe (ConnectionId, Connection)
  }

type GameSessions = STMMap.Map GameId (MVar GameSession)

-- Convenience helpers

setConn ::
  PlayerColor -> Maybe (ConnectionId, Connection) -> GameSession -> GameSession
setConn White val session = session{whiteConn = val}
setConn Black val session = session{blackConn = val}

clearConn :: PlayerColor -> ConnectionId -> GameSession -> GameSession
clearConn color uid session =
  case getConn color session of
    Just (u, _) | u == uid -> setConn color Nothing session
    _ -> session

getConn :: PlayerColor -> GameSession -> Maybe (ConnectionId, Connection)
getConn White session = session.whiteConn
getConn Black session = session.blackConn

-------------------------------------------------------------------------------
-- Messages from the client

data ClientMessage
  = Move Data.Move
  | Resign
  | OfferDraw
  | AcceptDraw
  | DeclineDraw
  | RequestUndo
  | AcceptUndo
  | DeclineUndo
  deriving (Show, Eq)

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "move" -> Move <$> (Data.Move <$> o .: "orig" <*> o .: "dest")
      "resign" -> pure Resign
      "offer_draw" -> pure OfferDraw
      "accept_draw" -> pure AcceptDraw
      "decline_draw" -> pure DeclineDraw
      "request_undo" -> pure RequestUndo
      "accept_undo" -> pure AcceptUndo
      "decline_undo" -> pure DeclineUndo
      _ -> fail $ "unknown client message type: " <> toString typ

-- | Convert a client message to an Online event, adding the player's
-- color and the current time.
toEvent :: PlayerColor -> Time -> ClientMessage -> Online.Event
toEvent color time = \case
  Move move -> Online.MakeMove color move time
  Resign -> Online.Resign color
  OfferDraw -> Online.OfferDraw color
  AcceptDraw -> Online.AcceptDraw color
  DeclineDraw -> Online.DeclineDraw color
  RequestUndo -> Online.RequestUndo color
  AcceptUndo -> Online.AcceptUndo color
  DeclineUndo -> Online.DeclineUndo color

-------------------------------------------------------------------------------
-- Storage

-- | Load online game state from the database
loadOnlineState :: GameId -> StorageTx Online.State
loadOnlineState gameId = do
  game <- getGame gameId
  gameMoves <- getMovesForGame gameId
  pendingAction <- getPendingAction gameId
  let appliedMoves = gameMoveToAppliedMoves gameMoves
      board = currentBoard appliedMoves
  pure $
    Online.reconstruct
      board
      appliedMoves
      game.outcome
      pendingAction

mkGame :: GameId -> Time -> Game
mkGame gameId time =
  Game
    { gameId = gameId
    , name = Nothing
    , mode = Online Nothing Nothing
    , startTime = time
    , endTime = Nothing
    , outcome = Nothing
    , createdAt = time
    }

data CreateGameResult = CreateGameResult
  { game :: Game
  , whiteToken :: GameParticipantToken
  , blackToken :: GameParticipantToken
  }

-- | Create a new online game in the database with tokens for both players.
-- Does NOT create a session in the STMMap (lazy creation on WS connect).
createGame ::
  (Storage :> es, Clock :> es, IdGen :> es) =>
  Eff es CreateGameResult
createGame = do
  game <- mkGame <$> generateId <*> now
  whiteTokenId <- generateId
  blackTokenId <- generateId
  whiteTokenText <- generateId @Text
  blackTokenText <- generateId @Text
  let whiteToken =
        GameParticipantToken
          { tokenId = whiteTokenId
          , gameId = game.gameId
          , token = whiteTokenText
          , role = White
          }
      blackToken =
        GameParticipantToken
          { tokenId = blackTokenId
          , gameId = game.gameId
          , token = blackTokenText
          , role = Black
          }
  runTransaction $ do
    insertGame game
    createGameParticipantToken whiteToken
    createGameParticipantToken blackToken
  pure CreateGameResult{game, whiteToken, blackToken}

-------------------------------------------------------------------------------
-- Session management

-- | Get or create a session in the STMMap. If not found, atomically
-- inserts an empty MVar as a placeholder atomically then fills it from DB.
-- Concurrent callers for the same gameId get the same MVar and block
-- until it's filled.
getOrCreateSession ::
  (Storage :> es, Concurrent :> es) =>
  GameSessions ->
  GameId ->
  Eff es (MVar GameSession)
getOrCreateSession sessions gameId = do
  emptyVar <- MVar.newEmptyMVar
  (var, isNew) <- STM.atomically $ getOrInsert emptyVar gameId sessions
  when isNew $
    onException
      do
        gameState <- runTransaction $ loadOnlineState gameId
        MVar.putMVar var (GameSession gameState Nothing Nothing)
      do
        STM.atomically $ STMMap.delete gameId sessions
        void $ MVar.tryPutMVar var (error "session failed to load")
  pure var

-- | Register a connection in the session. Returns the current game state
-- for sending to the connecting player.
connectPlayer ::
  Concurrent :> es =>
  MVar GameSession ->
  PlayerColor ->
  ConnectionId ->
  Connection ->
  Eff es Online.State
connectPlayer sessionVar color uid conn =
  MVar.modifyMVar sessionVar $ \session ->
    let session' = setConn color (Just (uid, conn)) session
     in pure (session', session'.gameState)

-- | Unregister a connection. Deletes the session from the map if both
-- players are now disconnected.
disconnectPlayer ::
  Concurrent :> es =>
  GameSessions ->
  GameId ->
  MVar GameSession ->
  PlayerColor ->
  ConnectionId ->
  Eff es ()
disconnectPlayer sessions gameId sessionVar color uid =
  MVar.modifyMVar_ sessionVar $ \session ->
    -- clearConn first: it only clears if the uid matches, so a stale
    -- finally from a reconnected player won't remove the new connection
    -- or trigger a spurious session delete.
    let session' = clearConn color uid session
     in if isNothing session'.whiteConn && isNothing session'.blackConn
          then STM.atomically (STMMap.delete gameId sessions) $> session'
          else pure session'

-------------------------------------------------------------------------------
-- Event processing

processEvent ::
  (Storage :> es, Clock :> es, Concurrent :> es, WebSocket :> es) =>
  MVar GameSession ->
  GameId ->
  PlayerColor ->
  ClientMessage ->
  Eff es ()
processEvent sessionVar gameId color clientMsg = do
  currentTime <- now
  let event = toEvent color currentTime clientMsg
  MVar.modifyMVar_ sessionVar $ \session ->
    case Online.transition session.gameState event of
      Left err -> do
        sendToPlayer session color (errorToJSON $ show err)
        pure session
      Right tr -> do
        let persistCmds = [cmd | Online.Persist cmd <- tr.commands]
        runTransaction $ persistenceCommandsToTx gameId currentTime persistCmds
        -- Send notifications to opponent
        let opponentColor = opponent color
        for_ [n | Online.NotifyOpponent n <- tr.commands] $
          sendToPlayer session opponentColor . notificationToJSON tr.newState
        -- Send actor notifications
        for_ [n | Online.NotifyActor n <- tr.commands] $
          sendToPlayer session color . actorNotificationToJSON tr.newState
        pure session{gameState = tr.newState}

-- | Send a message to a player's WebSocket connection, if connected.
-- Silently catches connection exceptions.
sendToPlayer ::
  WebSocket :> es => GameSession -> PlayerColor -> LByteString -> Eff es ()
sendToPlayer session color msg =
  for_ (getConn color session) $ \(_, conn) ->
    sendData conn msg
      `catch` \(_ :: ConnectionException) -> pure ()

-------------------------------------------------------------------------------
-- WebSocket handler

handleWebSocket ::
  (Storage :> es, Clock :> es, IdGen :> es, Concurrent :> es, WebSocket :> es) =>
  GameSessions ->
  Connection ->
  Eff es ()
handleWebSocket sessions conn = do
  authMsg <- receiveData conn
  case decodeAuthToken authMsg of
    Nothing -> sendData conn (errorToJSON "invalid auth message")
    Just tokenText -> do
      mToken <- runTransaction $ getTokenByText tokenText
      case mToken of
        Nothing -> sendData conn (errorToJSON "invalid token")
        Just tok -> do
          let gameId = tok.gameId
              color = tok.role
          sessionVar <- getOrCreateSession sessions gameId
          uid <- generateId
          gameState <- connectPlayer sessionVar color uid conn
          -- Send initial game state
          sendData conn (gameStateToJSON gameId gameState)
          -- Receive loop with disconnect cleanup
          receiveLoop sessionVar gameId color conn
            `finally` disconnectPlayer sessions gameId sessionVar color uid

-- | Read messages from the WebSocket and process them.
receiveLoop ::
  (Storage :> es, Clock :> es, Concurrent :> es, WebSocket :> es) =>
  MVar GameSession ->
  GameId ->
  PlayerColor ->
  Connection ->
  Eff es ()
receiveLoop sessionVar gameId color conn = forever $ do
  msg <- receiveData conn
  case decode msg of
    Nothing -> sendData conn (errorToJSON "invalid message")
    Just clientMsg -> processEvent sessionVar gameId color clientMsg
