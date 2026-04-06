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
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Concurrent.STM qualified as STM
import Effectful.Exception (finally)
import Hnefatafl.App.Online.Serialization (
  actorNotificationToJSON,
  gameStateToJSON,
  notificationToJSON,
 )
import Hnefatafl.App.Session (
  SessionEntry (..),
  insertOrAcquire,
  release,
  tryAcquire,
 )
import Hnefatafl.App.Storage (gameMoveToAppliedMoves, persistenceCommandsToTx)
import Hnefatafl.App.WebSocket (decodeAuthToken, errorToJSON, guardWebSocket, safeSend)
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
import Network.WebSockets (Connection)
import StmContainers.Map qualified as STMMap

-------------------------------------------------------------------------------
-- Types

newtype ConnectionId = ConnectionId Text
  deriving (Eq)

-- | In-memory session for an active online game. Connections are wrapped
-- in MVars to ensure thread-safe sends — only one thread can write to a
-- WebSocket connection at a time.
data GameSession = GameSession
  { gameState :: Online.State
  , whiteConn :: Maybe (ConnectionId, MVar Connection)
  , blackConn :: Maybe (ConnectionId, MVar Connection)
  }

type GameSessions = STMMap.Map GameId (SessionEntry GameSession)

-- Convenience helpers

setConn ::
  PlayerColor ->
  Maybe (ConnectionId, MVar Connection) ->
  GameSession ->
  GameSession
setConn White val session = session{whiteConn = val}
setConn Black val session = session{blackConn = val}

clearConn :: PlayerColor -> ConnectionId -> GameSession -> GameSession
clearConn color uid session =
  case getConn color session of
    Just (u, _) | u == uid -> setConn color Nothing session
    _ -> session

getConn :: PlayerColor -> GameSession -> Maybe (ConnectionId, MVar Connection)
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

-- | Get or create a session. If a session already exists in the map,
-- acquires it (fast path, no DB load). Otherwise loads from DB, creates
-- a full MVar, and inserts it. If two threads race on creation, one
-- wins and the other's work is discarded (optimistic concurrency).
getOrCreateSession ::
  (Storage :> es, Concurrent :> es) =>
  GameSessions ->
  GameId ->
  Eff es (MVar GameSession)
getOrCreateSession sessions gameId = do
  mVar <- STM.atomically $ tryAcquire gameId sessions
  case mVar of
    Just var -> pure var
    Nothing -> do
      gameState <- runTransaction $ loadOnlineState gameId
      var <- MVar.newMVar (GameSession gameState Nothing Nothing)
      STM.atomically $ insertOrAcquire var gameId sessions

-- | Register a connection in the session. Wraps the raw Connection in an
-- MVar and returns it alongside the game state for initial send.
connectPlayer ::
  Concurrent :> es =>
  MVar GameSession ->
  PlayerColor ->
  ConnectionId ->
  Connection ->
  Eff es (MVar Connection, Online.State)
connectPlayer sessionVar color uid conn = do
  connVar <- MVar.newMVar conn
  MVar.modifyMVar sessionVar $ \session ->
    let session' = setConn color (Just (uid, connVar)) session
     in pure (session', (connVar, session'.gameState))

-- | Unregister a connection from the session. Only clears if the uid
-- matches, so a stale finally from a reconnected player won't remove
-- the new connection. Map lifecycle is handled separately by
-- release in the finally block.
disconnectPlayer ::
  Concurrent :> es =>
  MVar GameSession ->
  PlayerColor ->
  ConnectionId ->
  Eff es ()
disconnectPlayer sessionVar color uid =
  MVar.modifyMVar_ sessionVar $ \session ->
    pure $ clearConn color uid session

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
  -- All effects (DB writes, notifications) run under the MVar lock so that
  -- no concurrent event can observe or act on intermediate state.
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
sendToPlayer ::
  (Concurrent :> es, WebSocket :> es) =>
  GameSession -> PlayerColor -> LByteString -> Eff es ()
sendToPlayer session color msg =
  for_ (getConn color session) $ \(_, connVar) ->
    safeSend connVar msg

-------------------------------------------------------------------------------
-- WebSocket handler

handleWebSocket ::
  (Storage :> es, Clock :> es, IdGen :> es, Concurrent :> es, WebSocket :> es, IOE :> es) =>
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
        Just tok -> guardWebSocket conn $ do
          let gameId = tok.gameId
              color = tok.role
          sessionVar <- getOrCreateSession sessions gameId
          uid <- generateId
          (connVar, gameState) <- connectPlayer sessionVar color uid conn
          safeSend connVar (gameStateToJSON gameId gameState)
          -- Receive loop with disconnect cleanup
          receiveLoop sessionVar gameId color connVar
            `finally` do
              disconnectPlayer sessionVar color uid
              STM.atomically $ release gameId sessions

-- | Read messages from the WebSocket and process them.
receiveLoop ::
  (Storage :> es, Clock :> es, Concurrent :> es, WebSocket :> es) =>
  MVar GameSession ->
  GameId ->
  PlayerColor ->
  MVar Connection ->
  Eff es ()
receiveLoop sessionVar gameId color connVar = do
  conn <- MVar.readMVar connVar
  forever $ do
    msg <- receiveData conn
    case decode msg of
      Nothing -> safeSend connVar (errorToJSON "invalid message")
      Just clientMsg -> processEvent sessionVar gameId color clientMsg
