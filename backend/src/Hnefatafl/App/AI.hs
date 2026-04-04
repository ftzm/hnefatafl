{-# LANGUAGE BlockArguments #-}

module Hnefatafl.App.AI (
  GameSession (..),
  GameSessions,
  CreateGameResult (..),
  ClientMessage (..),
  toEvent,
  createGame,
  connectToGame,
  processEvent,
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
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Concurrent.STM qualified as STM
import Effectful.Exception (catch, finally, onException)
import Hnefatafl.App.AI.Serialization (gameStateToJSON, notificationToJSON)
import Hnefatafl.App.Session (getOrInsert)
import Hnefatafl.App.Storage (gameMoveToAppliedMoves, persistenceCommandsToTx)
import Hnefatafl.App.WebSocket (decodeAuthToken, errorToJSON)
import Hnefatafl.Bindings (SearchTrustedResult (..))
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
import Hnefatafl.Effect.Search (Search, searchTrusted)
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
import Hnefatafl.Game.AI qualified as AI
import Hnefatafl.Game.Common (
  AppliedMove (..),
  currentBoard,
  currentTurn,
  opponent,
  outcomeFromEngine,
  zobristHashes,
 )
import Hnefatafl.Search (SearchTimeout (..))
import Network.WebSockets (Connection, ConnectionException)
import StmContainers.Map qualified as STMMap

-------------------------------------------------------------------------------
-- Types

newtype ConnectionId = ConnectionId Text
  deriving (Eq)

data GameSession = GameSession
  { gameState :: AI.State
  , humanColor :: PlayerColor
  , playerConn :: (ConnectionId, Connection)
  , engineAsync :: Maybe (Async.Async ())
  }

type GameSessions = STMMap.Map GameId (MVar GameSession)

-------------------------------------------------------------------------------
-- Messages from the client

data ClientMessage
  = Move Data.Move
  | Undo
  | Resign
  | OfferDraw
  | AcceptDraw
  | DeclineDraw
  deriving (Show, Eq)

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "move" -> Move <$> (Data.Move <$> o .: "orig" <*> o .: "dest")
      "undo" -> pure Undo
      "resign" -> pure Resign
      "offer_draw" -> pure OfferDraw
      "accept_draw" -> pure AcceptDraw
      "decline_draw" -> pure DeclineDraw
      _ -> fail $ "unknown client message type: " <> toString typ

-- | Convert a client message to an AI event, adding the current time
-- and player color where needed.
toEvent :: PlayerColor -> Time -> ClientMessage -> AI.Event
toEvent humanColor time = \case
  Move move -> AI.MakeMove move time
  Undo -> AI.Undo
  Resign -> AI.Resign humanColor
  OfferDraw -> AI.OfferDraw humanColor
  AcceptDraw -> AI.AcceptDraw humanColor
  DeclineDraw -> AI.DeclineDraw humanColor

-------------------------------------------------------------------------------
-- Storage

-- | Load AI game state from the database
loadAIState :: PlayerColor -> GameId -> StorageTx AI.State
loadAIState humanColor gameId = do
  game <- getGame gameId
  gameMoves <- getMovesForGame gameId
  pendingAction <- getPendingAction gameId
  let appliedMoves = gameMoveToAppliedMoves gameMoves
      board = currentBoard appliedMoves
  pure $
    AI.reconstruct
      humanColor
      board
      appliedMoves
      game.outcome
      pendingAction

mkGame :: GameId -> Time -> PlayerColor -> Game
mkGame gameId time humanColor =
  Game
    { gameId = gameId
    , name = Nothing
    , mode = VsAI Nothing humanColor (Data.PlayerId "engine")
    , startTime = time
    , endTime = Nothing
    , outcome = Nothing
    , createdAt = time
    }

data CreateGameResult = CreateGameResult
  { game :: Game
  , token :: GameParticipantToken
  }

-- | Create a new AI game in the database with a token for the human player.
createGame ::
  (Storage :> es, Clock :> es, IdGen :> es) =>
  PlayerColor ->
  Eff es CreateGameResult
createGame humanColor = do
  game <- mkGame <$> generateId <*> now <*> pure humanColor
  tokenId <- generateId
  tokenText <- generateId @Text
  let token =
        GameParticipantToken
          { tokenId = tokenId
          , gameId = game.gameId
          , token = tokenText
          , role = humanColor
          }
  runTransaction $ do
    insertGame game
    createGameParticipantToken token
  pure CreateGameResult{game, token}

-------------------------------------------------------------------------------
-- Session management

-- | Connect to an AI game. Gets or creates the session in the STMMap,
-- sets the connection, and re-triggers engine search if needed (e.g.
-- reconnect during EngineThinking). Returns the MVar handle and the
-- current game state.
connectToGame ::
  (Storage :> es, Search :> es, Clock :> es, Concurrent :> es, WebSocket :> es) =>
  GameSessions ->
  GameId ->
  PlayerColor ->
  ConnectionId ->
  Connection ->
  Eff es (MVar GameSession, AI.State)
connectToGame sessions gameId humanColor uid conn = do
  emptyVar <- MVar.newEmptyMVar
  (var, isNew) <- STM.atomically $ getOrInsert emptyVar gameId sessions
  if isNew
    then onException
      do
        gameState <- runTransaction $ loadAIState humanColor gameId
        let session = GameSession gameState humanColor (uid, conn) Nothing
        -- If engine was thinking (e.g. server restart), re-trigger the search
        case gameState of
          AI.State _ moves (AI.EngineThinking _) -> do
            searchAsync <- spawnEngineSearch var gameId humanColor moves
            MVar.putMVar var session{engineAsync = Just searchAsync}
          _ -> MVar.putMVar var session
        pure (var, gameState)
      do
        STM.atomically $ STMMap.delete gameId sessions
        void $ MVar.tryPutMVar var (error "session failed to load")
    else do
      -- Existing session (e.g. quick reconnect before old finally ran)
      gameState <- MVar.modifyMVar var $ \session -> do
        let session' = session{playerConn = (uid, conn)}
        pure (session', session'.gameState)
      pure (var, gameState)

-- | Disconnect from an AI game. If the uid matches the current connection,
-- cancels any engine search and deletes the session. Stale disconnects
-- (uid mismatch from a previous connection) are ignored.
disconnectPlayer ::
  Concurrent :> es =>
  GameSessions ->
  GameId ->
  MVar GameSession ->
  ConnectionId ->
  Eff es ()
disconnectPlayer sessions gameId sessionVar uid =
  MVar.modifyMVar_ sessionVar $ \session ->
    if fst session.playerConn == uid
      then do
        for_ session.engineAsync Async.cancel
        STM.atomically $ STMMap.delete gameId sessions
        pure session
      else pure session

-------------------------------------------------------------------------------
-- Event processing

-- | Process a game event. Transitions state, persists to DB, sends
-- notifications to the player, and handles engine search commands.
processEvent ::
  (Storage :> es, Clock :> es, Search :> es, Concurrent :> es, WebSocket :> es) =>
  MVar GameSession ->
  GameId ->
  AI.Event ->
  Eff es ()
processEvent sessionVar gameId event = do
  currentTime <- now
  MVar.modifyMVar_ sessionVar $ \session -> do
    case AI.transition session.humanColor session.gameState event of
      Left err -> do
        sendToPlayer session (errorToJSON $ show err)
        pure session
      Right tr -> do
        let persistCmds = [cmd | AI.Persist cmd <- tr.commands]
        runTransaction $ persistenceCommandsToTx gameId currentTime persistCmds
        -- Send player notifications
        for_ [n | AI.NotifyPlayer n <- tr.commands] $
          sendToPlayer session . notificationToJSON tr.newState
        -- Handle engine search commands
        engineAsync' <- handleEngineCommands sessionVar gameId session tr.commands
        -- Cancel previous engine search if a new one was triggered or cancelled
        let shouldCancel = any isCancelOrTrigger tr.commands
        when shouldCancel $
          for_ session.engineAsync Async.cancel
        pure session{gameState = tr.newState, engineAsync = engineAsync'}
 where
  isCancelOrTrigger AI.CancelEngineSearch = True
  isCancelOrTrigger (AI.TriggerEngineSearch _) = True
  isCancelOrTrigger _ = False

-- | Handle engine-related commands from a transition result.
-- Returns the new engine async handle (if a search was triggered).
handleEngineCommands ::
  (Search :> es, Clock :> es, Concurrent :> es, WebSocket :> es, Storage :> es) =>
  MVar GameSession ->
  GameId ->
  GameSession ->
  [AI.Command] ->
  Eff es (Maybe (Async.Async ()))
handleEngineCommands sessionVar gameId session commands =
  case [moves | AI.TriggerEngineSearch moves <- commands] of
    (moves : _) -> Just <$> spawnEngineSearch sessionVar gameId session.humanColor moves
    [] -> pure Nothing

-- | Spawn an async thread to run the engine search. When the search
-- completes, it feeds an EngineMove event back through processEvent.
spawnEngineSearch ::
  (Search :> es, Clock :> es, Concurrent :> es, WebSocket :> es, Storage :> es) =>
  MVar GameSession ->
  GameId ->
  PlayerColor ->
  [AppliedMove] ->
  Eff es (Async.Async ())
spawnEngineSearch sessionVar gameId humanColor moves =
  Async.async $
    doSearch `catch` \(ex :: SomeException) ->
      MVar.withMVar sessionVar $ \session ->
        sendToPlayer session (errorToJSON $ "Engine search failed: " <> show ex)
 where
  doSearch = do
    let board = currentBoard moves
        engineColor = opponent humanColor
        blackToMove = currentTurn moves == Black
        hashes = zobristHashes moves
        timeout = SearchTimeout 5000 -- 5 second search
    result <- searchTrusted board blackToMove hashes timeout False
    time <- now
    let applied =
          AppliedMove
            { move = result.searchMove
            , side = engineColor
            , captures = result.captures
            , boardAfter = result.updatedBoard
            , zobristHash = result.updatedZobristHash
            , timestamp = time
            }
        maybeOutcome = outcomeFromEngine result.gameStatus
    processEvent sessionVar gameId (AI.EngineMove applied maybeOutcome)

-- | Send a message to the player's WebSocket connection.
-- Silently catches connection exceptions.
sendToPlayer :: WebSocket :> es => GameSession -> LByteString -> Eff es ()
sendToPlayer session msg =
  let (_, conn) = session.playerConn
   in sendData conn msg
        `catch` \(_ :: ConnectionException) -> pure ()

-------------------------------------------------------------------------------
-- WebSocket handler

-- | Handle a WebSocket connection for an AI game.
handleWebSocket ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Search :> es
  , Concurrent :> es
  , WebSocket :> es
  ) =>
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
              humanColor = tok.role
          uid <- generateId
          (sessionVar, gameState) <- connectToGame sessions gameId humanColor uid conn
          sendData conn (gameStateToJSON gameId humanColor gameState)
          receiveLoop sessionVar gameId humanColor conn
            `finally` disconnectPlayer sessions gameId sessionVar uid

-- | Read messages from the WebSocket and process them.
receiveLoop ::
  (Storage :> es, Clock :> es, Search :> es, Concurrent :> es, WebSocket :> es) =>
  MVar GameSession ->
  GameId ->
  PlayerColor ->
  Connection ->
  Eff es ()
receiveLoop sessionVar gameId humanColor conn = forever $ do
  msg <- receiveData conn
  case decode msg of
    Nothing -> sendData conn (errorToJSON "invalid message")
    Just clientMsg -> do
      time <- now
      processEvent sessionVar gameId (toEvent humanColor time clientMsg)
