{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.App.AI (
  GameSession (..),
  GameSessions,
  CreateGameResult (..),
  toEvent,
  createGame,
  connectToGame,
  disconnectPlayer,
  handleWebSocket,
) where

import Chronos (Time)
import Data.Aeson (
  encode,
 )
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Concurrent.STM qualified as STM
import Effectful.Exception (bracket, catch, catchSync, onException)
import Effectful.Katip (KatipE, katipAddNamespace, logTM)
import Hnefatafl.Api.Types (Position (..))
import Hnefatafl.Api.Types.WS (
  WsError (..),
  WsErrorCode (..),
  transitionErrorToWsError,
 )
import Hnefatafl.Api.Types.WS.AI (AIClientMessage (..))
import Hnefatafl.App.AI.Serialization (gameStateMessage, notificationsFor)
import Hnefatafl.App.Session (
  SessionEntry (..),
  insertOrAcquire,
  release,
  tryAcquire,
 )
import Hnefatafl.App.Storage (gameMoveToAppliedMoves, persistEvents)
import Hnefatafl.App.WebSocket (
  authenticateWebSocket,
  guardWebSocket,
  runMessageLoop,
  safeSend,
  withGameContext,
 )
import Hnefatafl.Bindings (SearchTrustedResult (..))
import Hnefatafl.Core.Data (
  Game (..),
  GameId (..),
  GameMode (..),
  GameParticipantToken (..),
  GameParticipantTokenId (..),
  Outcome,
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
  insertGame,
  runTransaction,
 )
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Hnefatafl.Game.AI qualified as AI
import Hnefatafl.Game.Common (
  AppliedMove (..),
  currentBoard,
  currentTurn,
  opponent,
  outcomeFromEngine,
  zobristHashes,
 )
import Hnefatafl.Game.Common qualified as Common
import Hnefatafl.Metrics (
  HMetrics,
  Hs (..),
  decGauge,
  incGauge,
  increaseLabelledCounter,
  recordMetrics,
 )
import Hnefatafl.Search (SearchTimeout (..))
import Katip (Severity (..), ls)
import Network.WebSockets (Connection)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import StmContainers.Map qualified as STMMap

-------------------------------------------------------------------------------
-- Types

newtype ConnectionId = ConnectionId Text
  deriving (Eq)

-- | In-memory session for an active AI game. The connection is wrapped
-- in an MVar to ensure thread-safe sends — only one thread can write to
-- the WebSocket connection at a time.
data GameSession = GameSession
  { gameState :: AI.State
  , humanColor :: PlayerColor
  , playerConn :: (ConnectionId, MVar Connection)
  , engineAsync :: Maybe (Async.Async ())
  }
  deriving (Generic)

type GameSessions = STMMap.Map GameId (SessionEntry GameSession)

-------------------------------------------------------------------------------
-- Client message conversion

-- | Convert an API client message to an AI event, adding the current time
-- and player color where needed.
toEvent :: PlayerColor -> Time -> AIClientMessage -> AI.Event
toEvent humanColor time = \case
  AIMove (Position from) (Position to) ->
    AI.MakeMove (Data.Move (fromIntegral from) (fromIntegral to)) time
  AIUndo -> AI.Undo
  AIResign -> AI.Resign humanColor
  AIOfferDraw -> AI.OfferDraw humanColor
  AIAcceptDraw -> AI.AcceptDraw humanColor
  AIDeclineDraw -> AI.DeclineDraw humanColor

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
  (Storage :> es, Clock :> es, IdGen :> es, Trace :> es, HMetrics :> es) =>
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
  increaseLabelledCounter gamesCreated "ai"
  pure CreateGameResult{game, token}

-------------------------------------------------------------------------------
-- Session management

-- | Connect to an AI game. If a session exists, acquires it and updates
-- the connection. Otherwise loads from DB, creates the session, and
-- re-triggers engine search if needed (e.g. reconnect during
-- EngineThinking). Returns the MVar handle and the current game state.
connectToGame ::
  ( Storage :> es
  , Search :> es
  , Clock :> es
  , Concurrent :> es
  , IOE :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameSessions ->
  GameId ->
  PlayerColor ->
  ConnectionId ->
  Connection ->
  Eff es (MVar GameSession, MVar Connection, AI.State)
connectToGame sessions gameId humanColor uid conn = do
  connVar <- MVar.newMVar conn
  -- After 'tryAcquire' / 'insertOrAcquire' commits a refcount+1, the
  -- subsequent MVar mutation (and engine-search spawn, in the fresh path)
  -- are not atomic with the STM transaction. If they fail, we must
  -- explicitly release the refcount before rethrowing; otherwise the
  -- session's entry in the map is leaked. The outer 'bracket' in
  -- 'handleWebSocket' only handles cleanup when 'connectToGame' has
  -- returned successfully, so this inner protection is required.
  let releaseRefcount = STM.atomically $ release gameId sessions
  mVar <- STM.atomically $ tryAcquire gameId sessions
  case mVar of
    Just var ->
      ( do
          -- Existing session (e.g. quick reconnect before old finally ran)
          gameState <- MVar.modifyMVar var $ \session ->
            let session' = session{playerConn = (uid, connVar)}
             in pure (session', session'.gameState)
          pure (var, connVar, gameState)
      )
        `onException` releaseRefcount
    Nothing -> do
      gameState <- runTransaction $ loadAIState humanColor gameId
      let session = GameSession gameState humanColor (uid, connVar) Nothing
      var <- MVar.newMVar session
      sessionVar <- STM.atomically $ insertOrAcquire var gameId sessions
      ( do
          -- If engine was thinking (e.g. server restart), re-trigger the search
          when (sessionVar == var) $
            case gameState of
              AI.State _ ms (AI.EngineThinking _) -> do
                searchAsync <-
                  spawnEngineSearch sessionVar gameId humanColor ms
                MVar.modifyMVar_ sessionVar $
                  \s -> pure s{engineAsync = Just searchAsync}
              _ -> pure ()
          pure (sessionVar, connVar, gameState)
        )
        `onException` releaseRefcount

-- | Disconnect from an AI game. If the uid matches the current connection,
-- cancels any engine search. Stale disconnects (uid mismatch from a
-- previous connection) are ignored. Map lifecycle is handled separately
-- by release in the finally block.
disconnectPlayer ::
  Concurrent :> es =>
  MVar GameSession ->
  ConnectionId ->
  Eff es ()
disconnectPlayer sessionVar uid =
  MVar.modifyMVar_ sessionVar $ \session ->
    if fst session.playerConn == uid
      then do
        for_ session.engineAsync Async.cancel
        pure session
      else pure session

-------------------------------------------------------------------------------
-- Event processing

-- | Process a game event. Transitions state, persists to DB, sends
-- notifications to the player, and handles engine search commands.
-- | Shared transition logic: take MVar, run state machine, handle
-- errors, persist, notify, record metrics. On success, delegate
-- session update to the caller-provided callback.
withTransition ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  MVar GameSession ->
  GameId ->
  AI.Event ->
  (GameSession -> AI.State -> Eff es GameSession) ->
  Eff es ()
withTransition sessionVar gameId event onSuccess = do
  currentTime <- now
  MVar.modifyMVar_ sessionVar $ \session ->
    case AI.transition session.humanColor session.gameState event of
      Left err -> do
        when (err == Common.InvalidMove) $
          increaseLabelledCounter invalidMovesTotal "ai"
        sendToPlayer (encode $ transitionErrorToWsError err) session
        pure session
      Right (AI.TransitionResult newState events) -> do
        runTransaction $ persistEvents gameId currentTime events
        sendNotifications session.humanColor newState events session
        recordMetrics "ai" events
        onSuccess session newState

-- | Process a player event. Manages engine lifecycle: cancels the
-- engine search when leaving EngineThinking, spawns one when entering.
processEvent ::
  ( Storage :> es
  , Clock :> es
  , Search :> es
  , Concurrent :> es
  , IOE :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  MVar GameSession ->
  GameId ->
  AI.Event ->
  Eff es ()
processEvent sessionVar gameId event =
  withTransition sessionVar gameId event $ \session newState -> do
    let wasThinking = isEngineThinking session.gameState
        nowThinking = isEngineThinking newState
    when (wasThinking && not nowThinking) $
      for_ session.engineAsync Async.cancel
    engineAsync' <-
      if not wasThinking && nowThinking
        then Just <$> spawnEngineSearch sessionVar gameId session.humanColor newState.moves
        else pure Nothing
    pure session{gameState = newState, engineAsync = engineAsync'}

-- | Commit the engine's search result. The engine thread is already
-- finishing, so no lifecycle management is needed.
deliverEngineResult ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  MVar GameSession ->
  GameId ->
  AppliedMove ->
  Maybe Outcome ->
  Eff es ()
deliverEngineResult sessionVar gameId applied maybeOutcome =
  withTransition sessionVar gameId (AI.EngineMove applied maybeOutcome) $ \_session newState ->
    pure _session{gameState = newState, engineAsync = Nothing}

isEngineThinking :: AI.State -> Bool
isEngineThinking (AI.State _ _ (AI.EngineThinking _)) = True
isEngineThinking _ = False

-- | Spawn an async thread to run the engine search. When the search
-- completes, it delivers the result via 'deliverEngineResult'.
spawnEngineSearch ::
  ( Search :> es
  , Clock :> es
  , Concurrent :> es
  , IOE :> es
  , WebSocket :> es
  , Storage :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  MVar GameSession ->
  GameId ->
  PlayerColor ->
  [AppliedMove] ->
  Eff es (Async.Async ())
spawnEngineSearch sessionVar gameId humanColor moves = do
  -- hs-opentelemetry uses a thread-local IORef for span context, which is
  -- not inherited when we fork. Capture the parent context here so we can
  -- restore it inside the forked thread — otherwise the engine.search span
  -- created by 'runSearchLocal' would be a disconnected root rather than a
  -- child of the message span that triggered it.
  parentCtx <- liftIO ThreadLocal.getContext
  Async.async $ do
    _ <- liftIO $ ThreadLocal.attachContext parentCtx
    doSearch
      -- Silently absorb cancellation — it's intentional (e.g. player undo).
      `catch` (\(_ :: Async.AsyncCancelled) -> pure ())
      -- Report any sync failure to the player without killing the session.
      -- Other async exceptions (ThreadKilled, etc.) are not caught and
      -- propagate out of the async, where Async's machinery stores them
      -- as the async's result.
      `catchSync` \(ex :: SomeException) -> do
        $(logTM) ErrorS $ ls @Text ("Engine search failed: " <> show ex)
        MVar.withMVar sessionVar $
          sendToPlayer
            (encode $ WsError EngineSearchFailed ("Engine search failed: " <> show ex) False)
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
    deliverEngineResult sessionVar gameId applied maybeOutcome

-- | Send all notifications derived from domain events to the player.
sendNotifications ::
  (Concurrent :> es, WebSocket :> es) =>
  PlayerColor -> AI.State -> [Common.DomainEvent] -> GameSession -> Eff es ()
sendNotifications humanColor newState events session =
  for_ (notificationsFor humanColor newState events) $ \msg ->
    sendToPlayer (encode msg) session

-- | Send a message to the player's WebSocket connection.
sendToPlayer ::
  (Concurrent :> es, WebSocket :> es) => LByteString -> GameSession -> Eff es ()
sendToPlayer msg session =
  let (_, connVar) = session.playerConn
   in safeSend connVar msg

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
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  , IOE :> es
  ) =>
  GameSessions ->
  Connection ->
  Eff es ()
handleWebSocket sessions conn =
  katipAddNamespace "ai" $
    authenticateWebSocket conn
      >>= traverse_ (handleAuthenticated sessions conn)

-- | Run the AI game session for an already-authenticated player. Takes the
-- resolved token, opens the session (incrementing the refcount), and runs
-- the receive loop until the WebSocket exits or errors. 'bracket' ensures
-- the refcount is released on any exit path, including failures during
-- connection setup.
handleAuthenticated ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Search :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  , IOE :> es
  ) =>
  GameSessions ->
  Connection ->
  GameParticipantToken ->
  Eff es ()
-- Game context wraps guardWebSocket so that any exception the outer guard
-- catches is still logged with gameId/player context. 'bracket' (not
-- 'finally') is required so that a failure in the acquire path (e.g.
-- safeSend throws after connectToGame has incremented the refcount) still
-- releases the refcount. 'disconnectPlayer' is a no-op when the uid
-- doesn't match the stored connection, so it is safe to call even when
-- connectToGame never completed.
handleAuthenticated sessions conn tok =
  withGameContext gameId humanColor $
    guardWebSocket conn $
      bracket connect disconnect loop
 where
  gameId = tok.gameId
  humanColor = tok.role
  connect = do
    uid <- generateId
    (sessionVar, connVar, gameState) <-
      connectToGame sessions gameId humanColor uid conn
    safeSend
      connVar
      (encode $ gameStateMessage gameId humanColor gameState)
    incGauge aiSessions
    $(logTM) InfoS "player connected"
    pure (sessionVar, uid, connVar)
  disconnect (sessionVar, uid, _) = do
    decGauge aiSessions
    $(logTM) InfoS "player disconnected"
    disconnectPlayer sessionVar uid
    STM.atomically $ release gameId sessions
  loop (sessionVar, _, connVar) =
    runMessageLoop connVar $ \clientMsg -> do
      time <- now
      processEvent
        sessionVar
        gameId
        (toEvent humanColor time clientMsg)
