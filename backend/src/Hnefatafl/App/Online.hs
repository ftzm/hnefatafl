{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.App.Online (
  -- * Public API
  GameSessions,
  CreateGameResult (..),
  createGame,
  sweepExpiredTimeouts,
  handleWebSocket,

  -- * Internals (exported for testing)
  SessionEvent (..),
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
import Effectful.Exception (bracket, catchSync, throwIO)
import Effectful.Katip (KatipE, katipAddNamespace, logTM)
import Hnefatafl.Api.Types (Position (..))
import Hnefatafl.Api.Types.WS (
  WsError (..),
  WsErrorCode (..),
  transitionErrorToWsError,
 )
import Hnefatafl.Api.Types.WS.Online (
  OnlineClientMessage (..),
  OnlineServerMessage (..),
 )
import Hnefatafl.App.Online.Serialization (
  gameStateMessage,
  notificationsFor,
 )
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
import Hnefatafl.Core.Data (
  ClockState (..),
  Game (..),
  GameId (..),
  GameMode (..),
  GameParticipantToken (..),
  GameParticipantTokenId (..),
  PlayerColor (..),
  TimeControl,
  deduct,
  opponent,
  secondsToRemainingTime,
  toTimespan,
 )
import Hnefatafl.Core.Data qualified as Data
import Hnefatafl.Effect.Clock (Clock, delayUntil, now)
import Hnefatafl.Effect.IdGen (IdGen, generateId)
import Hnefatafl.Effect.Storage (
  Storage,
  StorageTx,
  createGameParticipantToken,
  getGame,
  getMovesForGame,
  getOnlineClockState,
  getOnlineTimeControl,
  getPendingAction,
  insertGame,
  listExpiredTimeouts,
  runTransaction,
  setOnlineClockState,
  setOnlineTimeControl,
  setTimeoutAt,
 )
import Hnefatafl.Effect.Trace (
  Trace,
  inSpan,
  inSpanWithLink,
  recordSpanException,
 )
import Hnefatafl.Effect.WebSocket (WebSocket)
import Hnefatafl.Exception (
  DomainException (..),
  IsDomainException (..),
  logCaughtException,
 )
import Hnefatafl.Game.Common (
  currentBoard,
 )
import Hnefatafl.Game.Common qualified as Common
import Hnefatafl.Game.Online (TransitionResult (..))
import Hnefatafl.Game.Online qualified as Online
import Hnefatafl.Metrics (
  HMetrics,
  Hs (..),
  decGauge,
  incGauge,
  increaseLabelledCounter,
  recordMetrics,
 )
import Katip (Severity (..), ls)
import Network.WebSockets (Connection)
import OpenTelemetry.Context (lookupSpan)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Trace.Core qualified as OT (getSpanContext)
import Optics ((^.))
import StmContainers.Map qualified as STMMap
import Torsor (add, difference)

-------------------------------------------------------------------------------
-- Types

newtype ConnectionId = ConnectionId Text
  deriving (Eq)

-- | Events processed by the session worker loop. All session state
-- mutations flow through this queue, ensuring sequential processing
-- without locks.
data SessionEvent
  = PlayerMessage PlayerColor Time OnlineClientMessage
  | PlayerConnected PlayerColor ConnectionId (MVar Connection)
  | PlayerDisconnected PlayerColor ConnectionId
  | TimeoutFired PlayerColor

-- | In-memory session for an active online game. A session exists
-- while at least one player is connected; the underlying game
-- persists in the database across sessions. Owned exclusively by the
-- worker loop, must never be shared.
data GameSession = GameSession
  { eventQueue :: STM.TBQueue SessionEvent
  , gameState :: Online.State
  , whiteConn :: Maybe (ConnectionId, MVar Connection)
  , blackConn :: Maybe (ConnectionId, MVar Connection)
  , timeoutAsync :: Maybe (Async.Async ())
  }
  deriving (Generic)

type GameSessions = STMMap.Map GameId (SessionEntry (STM.TBQueue SessionEvent))

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

eventColor :: SessionEvent -> PlayerColor
eventColor (PlayerMessage c _ _) = c
eventColor (PlayerConnected c _ _) = c
eventColor (PlayerDisconnected c _) = c
eventColor (TimeoutFired c) = c

isFinished :: Online.State -> Bool
isFinished (Online.State _ _ (Online.Finished _)) = True
isFinished _ = False

-------------------------------------------------------------------------------
-- Client message conversion

-- | Convert an API client message to an Online event, adding the
-- player's color and the current time.
toEvent :: PlayerColor -> Time -> OnlineClientMessage -> Online.Event
toEvent color time = \case
  OnlineMove (Position from) (Position to) ->
    Online.MakeMove color (Data.Move (fromIntegral from) (fromIntegral to)) time
  OnlineResign -> Online.Resign color
  OnlineOfferDraw -> Online.OfferDraw color
  OnlineAcceptDraw -> Online.AcceptDraw color
  OnlineDeclineDraw -> Online.DeclineDraw color
  OnlineRequestUndo -> Online.RequestUndo color
  OnlineAcceptUndo -> Online.AcceptUndo color time
  OnlineDeclineUndo -> Online.DeclineUndo color

-------------------------------------------------------------------------------
-- Storage

-- | Load online game state from the database
loadOnlineState :: GameId -> StorageTx Online.State
loadOnlineState gameId = do
  game <- getGame gameId
  gameMoves <- getMovesForGame gameId
  pendingAction <- getPendingAction gameId
  timeControl <- getOnlineTimeControl gameId
  clockState <- getOnlineClockState gameId
  let appliedMoves = gameMoveToAppliedMoves gameMoves
      board = currentBoard appliedMoves
      clock = liftA2 (,) timeControl clockState
  pure $
    Online.reconstruct
      board
      appliedMoves
      game.outcome
      pendingAction
      clock

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

-- | Create a new online game in the database with tokens for both
-- players. Does NOT create a session (lazy creation on WS connect).
createGame ::
  (Storage :> es, Clock :> es, IdGen :> es, Trace :> es, HMetrics :> es) =>
  Maybe TimeControl ->
  Eff es CreateGameResult
createGame timeControl = do
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
    for_ timeControl $ \tc -> do
      setOnlineTimeControl game.gameId tc
      let initial = secondsToRemainingTime tc.initialTime
      setOnlineClockState game.gameId $
        ClockState initial initial game.startTime
  increaseLabelledCounter gamesCreated "online"
  pure CreateGameResult{game, whiteToken, blackToken}

-------------------------------------------------------------------------------
-- Sweeper

-- | Resolve timed games that expired without an active session.
-- Runs periodically to catch games abandoned after server restart
-- or player disconnect. Uses the timeout_at column written by
-- handleTimerEvents.
sweepExpiredTimeouts ::
  (Storage :> es, Clock :> es, KatipE :> es) =>
  Eff es ()
sweepExpiredTimeouts = do
  currentTime <- now
  expired <- runTransaction $ listExpiredTimeouts currentTime
  for_ expired $ \gameId -> do
    gameState <- runTransaction $ loadOnlineState gameId
    case gameState.phase of
      Online.Active{turn} -> do
        case Online.transition gameState (Online.Timeout turn) of
          Right (TransitionResult _ events) -> do
            runTransaction $ do
              persistEvents gameId currentTime events
              setTimeoutAt gameId Nothing
          Left err ->
            $(logTM) WarningS $
              ls @Text $
                "sweeper: transition failed for "
                  <> show gameId
                  <> ": "
                  <> show err
      _ ->
        -- Game already finished, clear stale timeout_at
        runTransaction $ setTimeoutAt gameId Nothing

-- | Create a session and spawn its worker. Returns the event queue.
createSessionForGame ::
  ( Storage :> es
  , Concurrent :> es
  , IOE :> es
  , Clock :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameSessions ->
  GameId ->
  Online.State ->
  Eff es (STM.TBQueue SessionEvent)
createSessionForGame sessions gameId gameState = do
  -- Arbitrary limit: permissive enough to avoid backpressure
  -- under normal play, small enough to prevent abuse or
  -- pathological memory growth.
  -- Two threads can race into this function for the same game.
  -- insertOrAcquire is atomic: only one insert wins, the other
  -- acquires the winner's queue. Only the winner spawns a worker.
  (queue, inserted) <- STM.atomically $ do
    q <- STM.newTBQueue 20
    insertOrAcquire q gameId sessions
  when inserted $
    spawnWorker gameId gameState queue
  pure queue

-------------------------------------------------------------------------------
-- Session management

-- | Get or create a session's event queue. If a session already
-- exists in the map, acquires it (increments refcount). Otherwise
-- loads from DB, creates a queue, spawns a worker, and inserts.
getOrCreateSession ::
  ( Storage :> es
  , Concurrent :> es
  , IOE :> es
  , Clock :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameSessions ->
  GameId ->
  Eff es (STM.TBQueue SessionEvent)
getOrCreateSession sessions gameId = do
  existing <- STM.atomically $ tryAcquire gameId sessions
  case existing of
    Just queue -> pure queue
    Nothing -> do
      gameState <- runTransaction $ loadOnlineState gameId
      createSessionForGame sessions gameId gameState

-------------------------------------------------------------------------------
-- Session worker

-- | Spawn the session worker thread. The worker processes all
-- session events sequentially from the queue, so game state
-- transitions never race. The worker's root span links back to
-- the connection that spawned it for traceability.
spawnWorker ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , IOE :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameId ->
  Online.State ->
  STM.TBQueue SessionEvent ->
  Eff es ()
spawnWorker gameId initialState queue = do
  -- Capture the spawning span's context for the link. The worker
  -- creates its own root span rather than inheriting the parent,
  -- since it outlives the spawning connection and serves both players.
  mSpanCtx <- liftIO $ do
    ctx <- ThreadLocal.getContext
    traverse OT.getSpanContext (lookupSpan ctx)
  void $
    Async.async $
      katipAddNamespace "online" $
        katipAddNamespace "worker" $
          case mSpanCtx of
            Just spanCtx ->
              inSpanWithLink "online.session" spanCtx $
                workerLoop gameId (GameSession queue initialState Nothing Nothing Nothing)
            Nothing ->
              inSpan "online.session" $
                workerLoop gameId (GameSession queue initialState Nothing Nothing Nothing)

-- | Process events from the queue until the game is finished and
-- both players have disconnected.
workerLoop ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameId ->
  GameSession ->
  Eff es ()
workerLoop gameId session = do
  event <- STM.atomically $ STM.readTBQueue session.eventQueue
  session' <- handleSessionEvent gameId session event
  let noConnections =
        isNothing session'.whiteConn
          && isNothing session'.blackConn
      done =
        isFinished session'.gameState
          || (noConnections && isNothing session'.timeoutAsync)
  if done
    then for_ session'.timeoutAsync Async.cancel
    else workerLoop gameId session'

-- | Dispatch a session event. Non-fatal domain exceptions are
-- absorbed (logged + reported to the acting player) so the worker
-- stays alive. Fatal exceptions propagate and kill the worker.
handleSessionEvent ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameId ->
  GameSession ->
  SessionEvent ->
  Eff es GameSession
handleSessionEvent gameId session event =
  inSpan "session.event" (dispatchEvent gameId session event)
    `catchSync` \(ex :: SomeException) ->
      case fromException @DomainException ex of
        Just (DomainException e) | not (domainFatal e) -> do
          logCaughtException ex
          recordSpanException ex
          for_ (snd <$> getConn (eventColor event) session) $ \cv ->
            safeSend cv (encode $ WsError InternalError "internal error" False)
          pure session
        _ -> throwIO ex

-- | Route a session event to its handler. Connection events update
-- the session's connection state and notify the opponent; player
-- messages drive game state transitions via processGameEvent.
dispatchEvent ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameId ->
  GameSession ->
  SessionEvent ->
  Eff es GameSession
dispatchEvent gameId session = \case
  PlayerConnected color connId connVar -> do
    let session' = setConn color (Just (connId, connVar)) session
    safeSend connVar (encode $ gameStateMessage gameId color session'.gameState)
    sendToPlayer (opponent color) (encode OnlineOpponentJoined) session'
    -- If reconnecting to a timed game with no active timer,
    -- check if time has expired and recover the timer.
    if isNothing session'.timeoutAsync
      then recoverTimer gameId session'
      else pure session'
  PlayerDisconnected color connId -> do
    let session' = clearConn color connId session
    sendToPlayer (opponent color) (encode OnlineOpponentLeft) session'
    -- Cancel the timer when both players have disconnected. The
    -- sweeper resolves abandoned games; recoverTimer restarts the
    -- timer on reconnect.
    let noConns =
          isNothing session'.whiteConn && isNothing session'.blackConn
    if noConns
      then cancelTimer session'
      else pure session'
  PlayerMessage color time clientMsg ->
    processGameEvent session gameId color $
      toEvent color time clientMsg
  TimeoutFired color ->
    -- A timer can enqueue a TimeoutFired in the window between a move
    -- switching the turn and resetTimer cancelling that timer;
    -- Async.cancel cannot un-enqueue an already-written event. Drop a
    -- timeout for a player who is no longer to move (or a game that has
    -- ended) rather than routing it through the transition, which would
    -- reject it and surface a spurious error to that client.
    case session.gameState.phase of
      Online.Active{turn}
        | turn == color ->
            processGameEvent session gameId color (Online.Timeout color)
      _ -> pure session

-------------------------------------------------------------------------------
-- Game event processing

-- | Process a domain game event. Transitions state, persists to DB,
-- sends notifications, records metrics, and manages the timeout
-- timer. Not thread-safe.
processGameEvent ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameSession ->
  GameId ->
  PlayerColor ->
  Online.Event ->
  Eff es GameSession
processGameEvent session gameId color event =
  case Online.transition session.gameState event of
    Left err -> do
      when (err == Common.InvalidMove) $
        increaseLabelledCounter invalidMovesTotal "online"
      sendToPlayer
        color
        (encode $ transitionErrorToWsError err)
        session
      pure session
    Right (TransitionResult newState events) -> do
      currentTime <- now
      runTransaction $ persistEvents gameId currentTime events
      sendNotifications color newState events session
      recordMetrics "online" events

      handleTimerEvents gameId events session{gameState = newState}

-------------------------------------------------------------------------------
-- Timeout timer management

-- | Respond to domain events that affect the timer. Persists the
-- timeout deadline so the sweeper can resolve abandoned games.
handleTimerEvents ::
  (Storage :> es, Concurrent :> es, Clock :> es) =>
  GameId ->
  [Common.DomainEvent] ->
  GameSession ->
  Eff es GameSession
handleTimerEvents gameId events session
  | any isGameEnded events = do
      runTransaction $ setTimeoutAt gameId Nothing
      cancelTimer session
  | any isClockUpdated events = do
      currentTime <- now
      let deadline = computeDeadline currentTime session.gameState.phase
      runTransaction $ setTimeoutAt gameId (snd <$> deadline)
      resetTimer deadline session
  | otherwise = pure session
 where
  isGameEnded (Common.GameEnded _) = True
  isGameEnded _ = False
  isClockUpdated (Common.ClockUpdated _) = True
  isClockUpdated _ = False

-- | Compute the active player and the absolute deadline at which
-- their clock expires. Nothing when the game is finished or untimed.
computeDeadline :: Time -> Online.Phase -> Maybe (PlayerColor, Time)
computeDeadline currentTime = \case
  Online.Active{turn, clock = Just (_, cs)} ->
    let remaining = cs ^. Online.remainingFor turn
     in Just (turn, add (toTimespan remaining) currentTime)
  _ -> Nothing

cancelTimer :: Concurrent :> es => GameSession -> Eff es GameSession
cancelTimer session = do
  for_ session.timeoutAsync Async.cancel
  pure session{timeoutAsync = Nothing}

resetTimer ::
  (Concurrent :> es, Clock :> es) =>
  Maybe (PlayerColor, Time) ->
  GameSession ->
  Eff es GameSession
resetTimer deadline session = do
  for_ session.timeoutAsync Async.cancel
  timer <- traverse (spawnTimeoutTimer session.eventQueue) deadline
  pure session{timeoutAsync = timer}

-- | On reconnect, check if the active player's clock has expired
-- since turnStartedAt. If expired, apply the timeout transition
-- directly. If time remains, spawn a timer for the adjusted
-- duration and persist the deadline for the sweeper.
recoverTimer ::
  ( Storage :> es
  , Clock :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  ) =>
  GameId ->
  GameSession ->
  Eff es GameSession
recoverTimer gameId session =
  case session.gameState.phase of
    Online.Active{turn, clock = Just (_, cs)} -> do
      currentTime <- now
      let elapsed = difference currentTime cs.turnStartedAt
      case deduct elapsed (cs ^. Online.remainingFor turn) of
        Nothing ->
          processGameEvent session gameId turn (Online.Timeout turn)
        Just adjusted -> do
          let deadline = add (toTimespan adjusted) currentTime
          runTransaction $ setTimeoutAt gameId (Just deadline)
          timer <- spawnTimeoutTimer session.eventQueue (turn, deadline)
          pure session{timeoutAsync = Just timer}
    _ -> pure session

-- | Spawn an async that sleeps until the given absolute deadline,
-- then enqueues a TimeoutFired event. The deadline is captured by
-- the caller so it cannot drift with the spawned thread's start.
spawnTimeoutTimer ::
  (Concurrent :> es, Clock :> es) =>
  STM.TBQueue SessionEvent ->
  (PlayerColor, Time) ->
  Eff es (Async.Async ())
spawnTimeoutTimer queue (color, deadline) =
  Async.async $ do
    delayUntil deadline
    STM.atomically $ STM.writeTBQueue queue $ TimeoutFired color

-------------------------------------------------------------------------------
-- Notifications

-- | Send all notifications derived from domain events to the
-- appropriate players.
sendNotifications ::
  (Concurrent :> es, WebSocket :> es) =>
  PlayerColor ->
  Online.State ->
  [Common.DomainEvent] ->
  GameSession ->
  Eff es ()
sendNotifications actor newState events session =
  for_ (notificationsFor actor newState events) $ \(target, msg) ->
    sendToPlayer target (encode msg) session

-- | Send a message to a player's connection, if connected.
sendToPlayer ::
  (Concurrent :> es, WebSocket :> es) =>
  PlayerColor -> LByteString -> GameSession -> Eff es ()
sendToPlayer color msg session =
  for_ (getConn color session) $ \(_, connVar) ->
    safeSend connVar msg

-------------------------------------------------------------------------------
-- WebSocket handler

handleWebSocket ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
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
  katipAddNamespace "online" $
    authenticateWebSocket conn
      >>= traverse_ (handleAuthenticated sessions conn)

-- | Run the online game handler for an authenticated player. Gets
-- the session's event queue and enqueues connect/disconnect/message
-- events. The session worker processes them sequentially.
--
-- Uses 'bracket' (not 'finally') so that a failure during connect
-- (after getOrCreateSession has incremented the refcount) still
-- releases the refcount in disconnect. The PlayerDisconnected
-- enqueue is safe even if PlayerConnected never reached the
-- worker — clearConn is a no-op when the uid doesn't match.
handleAuthenticated ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
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
handleAuthenticated sessions conn tok =
  withGameContext gameId color $
    guardWebSocket conn $
      bracket connect disconnect loop
 where
  gameId = tok.gameId
  color = tok.role
  connect = do
    queue <- getOrCreateSession sessions gameId
    uid <- generateId
    connVar <- MVar.newMVar conn
    STM.atomically $
      STM.writeTBQueue queue (PlayerConnected color uid connVar)
    incGauge onlineSessions
    $(logTM) InfoS "player connected"
    pure (queue, uid, connVar)
  disconnect (queue, uid, _) = do
    decGauge onlineSessions
    $(logTM) InfoS "player disconnected"
    STM.atomically $
      STM.writeTBQueue queue (PlayerDisconnected color uid)
    STM.atomically $ release gameId sessions
  loop (queue, _, connVar) =
    runMessageLoop connVar $ \clientMsg -> do
      time <- now
      STM.atomically $
        STM.writeTBQueue queue (PlayerMessage color time clientMsg)
