{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.App.Online (
  -- * Public API
  GameSessions,
  CreateGameResult (..),
  createGame,
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
  RemainingTime,
  TimeControl,
  opponent,
  remainingToMicroseconds,
  secondsToRemainingTime,
 )
import Hnefatafl.Core.Data qualified as Data
import Hnefatafl.Effect.Clock (Clock, delay, now)
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
  runTransaction,
  setOnlineClockState,
  setOnlineTimeControl,
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
import Katip (Severity (..))
import Network.WebSockets (Connection)
import OpenTelemetry.Context (lookupSpan)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Trace.Core qualified as OT (getSpanContext)
import Optics ((^.))
import StmContainers.Map qualified as STMMap

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
      -- Arbitrary limit: permissive enough to avoid backpressure
      -- under normal play, small enough to prevent abuse or
      -- pathological memory growth.
      -- Two threads can race past the tryAcquire above if neither
      -- finds an existing entry. Both load state and create a queue,
      -- but insertOrAcquire is atomic: only one insert wins, the
      -- other acquires the winner's queue. The Bool distinguishes
      -- the winner (who must spawn the worker) from the loser
      -- (whose queue is discarded by insertOrAcquire).
      (queue, inserted) <- STM.atomically $ do
        q <- STM.newTBQueue 20
        insertOrAcquire q gameId sessions
      when inserted $
        spawnWorker gameId gameState queue
      pure queue

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
      done = isFinished session'.gameState || noConnections
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
    pure session'
  PlayerDisconnected color connId -> do
    let session' = clearConn color connId session
    sendToPlayer (opponent color) (encode OnlineOpponentLeft) session'
    pure session'
  PlayerMessage color time clientMsg ->
    processGameEvent session gameId color $
      toEvent color time clientMsg
  TimeoutFired color ->
    processGameEvent session gameId color $
      Online.Timeout color

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
      handleTimerEvents events session{gameState = newState}

-------------------------------------------------------------------------------
-- Timeout timer management

-- | Respond to domain events that affect the timer.
handleTimerEvents ::
  (Concurrent :> es, Clock :> es) =>
  [Common.DomainEvent] ->
  GameSession ->
  Eff es GameSession
handleTimerEvents events session
  | any isGameEnded events = cancelTimer session
  | any isClockUpdated events = resetTimer session
  | otherwise = pure session
 where
  isGameEnded (Common.GameEnded _) = True
  isGameEnded _ = False
  isClockUpdated (Common.ClockUpdated _) = True
  isClockUpdated _ = False

cancelTimer :: Concurrent :> es => GameSession -> Eff es GameSession
cancelTimer session = do
  for_ session.timeoutAsync Async.cancel
  pure session{timeoutAsync = Nothing}

resetTimer ::
  (Concurrent :> es, Clock :> es) =>
  GameSession ->
  Eff es GameSession
resetTimer session = do
  for_ session.timeoutAsync Async.cancel
  timer <-
    traverse
      (spawnTimeoutTimer session.eventQueue)
      (activeClockRemaining session.gameState.phase)
  pure session{timeoutAsync = timer}

-- | Extract the active player's remaining time from the game phase.
-- Returns Nothing if the game is finished or has no clock.
activeClockRemaining :: Online.Phase -> Maybe (PlayerColor, RemainingTime)
activeClockRemaining = \case
  Online.Active{turn, clock = Just (_, cs)} ->
    Just (turn, cs ^. Online.remainingFor turn)
  _ -> Nothing

-- | Spawn an async that sleeps for the given remaining time, then
-- enqueues a TimeoutFired event.
spawnTimeoutTimer ::
  (Concurrent :> es, Clock :> es) =>
  STM.TBQueue SessionEvent ->
  (PlayerColor, RemainingTime) ->
  Eff es (Async.Async ())
spawnTimeoutTimer queue (color, remaining) =
  Async.async $ do
    delay $ remainingToMicroseconds remaining
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
