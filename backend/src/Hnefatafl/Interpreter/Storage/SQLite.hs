{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
  withConnectionRecovery,
) where

import Chronos (getTimespan, now)
import Data.Unique (hashUnique, newUnique)
import Database.SQLite.Simple (Connection, Query (..), close, execute_)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Dispatch.Dynamic
import Effectful.Exception (catchSync, throwIO, try)
import Hnefatafl.Core.Data
import Hnefatafl.Effect.Clock (Clock, stopwatch)
import Hnefatafl.Effect.Storage
import Hnefatafl.Effect.Trace (Trace, addSpanAttribute, inSpan)
import Hnefatafl.Exception (
  ConnectionUnrecoverableException (..),
  DatabaseException (..),
  DomainException,
 )
import Hnefatafl.Interpreter.Storage.SQLite.Game (
  createGame,
  deleteGameById,
  gameToDb,
  getGameById,
  listGamesDb,
  setOutcomeById,
 )
import Hnefatafl.Interpreter.Storage.SQLite.Game qualified as GameDb
import Hnefatafl.Interpreter.Storage.SQLite.Move qualified as MoveDb
import Hnefatafl.Interpreter.Storage.SQLite.PendingAction qualified as PendingDb
import Hnefatafl.Interpreter.Storage.SQLite.Player
import Hnefatafl.Interpreter.Storage.SQLite.Token
import Hnefatafl.Interpreter.Storage.SQLite.Type ()
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Hnefatafl.Metrics (HMetrics, Hs (..), observe)

--------------------------------------------------------------------------------
-- SQLite effect implementation

-- | Run a 'StorageTx' inside a SAVEPOINT and return its result.
--
-- On any exception from the action or from the closing RELEASE, the SP
-- is removed by @ROLLBACK TO sp; RELEASE sp@ and the original exception
-- propagates. If that local cleanup itself fails, a plain @ROLLBACK@
-- ends the entire transaction stack — including any outer transaction
-- the caller may have open — and the original exception still
-- propagates. If even that escalation @ROLLBACK@ fails, throws
-- 'ConnectionUnrecoverableException' wrapping the original cause: the
-- connection's transaction state is then unknown and the caller must
-- replace the connection before reusing it.
withSavepoint ::
  (IOE :> es, Trace :> es) =>
  Connection -> StorageTx a -> Eff es a
withSavepoint conn txAction = do
  sp <- liftIO $ ("tx_" <>) . show . hashUnique <$> newUnique
  let wrap op io =
        io `catchSync` \(ex :: SomeException) ->
          case fromException @DomainException ex of
            Just _ -> throwIO ex
            Nothing -> throwIO $ DatabaseException op "Transaction" Nothing ex
      savepoint =
        wrap "Savepoint" . liftIO . execute_ conn $ Query $ "SAVEPOINT " <> sp
      rollback =
        wrap "Rollback" . liftIO . execute_ conn $ Query $ "ROLLBACK TO " <> sp
      release =
        wrap "Release" . liftIO . execute_ conn $ Query $ "RELEASE " <> sp
      -- Plain ROLLBACK ends every transaction on this connection,
      -- including any outer transaction the caller may have opened.
      -- The recovery of last resort when the SP can't be popped
      -- locally via ROLLBACK TO + RELEASE.
      escalate =
        wrap "Escalate" . liftIO . execute_ conn $ Query "ROLLBACK"
  savepoint
  -- The action and the closing RELEASE share one recovery handler: a
  -- failed RELEASE leaves the SP on the stack with uncommitted writes,
  -- the same shape as a mid-action failure, and needs the same cleanup.
  ( do
      v <- interpretTx conn txAction
      release
      pure v
    )
    `catchSync` \(originalEx :: SomeException) -> do
      -- ROLLBACK TO undoes the SP's writes; RELEASE pops it from the
      -- stack. Both must succeed for the SP to be gone.
      cleanupOk <-
        (rollback >> release >> pure True)
          `catchSync` \(_ :: SomeException) -> pure False
      if cleanupOk
        then throwIO originalEx
        else do
          escalateOk <-
            (escalate >> pure True)
              `catchSync` \(_ :: SomeException) -> pure False
          if escalateOk
            then throwIO originalEx
            else
              throwIO . ConnectionUnrecoverableException $
                toException originalEx

-- | Run a connection-using action under an 'MVar' connection. If the
-- action throws 'ConnectionUnrecoverableException', the current
-- connection is closed (best-effort) and replaced by one obtained from
-- @openConn@ before the exception is rethrown — the next caller then
-- observes a fresh connection in autocommit mode. Any other exception
-- propagates without touching the connection.
withConnectionRecovery ::
  (Concurrent :> es, IOE :> es) =>
  MVar Connection ->
  IO Connection ->
  (Connection -> Eff es a) ->
  Eff es a
withConnectionRecovery connVar openConn action = do
  outcome <- MVar.modifyMVar connVar $ \conn -> do
    res <- try (action conn)
    case res of
      Right v -> pure (conn, Right v)
      Left (e :: SomeException) ->
        case fromException @ConnectionUnrecoverableException e of
          Nothing -> pure (conn, Left e)
          Just _ -> do
            _ <- try @SomeException (liftIO (close conn))
            newConn <- liftIO openConn
            pure (newConn, Left e)
  case outcome of
    Right v -> pure v
    Left e -> throwIO e

runStorageSQLite ::
  (IOE :> es, Concurrent :> es, Trace :> es, HMetrics :> es, Clock :> es) =>
  MVar Connection -> IO Connection -> Eff (Storage : es) a -> Eff es a
runStorageSQLite connectionVar openConn = interpret $ \_ -> \case
  RunTransaction txAction ->
    inSpan "db.transaction" $ do
      (elapsed, result) <-
        stopwatch $
          withConnectionRecovery connectionVar openConn $
            \conn -> withSavepoint conn txAction
      let durationSec = fromIntegral (getTimespan elapsed) / 1_000_000_000
      observe dbTransaction durationSec
      pure result

dispatch :: StorageCmd a -> Connection -> IO a
dispatch = \case
  InsertHumanPlayer player -> \conn -> do
    currentTime <- now
    createHumanPlayer (fromDomain player) currentTime conn
  GetHumanPlayer playerId ->
    toDomain <<$>> getHumanPlayerById (fromDomain playerId)
  HumanPlayerFromName name ->
    toDomain <<<$>>> getHumanPlayerByName name
  InsertEnginePlayer player -> \conn -> do
    currentTime <- now
    createEnginePlayer (fromDomain player) currentTime conn
  GetEnginePlayer playerId ->
    toDomain <<$>> getEnginePlayerById (fromDomain playerId)
  GetPlayer playerId -> \conn ->
    getPlayerById conn (fromDomain playerId)
  DeletePlayer playerId ->
    deletePlayerById (fromDomain playerId)
  InsertGame game -> \conn ->
    createGame (gameToDb game) game.mode conn
  GetGame gameId ->
    getGameById (fromDomain gameId)
  ListGames ->
    listGamesDb
  SetOutcome gameId outcome endTime ->
    setOutcomeById (fromDomain gameId) (fromDomain (Just outcome)) endTime
  DeleteGame gameId ->
    deleteGameById (fromDomain gameId)
  InsertMove gameId gameMove -> \conn ->
    MoveDb.insertMoveDb conn (fromDomain gameId) (fromDomain gameMove)
  InsertMoves gameId gameMoves -> \conn ->
    MoveDb.insertMovesDb conn (fromDomain gameId) (map fromDomain gameMoves)
  GetMove gameId moveNumber ->
    toDomain <<$>> MoveDb.getMoveByCompositeKey (fromDomain gameId) moveNumber
  GetMovesForGame gameId ->
    toDomain <<<$>>> MoveDb.getMovesForGameDb (fromDomain gameId)
  GetLatestMoveForGame gameId ->
    toDomain <<<$>>> MoveDb.getLatestMoveForGameDb (fromDomain gameId)
  GetMoveCountForGame gameId ->
    MoveDb.getMoveCountForGameDb (fromDomain gameId)
  DeleteMove gameId moveNumber ->
    MoveDb.deleteMove (fromDomain gameId) moveNumber
  CreateGameParticipantToken token -> \conn -> do
    currentTime <- now
    createGameParticipantTokenDb (gameParticipantTokenToDb token currentTime) conn
  GetTokenByText tokenText ->
    gameParticipantTokenFromDb <<<$>>> getGameParticipantTokenByText tokenText
  GetActiveTokenByGameAndRole gameId role ->
    gameParticipantTokenFromDb
      <<<$>>> getActiveTokenByGameAndRoleDb (fromDomain gameId) (fromDomain role)
  InsertPendingAction gameId pa createdAt ->
    PendingDb.insertPendingActionDb (fromDomain gameId) (fromDomain pa) createdAt
  GetPendingAction gameId ->
    toDomain <<<$>>> PendingDb.getPendingActionDb (fromDomain gameId)
  DeletePendingAction gameId ->
    PendingDb.deletePendingActionDb (fromDomain gameId)
  DeleteLastNMoves gameId n ->
    MoveDb.deleteLastNMoves (fromDomain gameId) n
  SetOnlineTimeControl gameId tc ->
    GameDb.setOnlineTimeControl (fromDomain gameId) tc
  GetOnlineTimeControl gameId ->
    GameDb.getOnlineTimeControl (fromDomain gameId)
  SetOnlineClockState gameId cs ->
    GameDb.setOnlineClockState (fromDomain gameId) cs
  GetOnlineClockState gameId ->
    GameDb.getOnlineClockState (fromDomain gameId)
  ListActiveTimedOnlineGames ->
    GameDb.listActiveTimedOnlineGames
  SetTimeoutAt gameId timeout ->
    GameDb.setTimeoutAt (fromDomain gameId) timeout
  ListExpiredTimeouts currentTime ->
    GameDb.listExpiredTimeouts currentTime

interpretTx ::
  (IOE :> es, Trace :> es) =>
  Connection -> StorageTx a -> Eff es a
interpretTx _ (PureTx a) = pure a
interpretTx conn (BindTx cmd k) = do
  let (op, ent, mId) = describeCmd cmd
  result <-
    inSpan op $ do
      addSpanAttribute "db.entity" ent
      for_ mId $ addSpanAttribute "db.id"
      liftIO (dispatch cmd conn)
        `catchSync` \(ex :: SomeException) ->
          case fromException @DomainException ex of
            Just _ -> throwIO ex -- already a domain exception, don't wrap
            Nothing -> throwIO $ DatabaseException op ent mId ex
  interpretTx conn (k result)

-- | Describe a storage command as (operation, entity, entityId).
describeCmd :: StorageCmd a -> (Text, Text, Maybe Text)
describeCmd = \case
  InsertHumanPlayer p -> ("InsertHumanPlayer", "HumanPlayer", Just $ show p.playerId)
  GetHumanPlayer pid -> ("GetHumanPlayer", "HumanPlayer", Just $ show pid)
  HumanPlayerFromName name -> ("HumanPlayerFromName", "HumanPlayer", Just name)
  InsertEnginePlayer p -> ("InsertEnginePlayer", "EnginePlayer", Just $ show p.playerId)
  GetEnginePlayer pid -> ("GetEnginePlayer", "EnginePlayer", Just $ show pid)
  GetPlayer pid -> ("GetPlayer", "Player", Just $ show pid)
  DeletePlayer pid -> ("DeletePlayer", "Player", Just $ show pid)
  InsertGame g -> ("InsertGame", "Game", Just $ show g.gameId)
  GetGame gid -> ("GetGame", "Game", Just $ show gid)
  ListGames -> ("ListGames", "Game", Nothing)
  SetOutcome gid _ _ -> ("SetOutcome", "Game", Just $ show gid)
  DeleteGame gid -> ("DeleteGame", "Game", Just $ show gid)
  InsertMove gid _ -> ("InsertMove", "Move", Just $ show gid)
  InsertMoves gid _ -> ("InsertMoves", "Move", Just $ show gid)
  GetMove gid n -> ("GetMove", "Move", Just $ show gid <> "#" <> show n)
  GetMovesForGame gid -> ("GetMovesForGame", "Move", Just $ show gid)
  GetLatestMoveForGame gid -> ("GetLatestMoveForGame", "Move", Just $ show gid)
  GetMoveCountForGame gid -> ("GetMoveCountForGame", "Move", Just $ show gid)
  DeleteMove gid n -> ("DeleteMove", "Move", Just $ show gid <> "#" <> show n)
  CreateGameParticipantToken t -> ("CreateGameParticipantToken", "Token", Just $ show t.gameId)
  GetTokenByText tt -> ("GetTokenByText", "Token", Just tt)
  GetActiveTokenByGameAndRole gid role ->
    ("GetActiveTokenByGameAndRole", "Token", Just $ show gid <> "/" <> show role)
  InsertPendingAction gid _ _ -> ("InsertPendingAction", "PendingAction", Just $ show gid)
  GetPendingAction gid -> ("GetPendingAction", "PendingAction", Just $ show gid)
  DeletePendingAction gid -> ("DeletePendingAction", "PendingAction", Just $ show gid)
  DeleteLastNMoves gid n -> ("DeleteLastNMoves", "Move", Just $ show gid <> " last " <> show n)
  SetOnlineTimeControl gid _ -> ("SetOnlineTimeControl", "TimeControl", Just $ show gid)
  GetOnlineTimeControl gid -> ("GetOnlineTimeControl", "TimeControl", Just $ show gid)
  SetOnlineClockState gid _ -> ("SetOnlineClockState", "ClockState", Just $ show gid)
  GetOnlineClockState gid -> ("GetOnlineClockState", "ClockState", Just $ show gid)
  ListActiveTimedOnlineGames -> ("ListActiveTimedOnlineGames", "Game", Nothing)
  SetTimeoutAt gid _ -> ("SetTimeoutAt", "TimeoutAt", Just $ show gid)
  ListExpiredTimeouts _ -> ("ListExpiredTimeouts", "Game", Nothing)
