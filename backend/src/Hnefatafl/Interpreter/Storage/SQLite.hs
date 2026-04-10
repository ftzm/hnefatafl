{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
) where

import Chronos (now)
import Control.Concurrent.MVar
import Control.Exception qualified as CE
import Data.Unique (hashUnique, newUnique)
import Database.SQLite.Simple (Connection, Query (..), execute_)
import Effectful
import Effectful.Dispatch.Dynamic
import Hnefatafl.Core.Data
import Hnefatafl.Effect.Storage
import Hnefatafl.Exception (DatabaseException (..), DomainException)
import Hnefatafl.Interpreter.Storage.SQLite.Game (
  createGame,
  deleteGameById,
  gameToDb,
  getGameById,
  listGamesDb,
  setOutcomeById,
 )
import Hnefatafl.Interpreter.Storage.SQLite.Move qualified as MoveDb
import Hnefatafl.Interpreter.Storage.SQLite.PendingAction qualified as PendingDb
import Hnefatafl.Interpreter.Storage.SQLite.Player
import Hnefatafl.Interpreter.Storage.SQLite.Token
import Hnefatafl.Interpreter.Storage.SQLite.Type ()
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- SQLite effect implementation

withSavepoint :: Connection -> StorageTx a -> IO a
withSavepoint conn txAction =
  do
    sp <- ("tx_" <>) . show . hashUnique <$> newUnique
    let
      savepoint = execute_ conn $ Query $ "SAVEPOINT " <> sp
      rollback = execute_ conn $ Query $ "ROLLBACK TO  " <> sp
      release = execute_ conn $ Query $ "RELEASE " <> sp
    savepoint
    result <- interpretTx conn txAction `CE.onException` rollback
    release
    pure result

runStorageSQLite ::
  (IOE :> es) =>
  MVar Connection -> Eff (Storage : es) a -> Eff es a
runStorageSQLite connectionVar = interpret $ \_ -> \case
  RunTransaction txAction ->
    liftIO (withMVar connectionVar $ \conn -> withSavepoint conn txAction)

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

interpretTx :: Connection -> StorageTx a -> IO a
interpretTx _ (PureTx a) = pure a
interpretTx conn (BindTx cmd k) = do
  result <-
    dispatch cmd conn
      `CE.catch` \(ex :: CE.SomeException) ->
        case fromException @DomainException ex of
          Just _ -> CE.throwIO ex -- already a domain exception, don't wrap
          Nothing ->
            let (op, ent, eid) = describeCmd cmd
             in CE.throwIO $ DatabaseException op ent eid ex
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
  GetActiveTokenByGameAndRole gid role -> ("GetActiveTokenByGameAndRole", "Token", Just $ show gid <> "/" <> show role)
  InsertPendingAction gid _ _ -> ("InsertPendingAction", "PendingAction", Just $ show gid)
  GetPendingAction gid -> ("GetPendingAction", "PendingAction", Just $ show gid)
  DeletePendingAction gid -> ("DeletePendingAction", "PendingAction", Just $ show gid)
  DeleteLastNMoves gid n -> ("DeleteLastNMoves", "Move", Just $ show gid <> " last " <> show n)
