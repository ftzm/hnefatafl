{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
) where

import Chronos (now)
import Control.Concurrent.MVar
import Control.Exception qualified as CE
import Data.Unique (hashUnique, newUnique)
import Database.SQLite.Simple (Connection, Query (..), SQLError, execute_)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Hnefatafl.Core.Data
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite.Game (
  createGame,
  deleteGameById,
  gameToDb,
  getGameById,
  listGamesDb,
  updateGameStatusById,
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
  (IOE :> es, Error String :> es) =>
  MVar Connection -> Eff (Storage : es) a -> Eff es a
runStorageSQLite connectionVar = interpret $ \_ -> \case
  RunTransaction txAction ->
    liftIO
      (withMVar connectionVar $ \conn -> withSavepoint conn txAction)
      `catches` [ Handler $ \(e :: SQLError) -> throwError $ show @String e
                , Handler $ \(e :: IOException) -> throwError $ show @String e
                ]

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
  UpdateGameStatus gameId gameStatus endTime ->
    updateGameStatusById (fromDomain gameId) (fromDomain gameStatus) endTime
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
  result <- dispatch cmd conn
  interpretTx conn (k result)
