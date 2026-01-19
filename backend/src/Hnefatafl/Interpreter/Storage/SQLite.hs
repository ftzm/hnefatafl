{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
) where

import Chronos (now)
import Control.Concurrent.MVar
import Database.SQLite.Simple (Connection, SQLError)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Hnefatafl.Core.Data
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite.Game
import Hnefatafl.Interpreter.Storage.SQLite.Move qualified as MoveDb
import Hnefatafl.Interpreter.Storage.SQLite.Player
import Hnefatafl.Interpreter.Storage.SQLite.Token
import Hnefatafl.Interpreter.Storage.SQLite.Type ()
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- SQLite effect implementation

runStorageSQLite ::
  (IOE :> es, Error String :> es) =>
  MVar Connection -> Eff (Storage : es) a -> Eff es a
runStorageSQLite connectionVar = interpret $ \_ cmd -> run $ case cmd of
  InsertHumanPlayer player -> \conn -> do
    currentTime <- liftIO now
    createHumanPlayer (fromDomain player) currentTime conn
  GetHumanPlayer playerId ->
    toDomain <<$>> getHumanPlayerById (fromDomain playerId)
  HumanPlayerFromName name ->
    toDomain <<<$>>> getHumanPlayerByName name
  InsertEnginePlayer player -> \conn -> do
    currentTime <- liftIO now
    createEnginePlayer (fromDomain player) currentTime conn
  GetEnginePlayer playerId ->
    toDomain <<$>> getEnginePlayerById (fromDomain playerId)
  GetPlayer playerId -> \conn ->
    getPlayerById conn (fromDomain playerId)
  DeletePlayer playerId ->
    deletePlayerById (fromDomain playerId)
  InsertGame game ->
    createGame (fromDomain game)
  GetGame gameId ->
    toDomain <<$>> getGameById (fromDomain gameId)
  ListGames ->
    toDomain <<<$>>> listGamesDb
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
    currentTime <- liftIO now
    createGameParticipantTokenDb (gameParticipantTokenToDb token currentTime) conn
  GetTokenByText tokenText ->
    gameParticipantTokenFromDb <<<$>>> getGameParticipantTokenByText tokenText
  GetActiveTokenByGameAndRole gameId role ->
    gameParticipantTokenFromDb
      <<<$>>> getActiveTokenByGameAndRoleDb (fromDomain gameId) (fromDomain role)
 where
  run :: (Error String :> es, IOE :> es) => (Connection -> IO a) -> Eff es a
  run m =
    liftIO (withMVar connectionVar m)
      `catches` [ Handler $ \(e :: SQLError) -> throwError $ show @String e
                , Handler $ \(e :: IOException) -> throwError $ show @String e
                ]
