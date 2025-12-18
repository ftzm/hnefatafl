{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
) where

import Control.Concurrent.MVar
import Chronos (now)
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
runStorageSQLite connectionVar = interpret $ \_ -> \case
  InsertHumanPlayer player -> run $ \conn -> do
    currentTime <- liftIO now
    createHumanPlayer (fromDomain player) currentTime conn
  GetHumanPlayer playerId ->
    run $ toDomain <<$>> getHumanPlayerById (fromDomain playerId)
  HumanPlayerFromName name ->
    run $ toDomain <<<$>>> getHumanPlayerByName name
  InsertEnginePlayer player -> run $ \conn -> do
    currentTime <- liftIO now
    createEnginePlayer (fromDomain player) currentTime conn
  GetEnginePlayer playerId ->
    run $ toDomain <<$>> getEnginePlayerById (fromDomain playerId)
  GetPlayer playerId -> run $ \conn ->
    getPlayerById conn (fromDomain playerId)
  DeletePlayer playerId ->
    run $ deletePlayerById (fromDomain playerId)
  InsertGame game ->
    run $ createGame (fromDomain game)
  GetGame gameId ->
    run $ toDomain <<$>> getGameById (fromDomain gameId)
  ListGames ->
    run $ toDomain <<<$>>> listGamesDb
  UpdateGameStatus gameId gameStatus endTime ->
    run $
      updateGameStatusById (fromDomain gameId) (fromDomain gameStatus) endTime
  DeleteGame gameId ->
    run $ deleteGameById (fromDomain gameId)
  InsertMove gameId gameMove -> run $ \conn ->
    MoveDb.insertMoveDb conn (fromDomain gameId) (fromDomain gameMove)
  InsertMoves gameId gameMoves -> run $ \conn ->
    MoveDb.insertMovesDb conn (fromDomain gameId) (map fromDomain gameMoves)
  GetMove gameId moveNumber ->
    run $ toDomain <<$>> MoveDb.getMoveByCompositeKey (fromDomain gameId) moveNumber
  GetMovesForGame gameId ->
    run $ toDomain <<<$>>> MoveDb.getMovesForGameDb (fromDomain gameId)
  GetLatestMoveForGame gameId ->
    run $ toDomain <<<$>>> MoveDb.getLatestMoveForGameDb (fromDomain gameId)
  GetMoveCountForGame gameId ->
    run $ MoveDb.getMoveCountForGameDb (fromDomain gameId)
  DeleteMove gameId moveNumber ->
    run $ MoveDb.deleteMove (fromDomain gameId) moveNumber
  CreateGameParticipantToken token -> run $ \conn -> do
    currentTime <- liftIO now
    createGameParticipantTokenDb (gameParticipantTokenToDb token currentTime) conn
  GetTokenByText tokenText ->
    run $ gameParticipantTokenFromDb <<<$>>> getGameParticipantTokenByText tokenText
  GetActiveTokenByGameAndRole gameId role ->
    run $
      gameParticipantTokenFromDb
        <<<$>>> getActiveTokenByGameAndRoleDb (fromDomain gameId) (fromDomain role)
 where
  run :: (Error String :> es, IOE :> es) => (Connection -> IO a) -> Eff es a
  run m =
    liftIO (withMVar connectionVar m)
      `catches` [ Handler $ \(e :: SQLError) -> throwError $ show @String e
                , Handler $ \(e :: IOException) -> throwError $ show @String e
                ]