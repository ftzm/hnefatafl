{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
) where

import Control.Concurrent.MVar
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Connection, SQLError)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Hnefatafl.Core.Data
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite.Game
import Hnefatafl.Interpreter.Storage.SQLite.Move
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
    now <- getCurrentTime
    createHumanPlayer (fromDomain player) now conn
  GetHumanPlayer playerId ->
    run $ toDomain <<$>> getHumanPlayerById (fromDomain playerId)
  InsertEnginePlayer player -> run $ \conn -> do
    now <- getCurrentTime
    createEnginePlayer (fromDomain player) now conn
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
  UpdateGameStatus gameId gameStatus endTime ->
    run $
      updateGameStatusById (fromDomain gameId) (fromDomain gameStatus) endTime
  DeleteGame gameId ->
    run $ deleteGameById (fromDomain gameId)
  InsertMove gameId gameMove -> run $ \conn ->
    insertMoveDb conn (fromDomain gameId) (fromDomain gameMove)
  GetMove moveId ->
    run $ toDomain <<$>> getMoveById (fromDomain moveId)
  GetMovesForGame gameId ->
    run $ toDomain <<<$>>> getMovesForGameDb (fromDomain gameId)
  GetLatestMoveForGame gameId ->
    run $ toDomain <<<$>>> getLatestMoveForGameDb (fromDomain gameId)
  GetMoveCountForGame gameId ->
    run $ getMoveCountForGameDb (fromDomain gameId)
  DeleteMove moveId ->
    run $ deleteMoveById (fromDomain moveId)
  CreateGameParticipantToken token -> run $ \conn -> do
    now <- getCurrentTime
    createGameParticipantTokenDb (gameParticipantTokenToDb token now) conn
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