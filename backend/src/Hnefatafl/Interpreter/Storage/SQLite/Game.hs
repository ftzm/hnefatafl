module Hnefatafl.Interpreter.Storage.SQLite.Game (
  createGame,
  getGameById,
  updateGameStatusById,
  deleteGameById,
) where

import Data.Time (UTCTime)
import Database.SQLite.Simple
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- Game operations

createGame :: GameDb -> Connection -> IO ()
createGame =
  execute'
    """
    INSERT INTO game (id, name, white_player_id, black_player_id, start_time, end_time, game_status, created_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    """

getGameById :: GameIdDb -> Connection -> IO GameDb
getGameById =
  selectSingle
    """
    SELECT id, name, white_player_id, black_player_id, start_time, end_time, game_status, created_at
    FROM game
    WHERE id = ?
    """
    . Only

updateGameStatusById ::
  GameIdDb -> GameStatusDb -> Maybe UTCTime -> Connection -> IO ()
updateGameStatusById gameId gameStatus endTime =
  execute'
    "UPDATE game SET game_status = ?, end_time = ? WHERE id = ?"
    (gameStatus, endTime, gameId)

deleteGameById :: GameIdDb -> Connection -> IO ()
deleteGameById =
  execute'
    "DELETE FROM game WHERE id = ?"
    . Only