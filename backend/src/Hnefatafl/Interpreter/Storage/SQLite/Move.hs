module Hnefatafl.Interpreter.Storage.SQLite.Move (
  insertMoveDb,
  getMoveById,
  getMovesForGameDb,
  getLatestMoveForGameDb,
  getMoveCountForGameDb,
  deleteMoveById,
) where

import Database.SQLite.Simple
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- Move operations

insertMoveDb :: Connection -> GameIdDb -> MoveDb -> IO ()
insertMoveDb conn gameIdDb moveDb = do
  execute
    conn
    """
    INSERT INTO move (game_id, id, move_number, player_color, from_position, to_position,
                     black_lower, black_upper, white_lower, white_upper, king, timestamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    """
    (Only gameIdDb :. moveDb)

getMoveById :: MoveIdDb -> Connection -> IO MoveDb
getMoveById =
  selectSingle
    """
    SELECT id, move_number, player_color, from_position, to_position,
           black_lower, black_upper, white_lower, white_upper, king, timestamp
    FROM move
    WHERE id = ?
    """
    . Only

getMovesForGameDb :: GameIdDb -> Connection -> IO [MoveDb]
getMovesForGameDb =
  query'
    """
    SELECT id, move_number, player_color, from_position, to_position,
           black_lower, black_upper, white_lower, white_upper, king, timestamp
    FROM move
    WHERE game_id = ?
    ORDER BY move_number ASC
    """
    . Only

getLatestMoveForGameDb :: GameIdDb -> Connection -> IO (Maybe MoveDb)
getLatestMoveForGameDb =
  selectMaybe
    """
    SELECT id, move_number, player_color, from_position, to_position,
           black_lower, black_upper, white_lower, white_upper, king, timestamp
    FROM move
    WHERE game_id = ?
    ORDER BY move_number DESC
    LIMIT 1
    """
    . Only

getMoveCountForGameDb :: GameIdDb -> Connection -> IO Int
getMoveCountForGameDb =
  fromOnly
    <<<$>>> selectSingle "SELECT COUNT(*) FROM move WHERE game_id = ?"
    . Only

deleteMoveById :: MoveIdDb -> Connection -> IO ()
deleteMoveById = execute' "DELETE FROM move WHERE id = ?" . Only