module Hnefatafl.Interpreter.Storage.SQLite.Move (
  insertMoveDb,
  insertMovesDb,
  getMoveByCompositeKey,
  getMovesForGameDb,
  getLatestMoveForGameDb,
  getMoveCountForGameDb,
  deleteMove,
) where

import Database.SQLite.Simple
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- Move operations

insertMovesDb :: Connection -> GameIdDb -> [MoveDb] -> IO ()
insertMovesDb conn gameIdDb movesDb = do
  -- Get current max move number once
  maxMoveNum :: Int <-
    fromOnly
      <$> selectSingle
        "SELECT COALESCE(MAX(move_number), -1) FROM move WHERE game_id = ?"
        (Only gameIdDb)
        conn

  -- Number moves sequentially starting from maxMoveNum + 1
  let numberedMoves =
        zipWith
          (\i moveDb -> (gameIdDb, maxMoveNum + i) :. moveDb)
          [1 ..]
          movesDb

  executeMany
    conn
    """
    INSERT INTO move (game_id, move_number, player_color, from_position, to_position,
                     black_lower, black_upper, white_lower, white_upper, king,
                     captures_lower, captures_upper, timestamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    """
    numberedMoves

insertMoveDb :: Connection -> GameIdDb -> MoveDb -> IO ()
insertMoveDb conn gameIdDb moveDb =
  insertMovesDb conn gameIdDb [moveDb]

getMoveByCompositeKey :: GameIdDb -> Int -> Connection -> IO MoveDb
getMoveByCompositeKey gameIdDb moveNumber =
  selectSingle
    """
    SELECT player_color, from_position, to_position,
           black_lower, black_upper, white_lower, white_upper, king,
           captures_lower, captures_upper, timestamp
    FROM move
    WHERE game_id = ? AND move_number = ?
    """
    (gameIdDb, moveNumber)

getMovesForGameDb :: GameIdDb -> Connection -> IO [MoveDb]
getMovesForGameDb =
  query'
    """
    SELECT player_color, from_position, to_position,
           black_lower, black_upper, white_lower, white_upper, king,
           captures_lower, captures_upper, timestamp
    FROM move
    WHERE game_id = ?
    ORDER BY move_number ASC
    """
    . Only

getLatestMoveForGameDb :: GameIdDb -> Connection -> IO (Maybe MoveDb)
getLatestMoveForGameDb =
  selectMaybe
    """
    SELECT player_color, from_position, to_position,
           black_lower, black_upper, white_lower, white_upper, king,
           captures_lower, captures_upper, timestamp
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

deleteMove :: GameIdDb -> Int -> Connection -> IO ()
deleteMove gameIdDb moveNumber =
  execute'
    "DELETE FROM move WHERE game_id = ? AND move_number = ?"
    (gameIdDb, moveNumber)
