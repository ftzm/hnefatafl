module Hnefatafl.Interpreter.Storage.SQLite.PendingAction (
  insertPendingActionDb,
  getPendingActionDb,
  deletePendingActionDb,
) where

import Chronos (Time)
import Database.SQLite.Simple
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

insertPendingActionDb ::
  GameIdDb -> PendingActionDb -> Time -> Connection -> IO ()
insertPendingActionDb gameId pa createdAt =
  execute'
    """
    INSERT INTO pending_game_action (game_id, action_type, offered_by, created_at)
    VALUES (?, ?, ?, ?)
    """
    (gameId, pa.actionType, pa.offeredBy, createdAt)

getPendingActionDb :: GameIdDb -> Connection -> IO (Maybe PendingActionDb)
getPendingActionDb =
  selectMaybe
    """
    SELECT action_type, offered_by
    FROM pending_game_action
    WHERE game_id = ?
    """
    . Only

deletePendingActionDb :: GameIdDb -> Connection -> IO ()
deletePendingActionDb =
  execute'
    "DELETE FROM pending_game_action WHERE game_id = ?"
    . Only
