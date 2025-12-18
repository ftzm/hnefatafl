module Hnefatafl.Interpreter.Storage.SQLite.Player (
  createBasePlayer,
  createHumanPlayer,
  createEnginePlayer,
  getPlayerById,
  getHumanPlayerById,
  getEnginePlayerById,
  getHumanPlayerByName,
  deletePlayerById,
) where

import Data.Maybe (fromJust)
import Chronos (Time)
import Database.SQLite.Simple
import Hnefatafl.Core.Data
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- Player operations

createBasePlayer :: PlayerIdDb -> Text -> Time -> Connection -> IO ()
createBasePlayer playerId playerType createdAt =
  execute'
    "INSERT INTO player (id, player_type, created_at) VALUES (?, ?, ?)"
    (playerId, playerType, createdAt)

createHumanPlayer ::
  HumanPlayerDb -> Time -> Connection -> IO ()
createHumanPlayer player createdAt conn = do
  createBasePlayer player.playerId "human" createdAt conn
  execute'
    "INSERT INTO human_player (player_id, name, email) VALUES (?, ?, ?)"
    player
    conn

createEnginePlayer ::
  EnginePlayerDb -> Time -> Connection -> IO ()
createEnginePlayer player createdAt conn = do
  createBasePlayer player.playerId "engine" createdAt conn
  execute'
    "INSERT INTO engine_player (player_id, version) VALUES (?, ?)"
    player
    conn

getPlayerById :: Connection -> PlayerIdDb -> IO Player
getPlayerById conn playerId =
  fromJust . fromSql <$> q
 where
  fromSql ::
    (PlayerIdDb, PlayerType, Maybe Text, Maybe Text, Maybe Text) -> Maybe Player
  fromSql (pId, playerType, name, email, version) = case playerType of
    HumanType -> fmap HumanPlayerTag (flip (HumanPlayer $ toDomain pId) email <$> name)
    EngineType -> fmap EnginePlayerTag (EnginePlayer (toDomain pId) <$> version)
  q :: IO (PlayerIdDb, PlayerType, Maybe Text, Maybe Text, Maybe Text)
  q =
    selectSingle
      """
      SELECT p.id, p.player_type, h.name, h.email, e.version
      FROM player p
      LEFT JOIN human_player h ON p.id = h.player_id
      LEFT JOIN engine_player e ON p.id = e.player_id WHERE p.id = ?
      """
      (Only playerId)
      conn

getHumanPlayerById :: PlayerIdDb -> Connection -> IO HumanPlayerDb
getHumanPlayerById =
  selectSingle
    """
    SELECT h.player_id, h.name, h.email
    FROM human_player h
    WHERE h.player_id = ?
    """
    . Only

getEnginePlayerById :: PlayerIdDb -> Connection -> IO EnginePlayerDb
getEnginePlayerById =
  selectSingle
    """
    SELECT e.player_id, e.version
    FROM engine_player e
    WHERE e.player_id = ?
    """
    . Only

getHumanPlayerByName :: Text -> Connection -> IO (Maybe HumanPlayerDb)
getHumanPlayerByName name conn =
  selectMaybe
    """
    SELECT h.player_id, h.name, h.email
    FROM human_player h
    WHERE h.name = ?
    """
    (Only name)
    conn

deletePlayerById :: PlayerIdDb -> Connection -> IO ()
deletePlayerById playerId =
  execute'
    "DELETE FROM player WHERE id = ?"
    (Only playerId)