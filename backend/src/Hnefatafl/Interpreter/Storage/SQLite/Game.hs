module Hnefatafl.Interpreter.Storage.SQLite.Game (
  createGame,
  getGameById,
  listGamesDb,
  setOutcomeById,
  deleteGameById,
  gameToDb,
) where

import Chronos (Time)
import Control.Exception (throw)
import Database.SQLite.Simple
import Hnefatafl.Core.Data
import Hnefatafl.Exception (StorageException (..))
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

--------------------------------------------------------------------------------
-- Helpers

gameToDb :: Game -> GameDb
gameToDb Game{gameId, name, mode, startTime, endTime, outcome, createdAt} =
  GameDb
    { gameId = fromDomain gameId
    , name = name
    , gameType = case mode of
        Hotseat{} -> HotseatType
        VsAI{} -> AIType
        Online{} -> OnlineType
    , startTime = startTime
    , endTime = endTime
    , outcome = fromDomain outcome
    , createdAt = createdAt
    }

toParticipant :: Maybe PlayerIdDb -> Maybe Text -> Maybe Participant
toParticipant (Just pid) _ = Just (RegisteredPlayer (toDomain pid))
toParticipant Nothing (Just n) = Just (AnonymousPlayer n)
toParticipant Nothing Nothing = Nothing

participantPlayerId :: Maybe Participant -> Maybe PlayerIdDb
participantPlayerId (Just (RegisteredPlayer pid)) = Just (fromDomain pid)
participantPlayerId _ = Nothing

participantName :: Maybe Participant -> Maybe Text
participantName (Just (AnonymousPlayer n)) = Just n
participantName _ = Nothing

gameJoinRowToDomain :: GameJoinRow -> Game
gameJoinRowToDomain row =
  Game
    { gameId = toDomain row.gameId
    , name = row.name
    , mode = case row.gameType of
        HotseatType ->
          Hotseat (toDomain <$> row.hotseatOwnerId)
        AIType ->
          VsAI
            (toDomain <$> row.aiPlayerId)
            (toDomain (fromMaybe (throw MissingRequiredField{entity = "ai_game", field = "player_color", entityId = show row.gameId}) row.aiPlayerColor))
            (toDomain (fromMaybe (throw MissingRequiredField{entity = "ai_game", field = "engine_id", entityId = show row.gameId}) row.aiEngineId))
        OnlineType ->
          Online
            (toParticipant row.onlineWhitePlayerId row.onlineWhiteName)
            (toParticipant row.onlineBlackPlayerId row.onlineBlackName)
    , startTime = row.startTime
    , endTime = row.endTime
    , outcome = toDomain row.outcome
    , createdAt = row.createdAt
    }

--------------------------------------------------------------------------------
-- Game operations

createGame :: GameDb -> GameMode -> Connection -> IO ()
createGame gameDb mode conn = do
  execute'
    "INSERT INTO game (id, name, game_type, start_time, end_time, game_status, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
    gameDb
    conn
  let gid = gameDb.gameId
  case mode of
    Hotseat mOwner ->
      execute'
        "INSERT INTO hotseat_game (game_id, owner_id) VALUES (?, ?)"
        (gid, fromDomain @PlayerIdDb <$> mOwner)
        conn
    VsAI mPlayer color engineId ->
      execute'
        "INSERT INTO ai_game (game_id, player_id, player_color, engine_id) VALUES (?, ?, ?, ?)"
        ( gid
        , fromDomain @PlayerIdDb <$> mPlayer
        , fromDomain @PlayerColorDb color
        , fromDomain @PlayerIdDb engineId
        )
        conn
    Online w b ->
      execute'
        "INSERT INTO online_game (game_id, white_player_id, white_name, black_player_id, black_name) VALUES (?, ?, ?, ?, ?)"
        ( gid
        , participantPlayerId w
        , participantName w
        , participantPlayerId b
        , participantName b
        )
        conn

getGameById :: GameIdDb -> Connection -> IO Game
getGameById gameId conn =
  gameJoinRowToDomain
    <$> selectSingle
      """
      SELECT g.id, g.name, g.game_type, g.start_time, g.end_time, g.game_status, g.created_at,
             h.owner_id,
             a.player_id, a.player_color, a.engine_id,
             o.white_player_id, o.white_name, o.black_player_id, o.black_name
      FROM game g
      LEFT JOIN hotseat_game h ON g.id = h.game_id
      LEFT JOIN ai_game a ON g.id = a.game_id
      LEFT JOIN online_game o ON g.id = o.game_id
      WHERE g.id = ?
      """
      (Only gameId)
      conn

listGamesDb :: Connection -> IO [Game]
listGamesDb conn =
  map gameJoinRowToDomain
    <$> query'
      """
      SELECT g.id, g.name, g.game_type, g.start_time, g.end_time, g.game_status, g.created_at,
             h.owner_id,
             a.player_id, a.player_color, a.engine_id,
             o.white_player_id, o.white_name, o.black_player_id, o.black_name
      FROM game g
      LEFT JOIN hotseat_game h ON g.id = h.game_id
      LEFT JOIN ai_game a ON g.id = a.game_id
      LEFT JOIN online_game o ON g.id = o.game_id
      ORDER BY g.created_at DESC
      """
      ()
      conn

setOutcomeById ::
  GameIdDb -> OutcomeDb -> Maybe Time -> Connection -> IO ()
setOutcomeById gameId outcome endTime =
  execute'
    "UPDATE game SET game_status = ?, end_time = ? WHERE id = ?"
    (outcome, endTime, gameId)

deleteGameById :: GameIdDb -> Connection -> IO ()
deleteGameById =
  execute'
    "DELETE FROM game WHERE id = ?"
    . Only
