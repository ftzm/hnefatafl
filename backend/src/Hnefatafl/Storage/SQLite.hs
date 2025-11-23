{-# LANGUAGE GADTs #-}

module Hnefatafl.Storage.SQLite (
  runStorageSQLite,
) where

import Control.Concurrent.MVar
import Data.Maybe (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple hiding (Error)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Hnefatafl.Core.Data
import Hnefatafl.Storage.Effect
import Hnefatafl.Storage.SQLite.Type

--------------------------------------------------------------------------------
-- SQLite effect

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

--------------------------------------------------------------------------------
-- utils

execute' :: ToRow p => Query -> p -> Connection -> IO ()
execute' q params conn = execute conn q params

query' :: (ToRow p, FromRow r) => Query -> p -> Connection -> IO [r]
query' q p c = query c q p

selectSingle :: (ToRow p, FromRow r) => Query -> p -> Connection -> IO r
selectSingle q p = limit <=< query' q p
 where
  limit :: [r] -> IO r
  limit [] = fail "1 element expected; found 0"
  limit [x] = pure x
  limit (_ : _) = fail "1 element expected; found >1"

selectMaybe :: (ToRow p, FromRow r) => Query -> p -> Connection -> IO (Maybe r)
selectMaybe q p = limit <=< query' q p
 where
  limit :: [r] -> IO (Maybe r)
  limit [] = pure Nothing
  limit [x] = pure $ Just x
  limit (_ : _) = fail "at most 1 element expected; found >1"

--------------------------------------------------------------------------------

createBasePlayer :: PlayerIdDb -> Text -> UTCTime -> Connection -> IO ()
createBasePlayer playerId playerType createdAt =
  execute'
    "INSERT INTO player (id, player_type, created_at) VALUES (?, ?, ?)"
    (playerId, playerType, createdAt)

createHumanPlayer ::
  HumanPlayerDb -> UTCTime -> Connection -> IO ()
createHumanPlayer player createdAt conn = do
  createBasePlayer player.playerId "human" createdAt conn
  execute'
    "INSERT INTO human_player (player_id, name, email) VALUES (?, ?, ?)"
    player
    conn

createEnginePlayer ::
  EnginePlayerDb -> UTCTime -> Connection -> IO ()
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

deletePlayerById :: PlayerIdDb -> Connection -> IO ()
deletePlayerById playerId =
  execute'
    "DELETE FROM player WHERE id = ?"
    (Only playerId)

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

--------------------------------------------------------------------------------
-- GameParticipantToken operations

gameParticipantTokenToDb ::
  GameParticipantToken -> UTCTime -> GameParticipantTokenDb
gameParticipantTokenToDb GameParticipantToken{tokenId, gameId, token, role} createdAt =
  GameParticipantTokenDb
    { tokenId = fromDomain tokenId
    , gameId = fromDomain gameId
    , token = token
    , role = fromDomain role
    , createdAt = createdAt
    , isActive = True
    }

gameParticipantTokenFromDb :: GameParticipantTokenDb -> GameParticipantToken
gameParticipantTokenFromDb GameParticipantTokenDb{tokenId, gameId, token, role} =
  GameParticipantToken
    { tokenId = toDomain tokenId
    , gameId = toDomain gameId
    , token = token
    , role = toDomain role
    }

createGameParticipantTokenDb :: GameParticipantTokenDb -> Connection -> IO ()
createGameParticipantTokenDb =
  execute'
    """
    INSERT INTO game_participant_token (id, game_id, token, role, created_at, is_active)
    VALUES (?, ?, ?, ?, ?, ?)
    """

getGameParticipantTokenByText ::
  Text -> Connection -> IO (Maybe GameParticipantTokenDb)
getGameParticipantTokenByText tokenText =
  selectMaybe
    """
    SELECT id, game_id, token, role, created_at, is_active
    FROM game_participant_token
    WHERE token = ? AND is_active = true
    """
    (Only tokenText)

getActiveTokenByGameAndRoleDb ::
  GameIdDb -> PlayerColorDb -> Connection -> IO (Maybe GameParticipantTokenDb)
getActiveTokenByGameAndRoleDb gameId role =
  selectMaybe
    """
    SELECT id, game_id, token, role, created_at, is_active
    FROM game_participant_token
    WHERE game_id = ? AND role = ? AND is_active = true
    """
    (gameId, role)

--------------------------------------------------------------------------------
-- Cheeky triple-nested applicative syntax

infixl 4 <<<$>>>

(<<<$>>>) ::
  (Functor f1, Functor f2, Functor f3) =>
  (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
(<<<$>>>) = fmap . fmap . fmap