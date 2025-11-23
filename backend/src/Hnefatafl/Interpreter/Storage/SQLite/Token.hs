module Hnefatafl.Interpreter.Storage.SQLite.Token (
  gameParticipantTokenToDb,
  gameParticipantTokenFromDb,
  createGameParticipantTokenDb,
  getGameParticipantTokenByText,
  getActiveTokenByGameAndRoleDb,
) where

import Data.Time (UTCTime)
import Database.SQLite.Simple
import Hnefatafl.Core.Data
import Hnefatafl.Interpreter.Storage.SQLite.Type
import Hnefatafl.Interpreter.Storage.SQLite.Util

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