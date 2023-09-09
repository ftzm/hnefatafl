{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Game where

import Board.Board (Board, readBoard, serializeBoard)
import Data.Maybe (fromJust)
import Data.Text (intercalate)
import Data.UUID (UUID, fromText, toText)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField
import Prelude hiding (intercalate, toText)

--------------------------------------------------------------------------------
-- Orphan instances

instance ToRow UUID where
  toRow = toRow . Only . toText

instance FromField UUID where
  fromField textVal = fromJust . fromText <$> fromField textVal

instance ToField UUID where
  toField = toField . toText

--------------------------------------------------------------------------------
-- types and instances to move out

instance FromField Board where
  fromField stringVal = readBoard <$> fromField stringVal

instance ToField Board where
  toField = toField . serializeBoard

data Game = Game
  { id :: UUID
  , board :: Board
  , isBlackTurn :: Bool
  }
  deriving (Generic)
deriving anyclass instance FromRow Game

data AIGame = AIGame
  { id :: UUID
  , board :: Board
  , isBlackTurn :: Bool
  , humanIsBlack :: Bool
  }
  deriving (Generic)
deriving anyclass instance FromRow AIGame

data VsGame = VsGame
  { gameId :: UUID
  , whiteId :: UUID
  , blackId :: UUID
  , board :: Board
  , isBlackTurn :: Bool
  }
  deriving (Generic)
deriving anyclass instance FromRow VsGame

--------------------------------------------------------------------------------
-- utils

selectSingle :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO r
selectSingle c q = limit <=< query c q
 where
  limit :: [r] -> IO r
  limit [] = fail "1 element expected; found 0"
  limit [x] = pure x
  limit (_ : _) = fail "1 element expected; found >1"

ql :: [Text] -> Query
ql = Query . intercalate " "

--------------------------------------------------------------------------------
-- game

insertGame :: Connection -> Game -> IO ()
insertGame c g =
  execute
    c
    "INSERT INTO game (id, board, is_black_turn) VALUES (?, ?, ?)"
    (g.id, g.board, g.isBlackTurn)

selectGame :: Connection -> UUID -> IO Game
selectGame c = selectSingle c "SELECT id, board, is_black_turn FROM game WHERE id = ?"

updateGame :: Connection -> UUID -> Board -> Bool -> IO ()
updateGame c i b isBlackTurn =
  execute
    c
    "UPDATE game SET board = ?, is_black_turn = ? WHERE id = ?"
    (b, isBlackTurn, i)

--------------------------------------------------------------------------------
-- hotseat

insertHotseat :: Connection -> UUID -> IO ()
insertHotseat c hId =
  execute
    c
    "INSERT INTO hotseat (id) VALUES (?)"
    (Only hId)

selectHotseat :: Connection -> UUID -> IO Game
selectHotseat c =
  selectSingle
    c
    $ ql
      [ "SELECT g.id, g.board, g.is_black_turn"
      , "FROM game g "
      , "JOIN hotseat h on g.id = h.id "
      , "WHERE h.id = ?"
      ]

createHotseat :: Connection -> Game -> IO ()
createHotseat conn game = withTransaction conn $ do
  insertGame conn game
  insertHotseat conn game.id

--------------------------------------------------------------------------------
-- ai

insertAI :: Connection -> UUID -> Bool -> IO ()
insertAI c hId humanIsBlack =
  execute
    c
    "INSERT INTO ai (id, human_is_black) VALUES (?, ?)"
    (hId, humanIsBlack)

selectAI :: Connection -> UUID -> IO AIGame
selectAI c =
  selectSingle
    c
    $ ql
      [ "SELECT g.id, g.board, g.is_black_turn, a.human_is_black"
      , "FROM game g "
      , "JOIN ai a on g.id = a.id "
      , "WHERE a.id = ?"
      ]

createAI :: Connection -> Game -> Bool -> IO ()
createAI conn game humanIsBlack = withTransaction conn $ do
  insertGame conn game
  insertAI conn game.id humanIsBlack

--------------------------------------------------------------------------------
-- ai

insertVs :: Connection -> UUID -> UUID -> UUID -> IO ()
insertVs c gameId whiteId blackId =
  execute
    c
    "INSERT INTO vs (game_id, white_id, black_id) VALUES (?, ?, ?)"
    (gameId, whiteId, blackId)

selectVs :: Connection -> Bool -> UUID -> IO VsGame
selectVs c isBlack playerId =
  selectSingle
    c
    ( ql
        [ "SELECT g.id, vs.white_id, vs.black_id, g.board, g.is_black_turn"
        , "FROM game g "
        , "JOIN vs on vs.game_id = g.id "
        , "WHERE vs.? = ?"
        ]
    )
    (playerColor, playerId)
 where
  playerColor :: Text = if isBlack then "black_id" else "white_id"

createVs :: Connection -> Game -> UUID -> UUID -> IO ()
createVs conn game whiteId blackId = do
  insertGame conn game
  insertVs conn game.id whiteId blackId
