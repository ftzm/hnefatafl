{-# LANGUAGE BlockArguments #-}

module SQLiteTest.Util where

import Control.Concurrent.MVar qualified as MVar
import Data.List (zipWith3)
import Data.Time (UTCTime)
import Database.SQLite.Simple hiding (Error)
import Database.SQLite3 qualified as SQLite3
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Storage.Effect
import Hnefatafl.Storage.SQLite (runStorageSQLite)
import Paths_hnefatafl (getDataFileName)
import Test.Hspec.Expectations.Pretty

-- | Test fixture that sets up and tears down a shared in-memory db
withSharedDB :: (MVar Connection -> IO a) -> IO a
withSharedDB action = do
  conn <- open ":memory:"
  schemaPath <- getDataFileName "db/schema.sql"
  schemaSQL <- decodeUtf8 <$> readFileBS schemaPath
  SQLite3.exec (connectionHandle conn) schemaSQL
  connectionVar <- MVar.newMVar conn
  result <- action connectionVar
  close conn
  return result

-- | Run storage effects in a transaction and roll back
runStorageTest ::
  MVar Connection ->
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es a) ->
  IO (Either String a)
runStorageTest connectionVar action = do
  -- Start transaction
  conn <- MVar.readMVar connectionVar
  execute_ conn "BEGIN TRANSACTION"
  -- Run the test
  result <-
    runEff $ runErrorNoCallStack @String $ runStorageSQLite connectionVar action
  -- Always rollback to ensure test isolation
  execute_ conn "ROLLBACK"

  return result

-- | Test utility that runs a storage action and asserts it completes without error
shouldSucceed ::
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es a) ->
  MVar Connection ->
  Expectation
shouldSucceed action connectionVar = do
  result <- runStorageTest connectionVar action
  case result of
    Left err -> expectationFailure $ "Expected success but got error: " ++ err
    Right _ -> pure ()

-- | Test utility that runs a storage action and asserts the result equals expected value
resultEquals ::
  (Eq a, Show a) =>
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es a) ->
  a ->
  MVar Connection ->
  Expectation
resultEquals action expected connectionVar = do
  result <- runStorageTest connectionVar action
  case result of
    Left err -> expectationFailure $ "Expected success but got error: " ++ err
    Right actual -> actual `shouldBe` expected

-- | Test utility that runs a storage action and asserts it fails with an error
shouldFail ::
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es a) ->
  MVar Connection ->
  Expectation
shouldFail action connectionVar = do
  result <- runStorageTest connectionVar action
  case result of
    Left _ -> pure () -- Expected failure
    Right _ -> expectationFailure "Expected failure but action succeeded"

-- * Test data utilities

-- | Base human player for testing
baseHumanPlayer :: HumanPlayer
baseHumanPlayer =
  HumanPlayer
    { playerId = PlayerId "test-human-player"
    , name = "Test Human"
    , email = Just "test@example.com"
    }

-- | Base engine player for testing
baseEnginePlayer :: EnginePlayer
baseEnginePlayer =
  EnginePlayer
    { playerId = PlayerId "test-engine-player"
    , version = "v1.0.0"
    }

-- | Base game for testing (requires current time)
baseGame :: UTCTime -> Game
baseGame now =
  Game
    { gameId = GameId "test-game"
    , name = Just "Test Game"
    , whitePlayerId = Nothing
    , blackPlayerId = Nothing
    , startTime = now
    , endTime = Nothing
    , gameStatus = Ongoing
    , createdAt = now
    }

-- | Base game move for testing (requires current time)
baseMove :: UTCTime -> GameMove
baseMove now =
  GameMove
    { moveId = MoveId "test-move"
    , moveNumber = 0
    , playerColor = White
    , move = Move 0 1
    , boardStateAfter = emptyBoard
    , timestamp = now
    }

-- | Base game participant token for testing
baseToken :: GameParticipantToken
baseToken =
  GameParticipantToken
    { tokenId = GameParticipantTokenId "test-token"
    , gameId = GameId "test-game"
    , token = "test-token-text"
    , role = White
    }

-- | Empty board state for testing
emptyBoard :: ExternBoard
emptyBoard =
  ExternBoard
    { black = Layer 0 0
    , white = Layer 0 0
    , king = 0
    }

-- | Generate a sequence of GameMoves from a list of Moves
-- Automatically handles move IDs, move numbers, and alternating player colors
-- First move is always White, then alternates
generateMoves :: UTCTime -> [Move] -> [GameMove]
generateMoves timestamp moves =
  zipWith3 makeGameMove [0..] (cycle [White, Black]) moves
  where
    makeGameMove :: Int -> PlayerColor -> Move -> GameMove
    makeGameMove moveNum color move =
      GameMove
        { moveId = MoveId ("move-" <> show moveNum)
        , moveNumber = moveNum
        , playerColor = color
        , move = move
        , boardStateAfter = emptyBoard
        , timestamp = timestamp
        }