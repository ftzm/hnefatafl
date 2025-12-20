{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.Util where

import Chronos (Time)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception.Safe (tryAny)
import Data.Typeable (typeRep)
import Database.SQLite.Simple hiding (Error)
import Database.SQLite3 qualified as SQLite3
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite (runStorageSQLite)
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

-- | Run storage effects with automatic rollback for test isolation
runStorageSQLiteWithRollback ::
  (IOE :> es, Error String :> es) =>
  MVar Connection -> Eff (Storage : es) a -> Eff es a
runStorageSQLiteWithRollback connectionVar action = do
  -- Start transaction
  conn <- liftIO $ MVar.readMVar connectionVar
  liftIO $ execute_ conn "BEGIN TRANSACTION"
  -- Run the action
  result <- runStorageSQLite connectionVar action
  -- Always rollback to ensure test isolation
  liftIO $ execute_ conn "ROLLBACK"
  return result

-- | Run storage effects in a transaction and roll back
runStorageTest ::
  MVar Connection ->
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es a) ->
  IO (Either String a)
runStorageTest connectionVar action =
  runEff $
    runErrorNoCallStack @String $
      runStorageSQLiteWithRollback connectionVar action

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

-- | Test utility that runs a storage action returning a Bool and asserts it's True
shouldBeTrue ::
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es Bool) ->
  MVar Connection ->
  Expectation
shouldBeTrue action connectionVar = do
  result <- runStorageTest connectionVar action
  case result of
    Left err -> expectationFailure $ "Expected success but got error: " ++ err
    Right False -> expectationFailure "Expected True but got False"
    Right True -> pure ()

-- | Test utility that runs a storage action and asserts it throws a specific exception type
shouldThrowException ::
  forall e a.
  Exception e =>
  (forall es. (IOE :> es, Error String :> es, Storage :> es) => Eff es a) ->
  MVar Connection ->
  Expectation
shouldThrowException action connectionVar = do
  result <- liftIO $ tryAny $ runStorageTest connectionVar action
  case result of
    Left exception -> case fromException @e exception of
      Just _ -> pure () -- Expected exception type
      Nothing ->
        expectationFailure $
          "Expected " ++ show (typeRep (Proxy @e)) ++ " but got: " ++ show exception
    Right _ ->
      expectationFailure $
        "Expected " ++ show (typeRep (Proxy @e)) ++ " but action completed normally"

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
baseGame :: Time -> Game
baseGame currentTime =
  Game
    { gameId = GameId "test-game"
    , name = Just "Test Game"
    , whitePlayerId = Nothing
    , blackPlayerId = Nothing
    , startTime = currentTime
    , endTime = Nothing
    , gameStatus = Ongoing
    , createdAt = currentTime
    }

-- | Base game move for testing (requires current time)
baseMove :: Time -> GameMove
baseMove currentTime =
  GameMove
    { playerColor = White
    , move = Move 0 1
    , boardStateAfter = emptyBoard
    , captures = Layer 0 0  -- Empty captures layer
    , timestamp = currentTime
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
-- Automatically handles alternating player colors
-- First move is always White, then alternates
generateMoves :: Time -> [Move] -> [GameMove]
generateMoves timestamp = zipWith makeGameMove (cycle [White, Black])
 where
  makeGameMove :: PlayerColor -> Move -> GameMove
  makeGameMove color move =
    GameMove
      { playerColor = color
      , move = move
      , boardStateAfter = emptyBoard
      , captures = Layer 0 0  -- Empty captures layer
      , timestamp = timestamp
      }
