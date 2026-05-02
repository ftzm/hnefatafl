{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.TransactionTest where

import Chronos (now)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (ErrorCall (..))
import Control.Exception.Safe (bracket, throwIO, tryAny)
import Data.IORef qualified as IORef
import Database.SQLite.Simple (
  Connection,
  Only (..),
  close,
  execute_,
  open,
  query_,
 )
import Database.SQLite.Simple.Internal (connectionHandle)
import Database.SQLite3 qualified as SQLite3
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Katip (runKatipE)
import Hnefatafl.Core.Data
import Hnefatafl.Effect.Storage
import Hnefatafl.Exception (ConnectionUnrecoverableException (..))
import Hnefatafl.Interpreter.Clock.IO (runClockIO)
import Hnefatafl.Interpreter.Metrics.NoOp (runMetricsNoOp)
import Hnefatafl.Interpreter.Storage.SQLite (
  runStorageSQLite,
  withConnectionRecovery,
 )
import Hnefatafl.Interpreter.Storage.SQLite.Util (baseGame)
import Hnefatafl.Interpreter.Trace.NoOp (runTraceNoOp)
import Hnefatafl.Logging (withNoLogEnv)
import Optics
import Paths_hnefatafl (getDataFileName)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec (Spec, around, describe, it)
import Test.Hspec.Expectations.Pretty

-- | Fresh on-disk SQLite database with the production schema. The caller
-- receives the under-test 'MVar' Connection and the file path; they can
-- open additional connections via the path to verify what's actually
-- committed (i.e. visible to a *different* connection).
withFreshDB :: ((MVar Connection, FilePath) -> IO a) -> IO a
withFreshDB action =
  withSystemTempFile "hnefatafl-test-.db" $ \path handle -> do
    hClose handle
    bracket
      ( do
          conn <- open path
          schemaPath <- getDataFileName "db/schema.sql"
          schemaSQL <- decodeUtf8 <$> readFileBS schemaPath
          SQLite3.exec (connectionHandle conn) schemaSQL
          MVar.newMVar conn
      )
      (\v -> MVar.readMVar v >>= close)
      (\v -> action (v, path))

-- | Like 'withFreshDB' but threads a counter through the open-connection
-- action so tests can observe how many times a connection has been
-- (re)opened. Returns @(counter, connectionVar, openConn)@. The counter
-- starts at 1 — the initial open performed by the fixture itself.
withCountingDB ::
  ((IORef.IORef Int, MVar Connection, IO Connection) -> IO a) -> IO a
withCountingDB action =
  withSystemTempFile "hnefatafl-test-.db" $ \path handle -> do
    hClose handle
    counter <- IORef.newIORef 0
    let openConn = do
          IORef.modifyIORef' counter (+ 1)
          open path
    bracket
      ( do
          conn <- openConn
          schemaPath <- getDataFileName "db/schema.sql"
          schemaSQL <- decodeUtf8 <$> readFileBS schemaPath
          SQLite3.exec (connectionHandle conn) schemaSQL
          MVar.newMVar conn
      )
      (\v -> MVar.readMVar v >>= close)
      (\v -> action (counter, v, openConn))

-- | Runs a Storage action against the test connection without any test-level
-- transaction wrapping (so commits are real and visible from other
-- connections). The 'IO Connection' is the action 'runStorageSQLite' uses
-- to open a replacement connection if a transaction throws
-- 'ConnectionUnrecoverableException'.
runStorage ::
  MVar Connection ->
  IO Connection ->
  (forall es. (IOE :> es, Storage :> es) => Eff es a) ->
  IO a
runStorage connectionVar openConn action =
  withNoLogEnv "test" $ \logEnv ->
    runEff
      . runConcurrent
      . runKatipE logEnv
      . runTraceNoOp
      . runMetricsNoOp
      . runClockIO
      $ runStorageSQLite connectionVar openConn action

-- | Runs an arbitrary 'Connection'-using effectful action under
-- 'withConnectionRecovery' for direct testing of the recovery path.
runWithRecovery ::
  MVar Connection ->
  IO Connection ->
  (forall es. IOE :> es => Connection -> Eff es a) ->
  IO a
runWithRecovery connectionVar openConn action =
  runEff . runConcurrent $ withConnectionRecovery connectionVar openConn action

-- | Read 'game.id' values via a separate connection, sorted for stable
-- comparison.
queryGameIds :: FilePath -> IO [Text]
queryGameIds path =
  bracket (open path) close $ \c -> do
    rows <- query_ @(Only Text) c "SELECT id FROM game"
    pure $ sort $ map fromOnly rows

spec_Transaction :: Spec
spec_Transaction = do
  around withFreshDB $ do
    describe "runTransaction persistence" $ do
      it "successful transaction is visible from a separate connection" $
        \(v, path) -> do
          t <- now
          runStorage v (open path) (runTransaction $ insertGame (baseGame t))
          ids <- queryGameIds path
          ids `shouldBe` ["test-game"]

      it "failed transaction rolls back its writes" $ \(v, path) -> do
        t <- now
        let g = baseGame t & #name .~ Nothing
            other = g & #gameId .~ GameId "should-not-persist"
        -- Attempt: insert 'other' (would succeed) then 'g' twice (the
        -- second insertGame g triggers a PK violation that rolls back
        -- the whole transaction, including 'other').
        _ <- tryAny $ runStorage v (open path) $ runTransaction $ do
          insertGame other
          insertGame g
          insertGame g
        ids <- queryGameIds path
        ids `shouldBe` []

      it "subsequent transaction persists after a previous one failed" $
        \(v, path) -> do
          t <- now
          let g = baseGame t & #name .~ Nothing
              afterFailure = g & #gameId .~ GameId "after-failure"
          runStorage v (open path) (runTransaction $ insertGame g)
          _ <- tryAny $ runStorage v (open path) $ runTransaction $ do
            insertGame g -- PK violation
          runStorage v (open path) (runTransaction $ insertGame afterFailure)
          ids <- queryGameIds path
          ids `shouldBe` ["after-failure", "test-game"]

      it "successful transaction after a failed one survives close+reopen" $
        \(v, path) -> do
          t <- now
          let g = baseGame t & #name .~ Nothing
              afterFailure = g & #gameId .~ GameId "after-failure"
          runStorage v (open path) (runTransaction $ insertGame g)
          _ <- tryAny $ runStorage v (open path) $ runTransaction $ do
            insertGame g -- PK violation
          runStorage v (open path) (runTransaction $ insertGame afterFailure)
          -- Simulate a backend restart: close the under-test connection,
          -- then read via a freshly opened one.
          MVar.readMVar v >>= close
          newConn <- open path
          _ <- MVar.swapMVar v newConn
          ids <- queryGameIds path
          ids `shouldBe` ["after-failure", "test-game"]

    describe "nested savepoints" $ do
      it
        "outer test transaction can roll back work done by an inner runTransaction"
        $ \(v, path) -> do
          t <- now
          let g = baseGame t
          conn <- MVar.readMVar v
          execute_ conn "SAVEPOINT outer_test"
          runStorage v (open path) (runTransaction $ insertGame g)
          countGames conn `shouldReturn` 1
          execute_ conn "ROLLBACK TO outer_test"
          execute_ conn "RELEASE outer_test"
          countGames conn `shouldReturn` 0

      it
        ( "outer test transaction is preserved when an inner runTransaction"
            <> " fails normally"
        )
        $ \(v, path) -> do
          t <- now
          let g = baseGame t & #name .~ Nothing
              after = g & #gameId .~ GameId "after-failure"
          conn <- MVar.readMVar v
          execute_ conn "BEGIN"
          -- An inner failure whose cleanup succeeds must not destroy the
          -- outer transaction. The follow-up insert lands inside the
          -- outer transaction's pending state, observable on the same
          -- connection but not via a separate connection.
          _ <- tryAny $ runStorage v (open path) $ runTransaction $ do
            insertGame g
            insertGame g -- PK violation
          runStorage v (open path) (runTransaction $ insertGame after)
          countGames conn `shouldReturn` 1
          execute_ conn "ROLLBACK"
          countGames conn `shouldReturn` 0

  around withCountingDB $ do
    describe "withConnectionRecovery" $ do
      it
        ( "replaces the connection when the action throws"
            <> " ConnectionUnrecoverableException"
        )
        $ \(counter, v, openConn) -> do
          IORef.readIORef counter `shouldReturn` 1 -- initial open
          let underlying = toException (ErrorCall "underlying cause")
          _ <- tryAny $ runWithRecovery v openConn $ \_conn ->
            throwIO (ConnectionUnrecoverableException underlying)
          IORef.readIORef counter `shouldReturn` 2

      it
        ( "leaves the connection in place when the action throws a"
            <> " different exception"
        )
        $ \(counter, v, openConn) -> do
          IORef.readIORef counter `shouldReturn` 1
          _ <- tryAny $ runWithRecovery v openConn $ \_conn ->
            throwIO (ErrorCall "regular failure")
          IORef.readIORef counter `shouldReturn` 1

      it "returns the action's result on success without touching the conn" $
        \(counter, v, openConn) -> do
          IORef.readIORef counter `shouldReturn` 1
          result <- runWithRecovery v openConn $ \_conn -> pure (42 :: Int)
          result `shouldBe` 42
          IORef.readIORef counter `shouldReturn` 1
  where
    countGames :: Connection -> IO Int
    countGames c = do
      rows <- query_ @(Only Int) c "SELECT COUNT(*) FROM game"
      pure $ maybe 0 fromOnly (viaNonEmpty head rows)
