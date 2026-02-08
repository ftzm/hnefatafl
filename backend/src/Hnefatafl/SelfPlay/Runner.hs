{-# LANGUAGE DataKinds #-}

module Hnefatafl.SelfPlay.Runner (
  runSelfPlayWithUI,
  runSelfPlayHeadless,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async, cancel, wait, waitEither)
import Effectful.Concurrent.STM (newTChanIO)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.Labeled (Labeled)
import Hnefatafl.Effect.Search (Search)
import Hnefatafl.SelfPlay (
  ProcessingStateSnapshot (..),
  VersionId,
  getStateFileName,
  loadOrCreateStateSnapshot,
  runSelfPlayParallel,
 )
import Hnefatafl.SelfPlay.UI (formatMoveAvg, formatScore, mkInitialScoreState, runSelfPlayUI)
import System.FilePath ((</>))

-- | Run self-play with UI - combines game execution and UI display
runSelfPlayWithUI ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  , Error Text :> es
  , FileSystem :> es
  ) =>
  Int ->
  VersionId ->
  VersionId ->
  FilePath ->
  FilePath ->
  Eff es ()
runSelfPlayWithUI numActors version1 version2 stateDir startPositionsFile = do
  -- liftIO $ putStrLn "Starting self-play with UI..."
  let stateFilePath = stateDir </> getStateFileName version1 version2
  snapshot <- loadOrCreateStateSnapshot stateFilePath startPositionsFile
  eventChan <- newTChanIO

  -- Start UI in background
  uiAsync <- async $ runSelfPlayUI snapshot eventChan

  -- Start self-play execution
  gameAsync <-
    async $
      runSelfPlayParallel
        numActors
        version1
        version2
        stateDir
        startPositionsFile
        eventChan

  -- Wait for either to complete (UI can quit early)
  -- liftIO $ putStrLn "Waiting for self-play or UI to complete..."
  result <- waitEither uiAsync gameAsync

  case result of
    Left () -> do
      -- liftIO $ putStrLn "UI closed, stopping self-play..."
      cancel gameAsync
    Right () -> do
      -- liftIO $ putStrLn "Self-play completed, waiting for user to close UI..."
      wait uiAsync

  -- liftIO $ putStrLn "Self-play with UI completed"
  pure ()

-- | Run self-play without UI - just executes games and saves state
runSelfPlayHeadless ::
  ( Labeled "new" Search :> es
  , Labeled "old" Search :> es
  , IOE :> es
  , Concurrent :> es
  , Error Text :> es
  , FileSystem :> es
  ) =>
  Int ->
  VersionId ->
  VersionId ->
  FilePath ->
  FilePath ->
  Eff es ()
runSelfPlayHeadless numActors version1 version2 stateDir startPositionsFile = do
  eventChan <- newTChanIO
  runSelfPlayParallel
    numActors
    version1
    version2
    stateDir
    startPositionsFile
    eventChan

  -- Load final state and print score summary
  let stateFilePath = stateDir </> getStateFileName version1 version2
  finalSnapshot <- loadOrCreateStateSnapshot stateFilePath startPositionsFile
  let scoreState = mkInitialScoreState finalSnapshot.completedGames
  liftIO $ putStrLn $ "Pairwise score: " <> toString (formatScore scoreState)
  liftIO $ putStrLn $ "Movecount Advantage: " <> toString (formatMoveAvg scoreState)
