{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Clock.Test (
  runClockTest,
) where

import Chronos (Time, Timespan (..))
import Chronos qualified as C
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM qualified as STM
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Hnefatafl.Effect.Clock (Clock (..))
import Torsor (add)

-- | Test clock interpreter driven by a TVar. 'Now' reads the
-- current controlled time. 'Delay' blocks via STM retry until
-- the controlled time advances past the target. Tests advance
-- time by writing to the TVar.
runClockTest ::
  (IOE :> es, Concurrent :> es) =>
  TVar Time ->
  Eff (Clock : es) a ->
  Eff es a
runClockTest timeVar = interpret $ \env -> \case
  Now -> STM.atomically $ STM.readTVar timeVar
  Delay us -> do
    start <- STM.atomically $ STM.readTVar timeVar
    let target = add (Timespan (fromIntegral us * 1000)) start
    STM.atomically $ do
      current <- STM.readTVar timeVar
      when (current < target) STM.retry
  DelayUntil target ->
    STM.atomically $ do
      current <- STM.readTVar timeVar
      when (current < target) STM.retry
  Stopwatch action -> localSeqUnliftIO env $ \unlift ->
    liftIO $ C.stopwatch (unlift action)
