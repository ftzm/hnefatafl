{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Clock.IO (
  runClockIO,
) where

import Chronos qualified as C
import Control.Concurrent (threadDelay)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Hnefatafl.Effect.Clock

runClockIO :: IOE :> es => Eff (Clock : es) a -> Eff es a
runClockIO = interpret $ \env -> \case
  Now -> liftIO C.now
  Delay us -> liftIO $ threadDelay us
  Stopwatch action -> localSeqUnliftIO env $ \unlift ->
    liftIO $ C.stopwatch (unlift action)
