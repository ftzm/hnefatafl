{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Clock.IO (
  runClockIO,
) where

import Chronos (Timespan (..))
import Chronos qualified as C
import Control.Concurrent (threadDelay)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Hnefatafl.Effect.Clock
import Torsor (difference)

runClockIO :: IOE :> es => Eff (Clock : es) a -> Eff es a
runClockIO = interpret $ \env -> \case
  Now -> liftIO C.now
  Delay us -> liftIO $ threadDelay us
  DelayUntil target -> liftIO $ do
    current <- C.now
    let remainingNs = getTimespan (difference target current)
    threadDelay $ max 0 (fromIntegral (remainingNs `div` 1000))
  Stopwatch action -> localSeqUnliftIO env $ \unlift ->
    liftIO $ C.stopwatch (unlift action)
