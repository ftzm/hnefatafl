{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Clock.IO (
  runClockIO,
) where

import Chronos qualified as C
import Effectful
import Effectful.Dispatch.Dynamic
import Hnefatafl.Effect.Clock

runClockIO :: IOE :> es => Eff (Clock : es) a -> Eff es a
runClockIO = interpret $ \_ -> \case
  Now -> liftIO C.now
