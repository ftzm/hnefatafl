{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.Clock (
  module Hnefatafl.Effect.Clock,
) where

import Chronos (Time, Timespan)
import Effectful (Effect)
import Effectful.TH (makeEffect)

data Clock :: Effect where
  -- | Current time.
  Now :: Clock m Time
  -- | Block for the given number of microseconds.
  Delay :: Int -> Clock m ()
  -- | Measure the duration of an action.
  Stopwatch :: m a -> Clock m (Timespan, a)

makeEffect ''Clock
