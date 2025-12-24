{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.Clock (
  module Hnefatafl.Effect.Clock,
) where

import Chronos (Time)
import Effectful (Effect)
import Effectful.TH (makeEffect)

data Clock :: Effect where
  Now :: Clock m Time

makeEffect ''Clock
