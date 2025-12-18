{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.IdGen (
  module Hnefatafl.Effect.IdGen,
) where

import Effectful
import Effectful.TH

data IdGen :: Effect where
  GenerateId :: Coercible Text id => IdGen m id

makeEffect ''IdGen
