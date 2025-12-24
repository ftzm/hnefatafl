{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.Search (
  module Hnefatafl.Effect.Search,
) where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Hnefatafl.Bindings (SearchTrustedResult)
import Hnefatafl.Core.Data (ExternBoard)
import Hnefatafl.Search (SearchTimeout)

data Search :: Effect where
  SearchTrusted ::
    ExternBoard ->
    Bool ->
    [Word64] ->
    SearchTimeout ->
    Search m SearchTrustedResult

makeEffect ''Search
