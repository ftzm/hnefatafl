module Hnefatafl.Effect.Log (
  -- * Effect
  KatipE,

  -- * Runners
  runKatipE,
  runKatipContextE,

  -- * Logging
  logTM,
  ls,

  -- * Context
  katipAddNamespace,
  katipAddContext,
  sl,

  -- * Re-exports
  Severity (..),
) where

import Effectful.Katip (
  KatipE,
  katipAddContext,
  katipAddNamespace,
  logTM,
  runKatipContextE,
  runKatipE,
 )
import Katip (Severity (..), ls, sl)
