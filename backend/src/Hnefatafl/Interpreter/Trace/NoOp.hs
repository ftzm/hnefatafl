{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Trace.NoOp (
  runTraceNoOp,
) where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Hnefatafl.Effect.Trace (Trace (..))

-- | No-op interpreter for 'Trace'. Used by tests so the real
-- OpenTelemetry SDK is never initialized in test runs. 'InSpan' runs its
-- inner action unchanged; 'AddSpanAttribute' and 'RecordSpanException'
-- are dropped.
runTraceNoOp :: Eff (Trace : es) a -> Eff es a
runTraceNoOp = interpret $ \env -> \case
  InSpan _ action ->
    localSeqUnlift env $ \unlift -> unlift action
  AddSpanAttribute _ _ -> pure ()
  RecordSpanException _ -> pure ()
