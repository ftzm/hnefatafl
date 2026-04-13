{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Metrics.NoOp (
  runMetricsNoOp,
) where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Prometheus (Metrics (..))
import Hnefatafl.Metrics (Hs)

-- | No-op interpreter for tests and CLI tools. All metric operations
-- are silently dropped.
runMetricsNoOp :: Eff (Metrics Hs : es) a -> Eff es a
runMetricsNoOp = interpret $ \_ -> \case
  IncreaseCounter _ -> pure ()
  AddCounter _ _ -> pure ()
  SetGauge _ _ -> pure ()
  IncGauge _ -> pure ()
  DecGauge _ -> pure ()
  IncreaseLabelledCounter _ _ -> pure ()
  SetLabelledGauge _ _ _ -> pure ()
  Observe _ _ -> pure ()
  ObserveLabelled _ _ _ -> pure ()
