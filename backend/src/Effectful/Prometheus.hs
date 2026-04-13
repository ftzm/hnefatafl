{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Prometheus metrics effect. Vendored and extended from
-- https://github.com/haskell-effectful/prometheus-effectful
module Effectful.Prometheus (
  Metrics (..),
  runPrometheusMetrics,
  increaseCounter,
  addCounter,
  setGauge,
  incGauge,
  decGauge,
  increaseLabelledCounter,
  setLabelledGauge,
  observe,
  observeLabelled,
) where

import Effectful
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Reader.Static (asks, runReader)
import Prelude hiding (asks, runReader)
import Prometheus qualified as P

data Metrics metrics :: Effect where
  IncreaseCounter :: (metrics -> P.Counter) -> Metrics metrics m ()
  AddCounter :: (metrics -> P.Counter) -> Double -> Metrics metrics m ()
  SetGauge :: (metrics -> P.Gauge) -> Double -> Metrics metrics m ()
  IncGauge :: (metrics -> P.Gauge) -> Metrics metrics m ()
  DecGauge :: (metrics -> P.Gauge) -> Metrics metrics m ()
  IncreaseLabelledCounter :: P.Label label => (metrics -> P.Vector label P.Counter) -> label -> Metrics metrics m ()
  SetLabelledGauge :: P.Label label => (metrics -> P.Vector label P.Gauge) -> label -> Double -> Metrics metrics m ()
  Observe :: (metrics -> P.Histogram) -> Double -> Metrics metrics m ()
  ObserveLabelled :: P.Label label => (metrics -> P.Vector label P.Histogram) -> label -> Double -> Metrics metrics m ()

type instance DispatchOf (Metrics metrics) = Dynamic

runPrometheusMetrics :: IOE :> es => metricType -> Eff (Metrics metricType : es) a -> Eff es a
runPrometheusMetrics metricContainer =
  reinterpret (runReader metricContainer) $ \_ -> \case
    IncreaseCounter getter -> do
      c <- asks getter
      unsafeEff_ $ P.incCounter c
    AddCounter getter value -> do
      c <- asks getter
      unsafeEff_ $ void $ P.addCounter c value
    SetGauge getter value -> do
      g <- asks getter
      unsafeEff_ $ P.setGauge g value
    IncGauge getter -> do
      g <- asks getter
      unsafeEff_ $ P.incGauge g
    DecGauge getter -> do
      g <- asks getter
      unsafeEff_ $ P.decGauge g
    IncreaseLabelledCounter getter label -> do
      c <- asks getter
      unsafeEff_ $ P.withLabel c label P.incCounter
    SetLabelledGauge getter label value -> do
      g <- asks getter
      unsafeEff_ $ P.withLabel g label (`P.setGauge` value)
    Observe getter value -> do
      h <- asks getter
      unsafeEff_ $ P.observe h value
    ObserveLabelled getter label value -> do
      h <- asks getter
      unsafeEff_ $ P.withLabel h label (`P.observe` value)

increaseCounter :: Metrics metricType :> es => (metricType -> P.Counter) -> Eff es ()
increaseCounter getter = send (IncreaseCounter getter)

addCounter :: Metrics metricType :> es => (metricType -> P.Counter) -> Double -> Eff es ()
addCounter getter value = send (AddCounter getter value)

setGauge :: Metrics metricType :> es => (metricType -> P.Gauge) -> Double -> Eff es ()
setGauge getter value = send (SetGauge getter value)

incGauge :: Metrics metricType :> es => (metricType -> P.Gauge) -> Eff es ()
incGauge getter = send (IncGauge getter)

decGauge :: Metrics metricType :> es => (metricType -> P.Gauge) -> Eff es ()
decGauge getter = send (DecGauge getter)

increaseLabelledCounter ::
  (P.Label label, Metrics metricType :> es) =>
  (metricType -> P.Vector label P.Counter) ->
  label ->
  Eff es ()
increaseLabelledCounter getter label = send (IncreaseLabelledCounter getter label)

setLabelledGauge ::
  (P.Label label, Metrics metricType :> es) =>
  (metricType -> P.Vector label P.Gauge) ->
  label ->
  Double ->
  Eff es ()
setLabelledGauge getter label value = send (SetLabelledGauge getter label value)

observe :: Metrics metricType :> es => (metricType -> P.Histogram) -> Double -> Eff es ()
observe getter value = send (Observe getter value)

observeLabelled ::
  (P.Label label, Metrics metricType :> es) =>
  (metricType -> P.Vector label P.Histogram) ->
  label ->
  Double ->
  Eff es ()
observeLabelled getter label value = send (ObserveLabelled getter label value)
