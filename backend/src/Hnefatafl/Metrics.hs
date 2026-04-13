-- Vendored from the Effectful project repo due to missing methods and old version bounds.
{-# LANGUAGE ExplicitNamespaces #-}

module Hnefatafl.Metrics (
  -- * Handles
  Hs (..),
  HMetrics,
  initMetrics,

  -- * Domain event metrics
  recordMetrics,

  -- * Label helpers
  outcomeLabel,

  -- * Operations (pinned to Hs)
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

import Effectful (Eff, type (:>))
import Effectful.Prometheus qualified as EP
import Hnefatafl.Core.Data (Outcome (..))
import Hnefatafl.Game.Common (DomainEvent (..))
import Prometheus hiding (addCounter, decGauge, incGauge, observe, setGauge)

-- | All Prometheus metric instruments, pre-registered with the global
-- registry at startup.
data Hs = Hs
  { gamesCreated :: Vector Label1 Counter
  , gamesFinished :: Vector Label2 Counter
  , movesTotal :: Vector Label1 Counter
  , invalidMovesTotal :: Vector Label1 Counter
  , wsMessagesTotal :: Vector Label1 Counter
  , onlineSessions :: Gauge
  , aiSessions :: Gauge
  , engineSearch :: Histogram
  , dbTransaction :: Histogram
  }

-- | Register all metrics with the global prometheus-client registry.
initMetrics :: IO Hs
initMetrics =
  Hs
    <$> regVec1 "hnefatafl_games_created_total" "mode" "Total games created"
    <*> regVec2
      "hnefatafl_games_finished_total"
      ("mode", "outcome")
      "Total games finished"
    <*> regVec1 "hnefatafl_moves_total" "mode" "Total valid moves made"
    <*> regVec1 "hnefatafl_invalid_moves_total" "mode" "Total invalid move attempts"
    <*> regVec1 "hnefatafl_ws_messages_total" "kind" "Total WebSocket messages"
    <*> regGauge "hnefatafl_online_sessions_active" "Active online game sessions"
    <*> regGauge "hnefatafl_ai_sessions_active" "Active AI game sessions"
    <*> regHist "hnefatafl_engine_search_seconds" "Engine search duration"
    <*> regHist "hnefatafl_db_transaction_seconds" "DB transaction duration"
 where
  regVec1 name labelName help =
    register $ vector labelName $ counter (Info name help)
  regVec2 name labelNames help =
    register $ vector labelNames $ counter (Info name help)
  regGauge name help =
    register $ gauge (Info name help)
  regHist name help =
    register $ histogram (Info name help) defaultBuckets

-- | Record metrics for a list of domain events with the given mode label.
recordMetrics :: HMetrics :> es => Text -> [DomainEvent] -> Eff es ()
recordMetrics mode = traverse_ $ \case
  MovePlayed _ -> increaseLabelledCounter movesTotal mode
  GameEnded o -> increaseLabelledCounter gamesFinished (mode, outcomeLabel o)
  MovesUndone _ -> pure ()
  DrawOffered _ -> pure ()
  DrawDeclined -> pure ()
  UndoRequested _ -> pure ()
  UndoDeclined -> pure ()
  OfferCancelled -> pure ()

-- | Render an Outcome as a prometheus label value.
outcomeLabel :: Outcome -> Text
outcomeLabel (BlackWins _) = "black_wins"
outcomeLabel (WhiteWins _) = "white_wins"
outcomeLabel Draw = "draw"
outcomeLabel (ResignedBy _) = "resigned"
outcomeLabel (TimedOut _) = "timed_out"
outcomeLabel Abandoned = "abandoned"

-- | Convenience alias.
type HMetrics = EP.Metrics Hs

increaseCounter :: HMetrics :> es => (Hs -> Counter) -> Eff es ()
increaseCounter = EP.increaseCounter @Hs

addCounter :: HMetrics :> es => (Hs -> Counter) -> Double -> Eff es ()
addCounter = EP.addCounter @Hs

setGauge :: HMetrics :> es => (Hs -> Gauge) -> Double -> Eff es ()
setGauge = EP.setGauge @Hs

incGauge :: HMetrics :> es => (Hs -> Gauge) -> Eff es ()
incGauge = EP.incGauge @Hs

decGauge :: HMetrics :> es => (Hs -> Gauge) -> Eff es ()
decGauge = EP.decGauge @Hs

increaseLabelledCounter ::
  forall l es.
  (Label l, HMetrics :> es) => (Hs -> Vector l Counter) -> l -> Eff es ()
increaseLabelledCounter = EP.increaseLabelledCounter @l @Hs

setLabelledGauge ::
  forall l es.
  (Label l, HMetrics :> es) => (Hs -> Vector l Gauge) -> l -> Double -> Eff es ()
setLabelledGauge = EP.setLabelledGauge @l @Hs

observe :: HMetrics :> es => (Hs -> Histogram) -> Double -> Eff es ()
observe = EP.observe @Hs

observeLabelled ::
  forall l es.
  (Label l, HMetrics :> es) =>
  (Hs -> Vector l Histogram) -> l -> Double -> Eff es ()
observeLabelled = EP.observeLabelled @l @Hs
