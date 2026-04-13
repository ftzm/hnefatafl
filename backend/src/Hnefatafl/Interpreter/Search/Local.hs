{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Search.Local (
  runSearchLocal,
) where

import Effectful (Eff, IOE, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.QSem (QSem, signalQSem, waitQSem)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (bracket_)
import Effectful.Katip (KatipE, katipAddNamespace, logTM)
import Chronos (getTimespan)
import Hnefatafl.Effect.Clock (Clock, stopwatch)
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Effect.Trace (Trace, addSpanAttribute, inSpan)
import Hnefatafl.Metrics (HMetrics, Hs (..), observe)
import Hnefatafl.Search (SearchTimeout (..), searchWithTimeout)
import Katip (Severity (..), ls)

-- | the search function monopolizes a bound (OS) thread, so we guard it with
-- a semaphore to limit simultaneous searches
runSearchLocal ::
  (IOE :> es, Concurrent :> es, KatipE :> es, Trace :> es, HMetrics :> es, Clock :> es) =>
  QSem -> Eff (Search : es) a -> Eff es a
runSearchLocal qsem = interpret $ \_ -> \case
  SearchTrusted
    board
    blackToMove
    hashes
    (SearchTimeout timeoutMs)
    enableAdminEndings ->
      inSpan "engine.search" $ katipAddNamespace "search" $ do
        addSpanAttribute "search.timeout_ms" timeoutMs
        addSpanAttribute "search.enable_admin_endings" enableAdminEndings
        (elapsed, result) <-
          stopwatch $
            withSem $
              liftIO $
                searchWithTimeout
                  board
                  blackToMove
                  hashes
                  (SearchTimeout timeoutMs)
                  enableAdminEndings
        let durationMs = getTimespan elapsed `div` 1_000_000
            durationSec = fromIntegral (getTimespan elapsed) / 1_000_000_000
        observe engineSearch durationSec
        $(logTM) DebugS $ ls @Text ("search completed in " <> show durationMs <> "ms")
        pure result
 where
  withSem = bracket_ (waitQSem qsem) (signalQSem qsem)
