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
import Hnefatafl.Effect.Log (Log)
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Search (searchWithTimeout)
import Katip (Severity (..), katipAddNamespace, logTM, ls)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)

-- | the search function monopolizes a bound (OS) thread, so we guard it with
-- a semaphore to limit simultaneous searches
runSearchLocal ::
  (IOE :> es, Concurrent :> es, Log :> es) => QSem -> Eff (Search : es) a -> Eff es a
runSearchLocal qsem = interpret $ \_ -> \case
  SearchTrusted board blackToMove hashes timeout enableAdminEndings ->
    katipAddNamespace "search" $ do
      start <- liftIO $ getTime Monotonic
      result <-
        withSem $
          liftIO $
            searchWithTimeout board blackToMove hashes timeout enableAdminEndings
      end <- liftIO $ getTime Monotonic
      let durationMs = toNanoSecs (diffTimeSpec start end) `div` 1_000_000
      $(logTM) DebugS $ ls @Text ("search completed in " <> show durationMs <> "ms")
      pure result
 where
  withSem = bracket_ (waitQSem qsem) (signalQSem qsem)
