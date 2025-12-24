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
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Search (searchWithTimeout)

-- | the search function monopolizes a bound (OS) thread, so we guard it with
-- a semaphore to limit simultaneous searches
runSearchLocal ::
  (IOE :> es, Concurrent :> es) => QSem -> Eff (Search : es) a -> Eff es a
runSearchLocal qsem = interpret $ \_ -> \case
  SearchTrusted board blackToMove hashes timeout ->
    withSem (liftIO $ searchWithTimeout board blackToMove hashes timeout)
 where
  withSem = bracket_ (waitQSem qsem) (signalQSem qsem)
