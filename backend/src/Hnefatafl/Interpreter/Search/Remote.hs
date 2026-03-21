{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Search.Remote (
  runSearchRemote,
) where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Hnefatafl.Client (HnefataflClient)
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Server (Routes (..), SearchTrustedInput (..))
import Servant.Client (ClientEnv, ClientError, runClientM)

-- | Interpreter for Search effect that calls out to a remote server
runSearchRemote ::
  (IOE :> es, Error ClientError :> es) =>
  ClientEnv ->
  HnefataflClient ->
  Eff (Search : es) a ->
  Eff es a
runSearchRemote clientEnv client = interpret $ \_ -> \case
  SearchTrusted board blackToMove hashes timeout -> do
    let searchInput = SearchTrustedInput board blackToMove hashes timeout
    result <- liftIO $ runClientM (client.searchTrusted searchInput) clientEnv
    case result of
      Right searchResult -> pure searchResult
      Left clientError -> throwError clientError
