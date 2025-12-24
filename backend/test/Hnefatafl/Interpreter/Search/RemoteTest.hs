{-# LANGUAGE ExplicitNamespaces #-}

module Hnefatafl.Interpreter.Search.RemoteTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Effectful (Eff, runEff, type (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (runErrorNoCallStack)
import Hnefatafl.Bindings (SearchTrustedResult (..), startBoard)
import Hnefatafl.Client (HnefataflClient, createClient)
import Hnefatafl.Core.Data (ExternBoard, Move (..))
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Interpreter.Search.Remote (runSearchRemote)
import Hnefatafl.Search (SearchTimeout (..))
import Hnefatafl.Server (runServer)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (
  ClientEnv,
  ClientError,
  mkClientEnv,
  parseBaseUrl,
 )
import Test.Hspec (Spec, afterAll, beforeAll, describe, it, shouldSatisfy)

spec_remote_search_interpreter :: Spec
spec_remote_search_interpreter = beforeAll setupTestServer $ afterAll teardownTestServer $ do
  describe "Remote Search interpreter tests" $ do
    it "should perform search via remote interpreter" $ \((clientEnv, client), _) -> do
      result <-
        runEff $
          runErrorNoCallStack @ClientError $
            runSearchRemote clientEnv client $
              searchTrustedViaEffect startBoard True [] (SearchTimeout 1000)

      result `shouldSatisfy` \case
        Right searchResult ->
          let move = searchMove searchResult
              newBoard = updatedBoard searchResult
              hash = updatedZobristHash searchResult
           in move.orig >= 0
                && move.orig <= 120
                && move.dest >= 0 -- Valid board position
                && move.dest <= 120
                && move.orig /= move.dest -- Valid board position
                && newBoard /= startBoard -- Different positions
                && hash /= 0 -- Board changed
        Left _ -> False

-- Helper function to call the Search effect
searchTrustedViaEffect ::
  Search :> eff =>
  ExternBoard ->
  Bool ->
  [Word64] ->
  SearchTimeout ->
  Eff eff SearchTrustedResult
searchTrustedViaEffect board blackToMove hashes timeout =
  send $ SearchTrusted board blackToMove hashes timeout

setupTestServer :: IO ((ClientEnv, HnefataflClient), Async ())
setupTestServer = do
  let testPort = 18081
  manager <- newManager defaultManagerSettings
  baseUrl <- parseBaseUrl $ "http://localhost:" ++ show testPort
  let clientEnv = mkClientEnv manager baseUrl

  serverAsync <- async (runServer testPort)
  -- Wait a moment for server to start
  threadDelay 100000 -- 100ms
  pure ((clientEnv, createClient), serverAsync)

teardownTestServer :: ((ClientEnv, HnefataflClient), Async ()) -> IO ()
teardownTestServer (_, serverAsync) = cancel serverAsync
