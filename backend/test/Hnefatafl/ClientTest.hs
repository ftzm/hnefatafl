module Hnefatafl.ClientTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Hnefatafl.Bindings (SearchTrustedResult (..), startBoard)
import Hnefatafl.Client (HnefataflClient, createClient)
import Hnefatafl.Core.Data (Move (..))
import Hnefatafl.Search (SearchTimeout (..))
import Hnefatafl.Server (
  HealthResponse (..),
  Routes (..),
  SearchTrustedInput (..),
  VersionResponse (..),
  runServer,
 )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (
  ClientEnv,
  ClientError (..),
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
import Test.Hspec (Spec, afterAll, beforeAll, describe, it, shouldSatisfy)

spec_client_integration :: Spec
spec_client_integration = beforeAll setupTestServer $ afterAll teardownTestServer $ do
  describe "Client integration tests" $ do
    it "should get version information" $ \((clientEnv, client), _) -> do
      result <- runClientM client.version clientEnv
      result `shouldSatisfy` \case
        Right versionResp -> versionResp.versionNumber == "0.0.0.2"
        Left _ -> False

    it "should get health status" $ \((clientEnv, client), _) -> do
      result <- runClientM client.health clientEnv
      result `shouldSatisfy` \case
        Right healthResp -> healthResp.status == "OK"
        Left _ -> False

    it "should perform a trusted search" $ \((clientEnv, client), _) -> do
      let searchInput = SearchTrustedInput
            { board = startBoard
            , blackToMove = True
            , hashes = []
            , timeout = SearchTimeout 1000  -- 1 second timeout
            }
      result <- runClientM (client.searchTrusted searchInput) clientEnv
      result `shouldSatisfy` \case
        Right searchResult ->
          -- Basic validation that we got a valid search result
          let move = searchMove searchResult
              newBoard = updatedBoard searchResult
              hash = updatedZobristHash searchResult
          in move.orig >= 0 && move.orig <= 120 &&  -- Valid board position
             move.dest >= 0 && move.dest <= 120 &&  -- Valid board position
             move.orig /= move.dest &&               -- Different positions
             newBoard /= startBoard &&               -- Board changed
             hash /= 0                               -- Non-zero hash
        Left _ -> False

spec_client_errors :: Spec
spec_client_errors =
  describe "Client error handling" $ do
    it "should handle connection errors gracefully" $ do
      -- Test with invalid port to ensure error handling works
      manager <- newManager defaultManagerSettings
      baseUrl <- parseBaseUrl "http://localhost:99999"
      let badClientEnv = mkClientEnv manager baseUrl
      let client = createClient

      result <- runClientM client.version badClientEnv
      result `shouldSatisfy` \case
        Left (ConnectionError _) -> True
        Left _ -> True -- Any client error is fine for this test
        Right _ -> False

-- Setup function to start a shared test server and create client
setupTestServer :: IO ((ClientEnv, HnefataflClient), Async ())
setupTestServer = do
  -- Find an available port (simple approach - could be improved)
  let testPort = 18080
  manager <- newManager defaultManagerSettings
  baseUrl <- parseBaseUrl $ "http://localhost:" ++ show testPort
  let clientEnv = mkClientEnv manager baseUrl
  let client = createClient

  -- Start server in background
  serverAsync <- async (runServer testPort)
  -- Wait a moment for server to start
  threadDelay 100000 -- 100ms
  pure ((clientEnv, client), serverAsync)

-- Teardown function to stop the shared test server
teardownTestServer :: ((ClientEnv, HnefataflClient), Async ()) -> IO ()
teardownTestServer (_, serverAsync) = cancel serverAsync
