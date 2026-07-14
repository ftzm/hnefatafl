module Hnefatafl.SearchTest where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent qualified as Concurrent
import Control.Exception (
  AsyncException (..),
  catch,
  finally,
  throwTo,
 )
import Hnefatafl.Bindings (
  SearchTrustedResult (..),
  startBoard,
 )
import Hnefatafl.Core.Data (Move (..))
import Hnefatafl.Search (SearchTimeout (..), searchWithTimeout)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec_successful_completion :: Spec
spec_successful_completion =
  describe "Search with high timeout" $ do
    it "should complete without error and return meaningful data" $ do
      let board = startBoard
      let isBlackTurn = True
      let zobristHashes = [] :: [Word64]
      let timeout = SearchTimeout 100
      result <- searchWithTimeout board isBlackTurn zobristHashes timeout False

      -- Verify we got meaningful results
      let resultMove = searchMove result
      let resultBoard = updatedBoard result
      let resultHash = updatedZobristHash result

      -- Basic sanity checks on the results
      resultMove `shouldSatisfy` (\m -> orig m >= 0 && orig m <= 120) -- Valid board positions
      resultMove `shouldSatisfy` (\m -> dest m >= 0 && dest m <= 120) -- Valid board positions
      resultMove `shouldSatisfy` (\m -> orig m /= dest m) -- Origin != destination
      resultHash `shouldSatisfy` (/= 0) -- Non-zero hash
      resultBoard `shouldSatisfy` (/= board) -- Board should change after move

spec_timeout_behavior :: Spec
spec_timeout_behavior =
  describe "Search with timeout" $
    do
      it "should stop within timeout margin" $ do
        let board = startBoard
        let isBlackTurn = True
        let zobristHashes = [] :: [Word64]
        let timeoutMs = 50
        let timeout = SearchTimeout timeoutMs
        start <- getTime Monotonic
        _ <- searchWithTimeout board isBlackTurn zobristHashes timeout False
        end <- getTime Monotonic

        let elapsedMs = fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1000000 :: Double
        -- Lower bound: the search must run at least the timeout, proving
        -- it does not stop before the stop flag is set.
        let expectedMin = fromIntegral timeoutMs :: Double
        -- Upper bound: the search must stop soon after the flag, proving
        -- the stop mechanism fires. It cannot be tight: wall-clock
        -- elapsed also includes transposition-table setup/teardown and
        -- finishing the deepening iteration in flight when the flag is
        -- set (polled between nodes, not preemptively), which alone is
        -- ~90ms and spikes higher under the parallel test runner's
        -- scheduler jitter. A *broken* stop mechanism instead runs the
        -- full depth-8 search, which from the start position takes over
        -- two minutes — so a generous ceiling still catches it
        -- unambiguously while absorbing the scheduling tail.
        let expectedMax = 2000 :: Double
        elapsedMs `shouldSatisfy` (\t -> t >= expectedMin && t <= expectedMax)

spec_exception_handling :: Spec
spec_exception_handling =
  describe "Search with exception" $ do
    it "should handle exceptions within margin" $ do
      let board = startBoard
      let isBlackTurn = True
      let zobristHashes = [] :: [Word64]
      let timeout = SearchTimeout 10000 -- High timeout, we'll interrupt before this
      let interruptAfterMs = 50
      let marginMs = 20

      start <- getTime Monotonic
      finishedMVar <- Concurrent.newEmptyMVar

      -- Start the search in a separate thread so we can interrupt it
      searchThreadId <-
        forkIO $
          ( void (searchWithTimeout board isBlackTurn zobristHashes timeout False)
              `catch` \case
                UserInterrupt -> return () -- Catch the expected UserInterrupt
                _ -> return () -- Handle other async exceptions
          )
            `finally` Concurrent.putMVar finishedMVar ()

      -- Wait for the specified time then interrupt
      threadDelay (interruptAfterMs * 1000) -- Convert to microseconds
      throwTo searchThreadId UserInterrupt

      -- Wait for the thread to actually finish
      Concurrent.takeMVar finishedMVar
      end <- getTime Monotonic

      -- Clean up
      killThread searchThreadId

      let elapsedMs = fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1000000 :: Double
      let expectedMin = fromIntegral interruptAfterMs :: Double
      let expectedMax = fromIntegral (interruptAfterMs + marginMs) :: Double

      elapsedMs `shouldSatisfy` (\t -> t >= expectedMin && t <= expectedMax)
