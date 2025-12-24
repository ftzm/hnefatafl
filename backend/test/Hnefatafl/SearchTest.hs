module Hnefatafl.SearchTest where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent qualified as Concurrent
import Control.Exception (
  AsyncException (UserInterrupt),
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
      let timeout = SearchTimeout 10000 -- 10 seconds, should be plenty
      result <- searchWithTimeout board isBlackTurn zobristHashes timeout

      -- Verify we got meaningful results
      let resultMove = searchMove result
      let resultBoard = updatedBoard result
      let resultHash = updatedZobristHash result
      let resultStatus = gameStatus result

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
        let timeoutMs = 50 -- Increase timeout to see if search naturally takes longer
        let timeout = SearchTimeout timeoutMs
        let marginMs = 10 -- Allow larger margin
        start <- getTime Monotonic
        result <- searchWithTimeout board isBlackTurn zobristHashes timeout
        end <- getTime Monotonic

        let elapsedMs = fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1000000
        let expectedMin = fromIntegral timeoutMs
        let expectedMax = fromIntegral (timeoutMs + marginMs)

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
          ( void (searchWithTimeout board isBlackTurn zobristHashes timeout)
              `catch` \UserInterrupt ->
                return () -- Catch the expected UserInterrupt
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

      let elapsedMs = fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1000000
      let expectedMin = fromIntegral interruptAfterMs
      let expectedMax = fromIntegral (interruptAfterMs + marginMs)

      elapsedMs `shouldSatisfy` (\t -> t >= expectedMin && t <= expectedMax)
