{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Search (
  searchWithTimeout,
  SearchTimeout (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Exception (
  finally,
  uninterruptibleMask_,
 )
import Data.Aeson (FromJSON, ToJSON)
import Foreign (alloca, poke)
import Foreign.C.Types (CBool (..))
import Hnefatafl.Bindings (SearchTrustedResult (..), searchTrusted)
import Hnefatafl.Core.Data (ExternBoard)

-- | Timeout duration in milliseconds
newtype SearchTimeout = SearchTimeout Int
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Search with a timeout using async for better resource management.
searchWithTimeout ::
  ExternBoard -> Bool -> [Word64] -> SearchTimeout -> IO SearchTrustedResult
searchWithTimeout trustedBoard isBlackTurn zobristHashes (SearchTimeout timeoutMs) = do
  alloca $ \shouldStopPtr -> do
    -- Initialize the atomic boolean to False
    poke shouldStopPtr (CBool 0)

    -- Start search thread - this will be interruptible by setting the stop flag
    searchAsync <- async $ do
      uninterruptibleMask_ $
        searchTrusted trustedBoard isBlackTurn zobristHashes shouldStopPtr

    -- Start timeout thread to flip the boolean after timeout
    timeoutAsync <-
      async $
        threadDelay (timeoutMs * 1000) -- threadDelay takes microseconds
          `finally` poke shouldStopPtr (CBool 1)

    -- Clean up
    result <- wait searchAsync
    cancel timeoutAsync

    return result
