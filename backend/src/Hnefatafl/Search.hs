{-# LANGUAGE DeriveAnyClass #-}

module Hnefatafl.Search (
  searchWithTimeout,
  SearchTimeout (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Exception (bracket, finally, uninterruptibleMask_)
import Data.Aeson (FromJSON, ToJSON)
import Foreign (alloca, poke)
import Foreign.C.Types (CBool (..))
import Hnefatafl.Bindings (
  SearchTrustedResult (..),
  searchTrusted,
  ttCreate,
  ttDestroy,
 )
import Hnefatafl.Core.Data (ExternBoard)

-- | Timeout duration in milliseconds
newtype SearchTimeout = SearchTimeout Int
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Search with a timeout using async for better resource management.
searchWithTimeout ::
  ExternBoard ->
  Bool ->
  [Word64] ->
  SearchTimeout ->
  Bool ->
  IO SearchTrustedResult
searchWithTimeout trustedBoard isBlackTurn zobristHashes (SearchTimeout timeoutMs) enableAdminEndings = do
  alloca $ \shouldStopPtr -> do
    -- Initialize the atomic boolean to False
    poke shouldStopPtr (CBool 0)

    -- Start search thread - this is interruptible by setting the stop flag
    searchAsync <- async $ do
      uninterruptibleMask_ $
        bracket (ttCreate 256) ttDestroy $ \ttPtr ->
          searchTrusted
            trustedBoard
            isBlackTurn
            zobristHashes
            shouldStopPtr
            ttPtr
            enableAdminEndings

    -- Start timeout thread to set the stop boolean after timeout
    timeoutAsync <-
      async $
        threadDelay (timeoutMs * 1000) -- threadDelay takes microseconds
          `finally` poke shouldStopPtr (CBool 1)

    -- Clean up
    result <- wait searchAsync
    cancel timeoutAsync

    return result
