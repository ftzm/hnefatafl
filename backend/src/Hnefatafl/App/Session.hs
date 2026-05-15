{-# LANGUAGE BlockArguments #-}

module Hnefatafl.App.Session (
  SessionEntry (..),
  tryAcquire,
  insertOrAcquire,
  release,
) where

import Focus qualified
import StmContainers.Map qualified as STMMap

-- | A session entry in the map, pairing a session handle with a
-- reference count. The refcount tracks how many active WebSocket
-- handler threads hold a reference, ensuring the entry is only
-- deleted when all handlers have exited. The session value is
-- opaque — each consumer chooses its own type (e.g. MVar for
-- AI, TBQueue for Online).
data SessionEntry a = SessionEntry
  { session :: a
  , refCount :: TVar Int
  }

-- | Try to acquire an existing session. Atomically increments the
-- refcount if found, returns Nothing if no session exists.
tryAcquire ::
  Hashable key =>
  key ->
  STMMap.Map key (SessionEntry a) ->
  STM (Maybe a)
tryAcquire =
  STMMap.focus $
    Focus.casesM
      (pure (Nothing, Focus.Leave))
      \entry -> do
        modifyTVar' entry.refCount (+ 1)
        pure (Just entry.session, Focus.Leave)

-- | Insert a new session with refcount 1, or if another thread
-- inserted first, increment the existing entry's refcount and
-- return its value (the provided value is discarded). Returns
-- the value in the map and whether this call inserted it.
insertOrAcquire ::
  Hashable key =>
  a ->
  key ->
  STMMap.Map key (SessionEntry a) ->
  STM (a, Bool)
insertOrAcquire val =
  STMMap.focus $
    Focus.casesM
      do
        refVar <- newTVar 1
        pure ((val, True), Focus.Set (SessionEntry val refVar))
      \entry -> do
        modifyTVar' entry.refCount (+ 1)
        pure ((entry.session, False), Focus.Leave)

-- | Decrement the reference count and delete the entry if it reaches zero.
release ::
  Hashable key =>
  key ->
  STMMap.Map key (SessionEntry a) ->
  STM ()
release =
  STMMap.focus $
    Focus.unitCasesM
      (pure Focus.Leave)
      \entry -> do
        modifyTVar' entry.refCount (subtract 1)
        count <- readTVar entry.refCount
        pure $ if count <= 0 then Focus.Remove else Focus.Leave
