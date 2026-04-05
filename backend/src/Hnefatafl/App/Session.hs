{-# LANGUAGE BlockArguments #-}

module Hnefatafl.App.Session (
  SessionEntry (..),
  tryAcquire,
  insertOrAcquire,
  release,
) where

import Focus qualified
import StmContainers.Map qualified as STMMap

-- | A session entry in the map, pairing the session MVar with a reference
-- count. The refcount tracks how many active WebSocket handler threads
-- hold a reference, ensuring the entry is only deleted when all handlers
-- have exited. This prevents a race where a disconnecting handler deletes
-- the entry while a reconnecting handler has already looked it up.
data SessionEntry a = SessionEntry
  { session :: MVar a
  , refCount :: TVar Int
  }

-- | Try to acquire an existing session. Atomically increments the refcount
-- if found, returns Nothing if no session exists for this key.
tryAcquire ::
  Hashable key =>
  key ->
  STMMap.Map key (SessionEntry a) ->
  STM (Maybe (MVar a))
tryAcquire =
  STMMap.focus $
    Focus.casesM
      (pure (Nothing, Focus.Leave))
      \entry -> do
        modifyTVar' entry.refCount (+ 1)
        pure (Just entry.session, Focus.Leave)

-- | Insert a new session with refcount 1, or if another thread inserted
-- first, increment the existing entry's refcount and return its MVar
-- (the provided MVar is discarded). Either way, returns the MVar that
-- is in the map.
insertOrAcquire ::
  Hashable key =>
  MVar a ->
  key ->
  STMMap.Map key (SessionEntry a) ->
  STM (MVar a)
insertOrAcquire var =
  STMMap.focus $
    Focus.casesM
      do
        refVar <- newTVar 1
        pure (var, Focus.Set (SessionEntry var refVar))
      \entry -> do
        modifyTVar' entry.refCount (+ 1)
        pure (entry.session, Focus.Leave)

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
