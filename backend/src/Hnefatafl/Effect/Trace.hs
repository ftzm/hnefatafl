{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Hnefatafl.Effect.Trace (
  Trace (..),
  inSpan,
  addSpanAttribute,
  recordSpanException,

  -- * Re-exports for callers
  OT.ToAttribute,
) where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import OpenTelemetry.Attributes qualified as OT (ToAttribute)

-- | Custom tracing effect wrapping hs-opentelemetry-api. 'inSpan' always
-- creates a span (as a child of the current thread-local context if one
-- is active, otherwise as a new root). 'addSpanAttribute' and
-- 'recordSpanException' act on whichever span is currently active in
-- OpenTelemetry's thread-local context and are silent no-ops if none is.
data Trace :: Effect where
  InSpan :: Text -> m a -> Trace m a
  AddSpanAttribute :: OT.ToAttribute v => Text -> v -> Trace m ()
  RecordSpanException :: SomeException -> Trace m ()

type instance DispatchOf Trace = Dynamic

-- | Run an action inside a named span. The span is ended when the action
-- completes or raises. Exceptions thrown by the action are automatically
-- recorded on the span by the underlying OpenTelemetry bracket.
inSpan :: (HasCallStack, Trace :> es) => Text -> Eff es a -> Eff es a
inSpan name = send . InSpan name

-- | Add an attribute to the currently active span. No-op if no span is
-- active.
addSpanAttribute ::
  (HasCallStack, Trace :> es, OT.ToAttribute v) => Text -> v -> Eff es ()
addSpanAttribute k v = send (AddSpanAttribute k v)

-- | Record an exception on the currently active span. No-op if no span is
-- active. Does not rethrow.
recordSpanException ::
  (HasCallStack, Trace :> es) => SomeException -> Eff es ()
recordSpanException = send . RecordSpanException
