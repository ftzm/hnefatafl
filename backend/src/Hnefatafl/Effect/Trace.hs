{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Hnefatafl.Effect.Trace (
  Trace (..),
  inSpan,
  inSpanWithLink,
  addSpanAttribute,
  recordSpanException,

  -- * Re-exports for callers
  OT.ToAttribute,
  OT.SpanContext,
) where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import OpenTelemetry.Attributes qualified as OT (ToAttribute)
import OpenTelemetry.Trace.Core qualified as OT (SpanContext)

-- | Custom tracing effect wrapping hs-opentelemetry-api. 'inSpan' always
-- creates a span (as a child of the current thread-local context if one
-- is active, otherwise as a new root). 'inSpanWithLink' creates a root
-- span with a link to a related span context. 'addSpanAttribute' and
-- 'recordSpanException' act on whichever span is currently active in
-- OpenTelemetry's thread-local context and are silent no-ops if none is.
data Trace :: Effect where
  InSpan :: Text -> m a -> Trace m a
  InSpanWithLink :: Text -> OT.SpanContext -> m a -> Trace m a
  AddSpanAttribute :: OT.ToAttribute v => Text -> v -> Trace m ()
  RecordSpanException :: SomeException -> Trace m ()

type instance DispatchOf Trace = Dynamic

-- | Run an action inside a named span. The span is ended when the action
-- completes or raises. Exceptions thrown by the action are automatically
-- recorded on the span by the underlying OpenTelemetry bracket.
inSpan :: (HasCallStack, Trace :> es) => Text -> Eff es a -> Eff es a
inSpan name = send . InSpan name

-- | Run an action inside a new root span that links to a related span
-- context. The link expresses a causal relationship without
-- parent-child hierarchy.
inSpanWithLink ::
  (HasCallStack, Trace :> es) => Text -> OT.SpanContext -> Eff es a -> Eff es a
inSpanWithLink name ctx = send . InSpanWithLink name ctx

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
