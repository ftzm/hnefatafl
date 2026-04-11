{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Trace.OTel (
  runTraceOTel,
  setKatipTraceId,
) where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Exception (bracket, catch, throwIO)
import Effectful.Katip (KatipE, katipAddContext)
import Hnefatafl.Effect.Trace (Trace (..))
import Katip (sl)
import OpenTelemetry.Context (insertSpan, lookupSpan)
import OpenTelemetry.Context.ThreadLocal (
  attachContext,
  detachContext,
  getContext,
 )
import OpenTelemetry.Trace (
  Tracer,
  addAttribute,
  defaultSpanArguments,
  endSpan,
  recordException,
  setStatus,
  traceId,
 )
import OpenTelemetry.Trace.Core (SpanStatus (Error), createSpanWithoutCallStack)
import OpenTelemetry.Trace.Core qualified as OT (getSpanContext)
import OpenTelemetry.Trace.Id (Base (..), traceIdBaseEncodedText)
import Prelude hiding (traceId)

-- | Interpret the 'Trace' effect using hs-opentelemetry. Each 'InSpan'
-- creates a real OT span, extracts its trace ID, and attaches that trace
-- ID to the Katip context for the duration of the inner action so every
-- log line emitted inside the span carries @trace_id@ as a structured
-- field. Exceptions raised inside the inner action are recorded on the
-- span before being rethrown. 'AddSpanAttribute' and 'RecordSpanException'
-- act on whichever span is currently active in OT's thread-local context.
runTraceOTel ::
  (IOE :> es, KatipE :> es) =>
  Tracer ->
  Eff (Trace : es) a ->
  Eff es a
runTraceOTel tracer = interpret $ \env -> \case
  InSpan name action ->
    localSeqUnlift env $ \unlift ->
      bracket (acquire name) release (use unlift action)
  AddSpanAttribute k v -> liftIO $ do
    mSpan <- lookupSpan <$> getContext
    for_ mSpan $ addAttribute' k v
  RecordSpanException e -> liftIO $ do
    mSpan <- lookupSpan <$> getContext
    for_ mSpan $ recordException' e
 where
  addAttribute' k v sp = addAttribute sp k v
  recordException' e sp = recordException sp mempty Nothing e
  -- Create the span, push it onto the thread-local context, and remember
  -- the parent span (if any) so we can restore it on release.
  acquire name = liftIO $ do
    ctx <- getContext
    sp <- createSpanWithoutCallStack tracer ctx name defaultSpanArguments
    _ <- attachContext (insertSpan sp ctx)
    pure (lookupSpan ctx, sp)
  -- End the span and restore the parent span (or none) as the active one.
  release (mParent, sp) = liftIO $ do
    endSpan sp Nothing
    _ <- detachContext
    for_ mParent $ \parentSp -> do
      ctx <- getContext
      attachContext (insertSpan parentSp ctx)
  -- Run the unlifted inner action under a Katip context carrying trace_id.
  -- Catch exceptions to mark the span as errored before rethrowing so the
  -- bracket's release handler runs and the span ends with the right status.
  use unlift action (_, sp) = do
    spanCtx <- liftIO $ OT.getSpanContext sp
    let tid = traceIdBaseEncodedText Base16 (traceId spanCtx)
    katipAddContext (sl "trace_id" tid) (unlift action)
      `catch` \(ex :: SomeException) -> do
        liftIO $ do
          setStatus sp (Error (T.pack (displayException ex)))
          recordException sp mempty Nothing ex
        throwIO ex

-- | Read the trace ID of the span currently active in OpenTelemetry's
-- thread-local context (e.g. one set by the WAI middleware) and attach
-- it to the Katip context for the duration of the inner action. No-op
-- if no span is active. Useful for bridging trace IDs into Katip logs
-- at points in the call stack that precede our own 'inSpan' calls
-- (e.g. the 'guardExceptions' catch happens outside any effect-level
-- span but inside the WAI middleware's server span).
setKatipTraceId ::
  (IOE :> es, KatipE :> es) => Eff es a -> Eff es a
setKatipTraceId action = do
  mTid <- liftIO $ do
    mSpan <- lookupSpan <$> getContext
    case mSpan of
      Nothing -> pure Nothing
      Just sp -> do
        spanCtx <- OT.getSpanContext sp
        pure $ Just (traceIdBaseEncodedText Base16 (traceId spanCtx))
  case mTid of
    Nothing -> action
    Just tid -> katipAddContext (sl "trace_id" tid) action
