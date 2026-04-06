{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hnefatafl.Exception (
  -- * Exception hierarchy
  DomainException (..),
  IsDomainException (..),

  -- * Concrete exception types
  StorageException (..),
  DatabaseException (..),
  GameInvariantException (..),

  -- * Exception guards
  guardExceptions,
) where

import Control.Exception (SomeAsyncException (..), someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Data.Typeable (cast)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catch, throwIO)
import Servant (ServerError, err500, errBody)
import System.IO (hPutStrLn)
import Text.Show (Show (showsPrec), showString)

-- | Existential wrapper — the "base class" for all domain exceptions.
-- catch @DomainException catches any domain exception.
data DomainException = forall e. (IsDomainException e) => DomainException e

-- | Shared interface for all domain exceptions.
class (Exception e, Typeable e) => IsDomainException e where
  domainErrorLabel :: e -> Text
  domainContext :: e -> [(Text, Text)]

instance Show DomainException where
  showsPrec p (DomainException e) = showsPrec p e

instance Exception DomainException where
  displayException (DomainException e) = displayException e

-------------------------------------------------------------------------------
-- Storage data exceptions

-- | Data integrity violations detected in application code.
data StorageException
  = MissingRequiredField {entity :: Text, field :: Text, entityId :: Text}
  | EntityNotFound {entity :: Text, entityId :: Text}
  deriving (Show)

instance Exception StorageException where
  toException = toException . DomainException
  fromException se = do
    DomainException e <- fromException se
    cast e
  displayException = \case
    MissingRequiredField{..} ->
      "Missing required field " <> toString field
        <> " on " <> toString entity
        <> " (id: " <> toString entityId <> ")"
    EntityNotFound{..} ->
      toString entity <> " not found (id: " <> toString entityId <> ")"

instance IsDomainException StorageException where
  domainErrorLabel = \case
    MissingRequiredField{} -> "missing_required_field"
    EntityNotFound{} -> "entity_not_found"
  domainContext = \case
    MissingRequiredField{..} -> [("entity", entity), ("field", field), ("id", entityId)]
    EntityNotFound{..} -> [("entity", entity), ("id", entityId)]

-------------------------------------------------------------------------------
-- Database exceptions

-- | Wraps an underlying database exception (SQLError, IOException, etc.)
-- with the storage operation that was running when it failed.
data DatabaseException = DatabaseException
  { operation :: Text
  , dbEntity :: Text
  , dbEntityId :: Maybe Text
  , cause :: SomeException
  }

instance Show DatabaseException where
  showsPrec _ DatabaseException{..} =
    showString "DatabaseException {operation = "
      . showString (toString operation)
      . showString ", entity = "
      . showString (toString dbEntity)
      . showString ", entityId = "
      . showsPrec 0 dbEntityId
      . showString ", cause = "
      . showsPrec 0 cause
      . showString "}"

instance Exception DatabaseException where
  toException = toException . DomainException
  fromException se = do
    DomainException e <- fromException se
    cast e
  displayException DatabaseException{..} =
    toString operation <> " on " <> toString dbEntity
      <> maybe "" (\i -> " (id: " <> toString i <> ")") dbEntityId
      <> ": " <> displayException cause

instance IsDomainException DatabaseException where
  domainErrorLabel _ = "database_error"
  domainContext DatabaseException{..} =
    [("operation", operation), ("entity", dbEntity)]
      <> maybe [] (\i -> [("id", i)]) dbEntityId
      <> [("cause", toText (displayException cause))]

-------------------------------------------------------------------------------
-- Game invariant exceptions

-- | Game logic invariant violations.
data GameInvariantException
  = EngineReplayFailed {context :: Text, detail :: Text}
  | InvariantViolation Text
  deriving (Show)

instance Exception GameInvariantException where
  toException = toException . DomainException
  fromException se = do
    DomainException e <- fromException se
    cast e
  displayException = \case
    EngineReplayFailed{..} ->
      "Engine replay failed in " <> toString context <> ": " <> toString detail
    InvariantViolation msg ->
      "Invariant violation: " <> toString msg

instance IsDomainException GameInvariantException where
  domainErrorLabel = \case
    EngineReplayFailed{} -> "engine_replay_failed"
    InvariantViolation{} -> "invariant_violation"
  domainContext = \case
    EngineReplayFailed{..} -> [("context", context), ("detail", detail)]
    InvariantViolation msg -> [("message", msg)]

-------------------------------------------------------------------------------
-- Exception guards

-- | Catch-all exception guard for Servant handlers. Catches all synchronous
-- exceptions, logs structured data for domain exceptions, and returns 500.
-- Async exceptions are always re-thrown.
guardExceptions ::
  (IOE :> es, Error ServerError :> es) =>
  Eff es a -> Eff es a
guardExceptions action =
  action `catch` \(ex :: SomeException) ->
    case fromException @SomeAsyncException ex of
      Just _ -> throwIO ex
      Nothing -> do
        let ctx = someExceptionContext ex
        case fromException @DomainException ex of
          Just (DomainException e) ->
            liftIO $
              hPutStrLn stderr $
                "Domain exception [" <> toString (domainErrorLabel e) <> "]: "
                  <> displayException e
                  <> "\n"
                  <> toString (show (domainContext e) :: Text)
                  <> "\n"
                  <> displayExceptionContext ctx
          Nothing ->
            liftIO $
              hPutStrLn stderr $
                "Unhandled exception: " <> displayException ex
                  <> "\n"
                  <> displayExceptionContext ctx
        throwError err500{errBody = "internal server error"}
