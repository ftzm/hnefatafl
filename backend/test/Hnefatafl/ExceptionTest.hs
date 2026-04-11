{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Hnefatafl.ExceptionTest where

import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Exception (throwIO)
import Effectful.Katip (runKatipE)
import Hnefatafl.Exception (
  DataIntegrityException (..),
  DatabaseException (..),
  DomainException (..),
  GameInvariantException (..),
  IsDomainException (..),
  guardExceptions,
 )
import Hnefatafl.Logging (withNoLogEnv)
import Servant (ServerError (..))
import System.IO.Error (userError)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty

-------------------------------------------------------------------------------
-- Exception hierarchy tests

spec_ExceptionHierarchy :: Spec
spec_ExceptionHierarchy =
  describe "Exception hierarchy" $ do
    describe "DomainException routing" $ do
      it "DataIntegrityException can be caught as DomainException" $ do
        let ex = toException $ EntityNotFound{entity = "Player", entityId = "123"}
        fromException @DomainException ex `shouldSatisfy` isJust

      it "GameInvariantException can be caught as DomainException" $ do
        let ex = toException $ InvariantViolation "test"
        fromException @DomainException ex `shouldSatisfy` isJust

      it "DatabaseException can be caught as DomainException" $ do
        let ex =
              toException $
                DatabaseException "GetGame" "Game" (Just "abc") (toException $ userError "boom")
        fromException @DomainException ex `shouldSatisfy` isJust

      it "DataIntegrityException is not caught as GameInvariantException" $ do
        let ex = toException $ EntityNotFound{entity = "Player", entityId = "123"}
        fromException @GameInvariantException ex `shouldSatisfy` isNothing

      it "DatabaseException is not caught as DataIntegrityException" $ do
        let ex =
              toException $
                DatabaseException "GetGame" "Game" Nothing (toException $ userError "boom")
        fromException @DataIntegrityException ex `shouldSatisfy` isNothing

    describe "domainErrorLabel" $ do
      it "MissingRequiredField" $
        domainErrorLabel MissingRequiredField{entity = "x", field = "y", entityId = "z"}
          `shouldBe` "missing_required_field"
      it "EntityNotFound" $
        domainErrorLabel EntityNotFound{entity = "x", entityId = "z"}
          `shouldBe` "entity_not_found"
      it "EngineReplayFailed" $
        domainErrorLabel EngineReplayFailed{context = "x", detail = "y"}
          `shouldBe` "engine_replay_failed"
      it "InvariantViolation" $
        domainErrorLabel (InvariantViolation "x") `shouldBe` "invariant_violation"
      it "DatabaseException" $
        domainErrorLabel
          (DatabaseException "op" "ent" Nothing (toException $ userError ""))
          `shouldBe` "database_error"

    describe "displayException" $ do
      it "MissingRequiredField includes entity, field, and id" $ do
        let msg =
              displayException
                MissingRequiredField
                  { entity = "ai_game"
                  , field = "player_color"
                  , entityId = "42"
                  }
        msg `shouldContain` "player_color"
        msg `shouldContain` "ai_game"
        msg `shouldContain` "42"

      it "DatabaseException includes operation, entity, id, and cause" $ do
        let msg =
              displayException $
                DatabaseException
                  "GetGame"
                  "Game"
                  (Just "abc")
                  (toException $ userError "row not found")
        msg `shouldContain` "GetGame"
        msg `shouldContain` "Game"
        msg `shouldContain` "abc"
        msg `shouldContain` "row not found"

-------------------------------------------------------------------------------
-- Guard tests

spec_GuardExceptions :: Spec
spec_GuardExceptions =
  describe "guardExceptions" $ do
    it "passes through successful actions" $ do
      result <- withNoLogEnv "test" $ \logEnv ->
        runEff $
          runErrorNoCallStack @ServerError $
            runKatipE logEnv $
              guardExceptions (pure (42 :: Int))
      result `shouldBe` Right 42

    it "catches DomainException and returns 500" $ do
      result <- withNoLogEnv "test" $ \logEnv ->
        runEff $
          runErrorNoCallStack @ServerError $
            runKatipE logEnv $
              guardExceptions $
                throwIO $
                  EntityNotFound{entity = "Game", entityId = "abc"}
      case result of
        Left err -> errHTTPCode err `shouldBe` 500
        Right _ -> expectationFailure "Expected ServerError 500"

    it "catches DatabaseException and returns 500" $ do
      result <- withNoLogEnv "test" $ \logEnv ->
        runEff $
          runErrorNoCallStack @ServerError $
            runKatipE logEnv $
              guardExceptions $
                throwIO $
                  DatabaseException
                    "GetGame"
                    "Game"
                    (Just "xyz")
                    (toException $ userError "SQL error")
      case result of
        Left err -> errHTTPCode err `shouldBe` 500
        Right _ -> expectationFailure "Expected ServerError 500"

    it "catches non-domain exceptions and returns 500" $ do
      result <- withNoLogEnv "test" $ \logEnv ->
        runEff $
          runErrorNoCallStack @ServerError $
            runKatipE logEnv $
              guardExceptions $
                throwIO $
                  userError "something went wrong"
      case result of
        Left err -> errHTTPCode err `shouldBe` 500
        Right _ -> expectationFailure "Expected ServerError 500"
