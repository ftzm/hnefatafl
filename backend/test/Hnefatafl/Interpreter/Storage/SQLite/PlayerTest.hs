{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.PlayerTest where

import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Effect.Storage
import Optics
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Test.Hspec (Spec, around, describe, it)

spec_Player :: Spec
spec_Player =
  around withSharedDB $ do
    describe "insertHumanPlayer" $ do
      it "can insert a human player without error" $ \conn ->
        shouldSucceed (insertHumanPlayer baseHumanPlayer) conn

      it "can insert multiple human players in sequence" $ \conn -> do
        let testPlayer1 = baseHumanPlayer & #playerId .~ PlayerId "player-1" & #name .~ "Test Human 1"
            testPlayer2 = baseHumanPlayer & #playerId .~ PlayerId "player-2" & #name .~ "Test Human 2" & #email .~ Nothing

        shouldSucceed
          ( do
              insertHumanPlayer testPlayer1
              insertHumanPlayer testPlayer2
          )
          conn

      it
        "verifies data isolation between tests (this test should not see previous test data)"
        $ \conn -> do
          -- This test should not see any players from previous tests due to rollback
          shouldSucceed (insertHumanPlayer baseHumanPlayer) conn

    describe "getHumanPlayer" $ do
      it "can retrieve a human player that was inserted" $ \conn ->
        resultEquals
          ( do
              insertHumanPlayer baseHumanPlayer
              getHumanPlayer baseHumanPlayer.playerId
          )
          baseHumanPlayer
          conn

      it "can retrieve multiple different players" $ \conn -> do
        let player1 = baseHumanPlayer & #playerId .~ PlayerId "multi-1" & #name .~ "Multi Test 1"
            player2 = baseHumanPlayer & #playerId .~ PlayerId "multi-2" & #name .~ "Multi Test 2" & #email .~ Nothing

        resultEquals
          ( do
              insertHumanPlayer player1
              insertHumanPlayer player2
              -- Retrieve player2 to verify we get the right one
              getHumanPlayer player2.playerId
          )
          player2
          conn

      it "can insert and retrieve an engine player" $ \conn ->
        resultEquals
          ( do
              insertEnginePlayer baseEnginePlayer
              getEnginePlayer baseEnginePlayer.playerId
          )
          baseEnginePlayer
          conn

    describe "getPlayer" $ do
      it "can retrieve a human player as Player type" $ \conn ->
        resultEquals
          ( do
              insertHumanPlayer baseHumanPlayer
              getPlayer baseHumanPlayer.playerId
          )
          (HumanPlayerTag baseHumanPlayer)
          conn

      it "can retrieve an engine player as Player type" $ \conn ->
        resultEquals
          ( do
              insertEnginePlayer baseEnginePlayer
              getPlayer baseEnginePlayer.playerId
          )
          (EnginePlayerTag baseEnginePlayer)
          conn

    describe "deletePlayer" $ do
      it "can delete a human player and cascade to human_player table" $ \conn -> do
        let testHumanPlayer =
              HumanPlayer
                { playerId = PlayerId "delete-human-1"
                , name = "To Be Deleted"
                , email = Just "delete@example.com"
                }

        shouldSucceed
          ( do
              insertHumanPlayer testHumanPlayer
              deletePlayer testHumanPlayer.playerId
          )
          conn

        -- Verify player no longer exists by attempting to retrieve it
        shouldFail (getHumanPlayer testHumanPlayer.playerId) conn

      it "can delete an engine player and cascade to engine_player table" $ \conn -> do
        let testEnginePlayer =
              EnginePlayer
                { playerId = PlayerId "delete-engine-1"
                , version = "v1.5.0"
                }

        shouldSucceed
          ( do
              insertEnginePlayer testEnginePlayer
              deletePlayer testEnginePlayer.playerId
          )
          conn

        -- Verify player no longer exists by attempting to retrieve it
        shouldFail (getEnginePlayer testEnginePlayer.playerId) conn

      it "can delete a player using getPlayer and verify deletion" $ \conn -> do
        let testHumanPlayer =
              HumanPlayer
                { playerId = PlayerId "delete-getplayer-1"
                , name = "Delete via GetPlayer"
                , email = Nothing
                }

        shouldSucceed
          ( do
              insertHumanPlayer testHumanPlayer
              deletePlayer testHumanPlayer.playerId
          )
          conn

        -- Verify deletion using getPlayer
        shouldFail (getPlayer testHumanPlayer.playerId) conn

      it "deleting a non-existent player should succeed (no-op)" $ \conn -> do
        let nonExistentId = PlayerId "does-not-exist"

        shouldSucceed (deletePlayer nonExistentId) conn

    describe "humanPlayerFromName" $ do
      it "returns Nothing when no player with the given name exists" $ \conn ->
        resultEquals
          (humanPlayerFromName "NonexistentPlayer")
          Nothing
          conn

      it "returns the human player when a player with the given name exists" $ \conn -> do
        let testPlayer =
              baseHumanPlayer & #playerId .~ PlayerId "name-lookup-1" & #name .~ "UniqueTestName"

        resultEquals
          ( do
              insertHumanPlayer testPlayer
              humanPlayerFromName "UniqueTestName"
          )
          (Just testPlayer)
          conn

      it "returns the correct player when multiple players exist with different names" $ \conn -> do
        let player1 = baseHumanPlayer & #playerId .~ PlayerId "name-lookup-2" & #name .~ "Alice"
            player2 = baseHumanPlayer & #playerId .~ PlayerId "name-lookup-3" & #name .~ "Bob"
            player3 = baseHumanPlayer & #playerId .~ PlayerId "name-lookup-4" & #name .~ "Charlie"

        resultEquals
          ( do
              insertHumanPlayer player1
              insertHumanPlayer player2
              insertHumanPlayer player3
              humanPlayerFromName "Bob"
          )
          (Just player2)
          conn