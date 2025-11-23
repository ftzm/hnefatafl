{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.TokenTest where

import Data.List (isInfixOf)
import Data.Time (getCurrentTime)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Effect.Storage
import Optics
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Test.Hspec (Spec, around, describe, it)
import Test.Hspec.Expectations.Pretty

spec_Token :: Spec
spec_Token =
  around withSharedDB $ do
    describe "createGameParticipantToken" $ do
      it "can create a token for a game" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            testToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "test-token-1"
                & #token
                .~ "abcd1234"

        shouldSucceed
          ( do
              insertGame testGame
              createGameParticipantToken testToken
          )
          conn

      it "can create tokens for both roles" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            whiteToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "white-token-1"
                & #token
                .~ "white-abc123"
            blackToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "black-token-1"
                & #token
                .~ "black-xyz789"
                & #role
                .~ Black

        shouldSucceed
          ( do
              insertGame testGame
              createGameParticipantToken whiteToken
              createGameParticipantToken blackToken
          )
          conn

    describe "getTokenByText" $ do
      it "can retrieve a token by its text value" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            testToken =
              baseToken
                & #token
                .~ "retrieve-me-123"

        resultEquals
          ( do
              insertGame testGame
              createGameParticipantToken testToken
              getTokenByText "retrieve-me-123"
          )
          (Just testToken)
          conn

      it "returns Nothing for non-existent token" $ \conn ->
        resultEquals
          (getTokenByText "does-not-exist")
          (Nothing :: Maybe GameParticipantToken)
          conn

      it "can distinguish between different token texts" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            token1 =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "token-1"
                & #token
                .~ "first-token"
            token2 =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "token-2"
                & #token
                .~ "second-token"
                & #role
                .~ Black

        resultEquals
          ( do
              insertGame testGame
              createGameParticipantToken token1
              createGameParticipantToken token2
              getTokenByText "second-token"
          )
          (Just token2)
          conn

    describe "getActiveTokenByGameAndRole" $ do
      it "can retrieve active token by game and role" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            whiteToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "active-white-token"
                & #token
                .~ "active-white-123"
            blackToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "active-black-token"
                & #token
                .~ "active-black-456"
                & #role
                .~ Black

        resultEquals
          ( do
              insertGame testGame
              createGameParticipantToken whiteToken
              createGameParticipantToken blackToken
              getActiveTokenByGameAndRole testGame.gameId White
          )
          (Just whiteToken)
          conn

      it "returns Nothing when no active token exists for role" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now

        resultEquals
          ( do
              insertGame testGame
              getActiveTokenByGameAndRole testGame.gameId White
          )
          (Nothing :: Maybe GameParticipantToken)
          conn

    describe "unique constraint on active tokens" $ do
      it "prevents multiple active tokens for same game and role" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            firstToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "first-white-token"
                & #token
                .~ "first-white-token-text"
            duplicateToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "second-white-token"
                & #token
                .~ "second-white-token-text"

        result <- runStorageTest conn $ do
          insertGame testGame
          createGameParticipantToken firstToken
          createGameParticipantToken duplicateToken -- This should fail
        result `shouldSatisfy` \case
          Left err -> "UNIQUE constraint failed" `isInfixOf` err
          Right _ -> False

      it "allows multiple active tokens for different roles in same game" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            whiteToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "unique-white-token"
                & #token
                .~ "white-token-unique"
            blackToken =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "unique-black-token"
                & #token
                .~ "black-token-unique"
                & #role
                .~ Black

        shouldSucceed
          ( do
              insertGame testGame
              createGameParticipantToken whiteToken
              createGameParticipantToken blackToken
          )
          conn

      it "allows same role tokens for different games" $ \conn -> do
        now <- getCurrentTime
        let game1 =
              baseGame now
                & #gameId
                .~ GameId "multi-game-1"
                & #name
                ?~ "Multi Game 1"
            game2 =
              baseGame now
                & #gameId
                .~ GameId "multi-game-2"
                & #name
                ?~ "Multi Game 2"
            token1 =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "game1-white-token"
                & #gameId
                .~ game1.gameId
                & #token
                .~ "game1-white"
            token2 =
              baseToken
                & #tokenId
                .~ GameParticipantTokenId "game2-white-token"
                & #gameId
                .~ game2.gameId
                & #token
                .~ "game2-white"

        shouldSucceed
          ( do
              insertGame game1
              insertGame game2
              createGameParticipantToken token1
              createGameParticipantToken token2
          )
          conn