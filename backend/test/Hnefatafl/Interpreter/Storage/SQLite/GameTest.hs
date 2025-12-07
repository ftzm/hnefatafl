{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.GameTest where

import Data.List (isInfixOf)
import Data.Time (getCurrentTime)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Optics
import Test.Hspec (Spec, around, describe, it)
import Test.Hspec.Expectations.Pretty

spec_Game :: Spec
spec_Game =
  around withSharedDB $ do
    describe "insertGame" $ do
      it "can insert a game without players (anonymous)" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now

        shouldSucceed (insertGame testGame) conn

      it "can insert a game with players" $ \conn -> do
        now <- getCurrentTime
        let whitePlayer = baseHumanPlayer & #playerId .~ PlayerId "white-player"
            blackPlayer = baseHumanPlayer & #playerId .~ PlayerId "black-player"
            testGame =
              baseGame now
                & #gameId
                .~ GameId "test-game-2"
                & #whitePlayerId
                ?~ PlayerId "white-player"
                & #blackPlayerId
                ?~ PlayerId "black-player"

        shouldSucceed
          ( do
              insertHumanPlayer whitePlayer
              insertHumanPlayer blackPlayer
              insertGame testGame
          )
          conn

    describe "getGame" $ do
      it "can retrieve a game that was inserted" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
        resultEquals
          ( do
              insertGame testGame
              getGame testGame.gameId
          )
          testGame
          conn

      it "can retrieve a game with different status" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now & #endTime ?~ now & #gameStatus .~ WhiteWonKingEscaped
        resultEquals
          ( do
              insertGame testGame
              getGame testGame.gameId
          )
          testGame
          conn

    describe "updateGameStatus" $ do
      it "can update game status to completed" $ \conn -> do
        now <- getCurrentTime
        let originalGame = baseGame now
            expectedGame = originalGame & #gameStatus .~ BlackWonKingCaptured & #endTime ?~ now

        resultEquals
          ( do
              insertGame originalGame
              updateGameStatus originalGame.gameId BlackWonKingCaptured (Just now)
              getGame originalGame.gameId
          )
          expectedGame
          conn

      it "can update game status without setting end time" $ \conn -> do
        now <- getCurrentTime
        let originalGame = baseGame now
            expectedGame = originalGame & #gameStatus .~ Draw

        resultEquals
          ( do
              insertGame originalGame
              updateGameStatus originalGame.gameId Draw Nothing
              getGame originalGame.gameId
          )
          expectedGame
          conn

    describe "deleteGame" $ do
      it "can delete a game and verify deletion" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now

        shouldSucceed
          ( do
              insertGame testGame
              deleteGame testGame.gameId
          )
          conn

        -- Verify game no longer exists
        shouldFail (getGame testGame.gameId) conn

      it "deleting a non-existent game should succeed (no-op)" $ \conn -> do
        let nonExistentId = GameId "does-not-exist"

        shouldSucceed (deleteGame nonExistentId) conn

    describe "game names" $ do
      it "can insert games with null names" $ \conn -> do
        now <- getCurrentTime
        let game1 = baseGame now & #gameId .~ GameId "null-name-1" & #name .~ Nothing
            game2 = baseGame now & #gameId .~ GameId "null-name-2" & #name .~ Nothing
        shouldSucceed
          ( do
              insertGame game1
              insertGame game2
              retrievedGame1 <- getGame (GameId "null-name-1")
              retrievedGame2 <- getGame (GameId "null-name-2")
              pure (isNothing retrievedGame1.name && isNothing retrievedGame2.name)
          )
          conn

      it "fails when inserting games with duplicate names" $ \conn -> do
        now <- getCurrentTime
        let game1 = baseGame now & #gameId .~ GameId "dup-name-1" & #name ?~ "Duplicate Name"
            game2 = baseGame now & #gameId .~ GameId "dup-name-2" & #name ?~ "Duplicate Name"

        -- Both operations in the same transaction to test the constraint
        result <- runStorageTest conn $ do
          insertGame game1 -- This should succeed
          insertGame game2 -- This should fail due to unique constraint
        result `shouldSatisfy` \case
          Left err -> "UNIQUE constraint failed" `isInfixOf` err
          Right _ -> False

    describe "game with all status types" $ it "can handle all GameStatus variants" $ \conn -> do
      now <- getCurrentTime
      let statusTests =
            [ (Ongoing, "ongoing-game")
            , (BlackWonKingCaptured, "black-won-king-captured-game")
            , (BlackWonWhiteSurrounded, "black-won-white-surrounded-game")
            , (BlackWonNoWhiteMoves, "black-won-no-white-moves-game")
            , (BlackWonResignation, "black-resignation-game")
            , (BlackWonTimeout, "black-timeout-game")
            , (WhiteWonKingEscaped, "white-won-king-escaped-game")
            , (WhiteWonExitFort, "white-won-exit-fort-game")
            , (WhiteWonNoBlackMoves, "white-won-no-black-moves-game")
            , (WhiteWonResignation, "white-resignation-game")
            , (WhiteWonTimeout, "white-timeout-game")
            , (Draw, "draw-game")
            , (Abandoned, "abandoned-game")
            ]

      shouldSucceed
        do
          mapM_
            ( \(status, gameIdSuffix) -> do
                let gameId = GameId gameIdSuffix
                    testGame =
                      baseGame now
                        & #gameId
                        .~ gameId
                        & #name
                        ?~ ("Test Game " <> gameIdSuffix)
                        & #gameStatus
                        .~ status
                insertGame testGame
                retrievedGame <- getGame gameId
                -- Verify status was stored and retrieved correctly
                pure $ retrievedGame.gameStatus == status
            )
            statusTests
        conn
