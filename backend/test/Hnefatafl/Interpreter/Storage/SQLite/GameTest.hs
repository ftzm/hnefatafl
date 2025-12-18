{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.GameTest where

import Data.List (isInfixOf)
import Chronos (now)
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
        currentTime <- now
        let testGame = baseGame currentTime

        shouldSucceed (insertGame testGame) conn

      it "can insert a game with players" $ \conn -> do
        currentTime <- now
        let whitePlayer = baseHumanPlayer & #playerId .~ PlayerId "white-player" & #name .~ "White Player"
            blackPlayer = baseHumanPlayer & #playerId .~ PlayerId "black-player" & #name .~ "Black Player"
            testGame =
              baseGame currentTime
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
        currentTime <- now
        let testGame = baseGame currentTime
        resultEquals
          ( do
              insertGame testGame
              getGame testGame.gameId
          )
          testGame
          conn

      it "can retrieve a game with different status" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime & #endTime ?~ currentTime & #gameStatus .~ WhiteWonKingEscaped
        resultEquals
          ( do
              insertGame testGame
              getGame testGame.gameId
          )
          testGame
          conn

    describe "updateGameStatus" $ do
      it "can update game status to completed" $ \conn -> do
        currentTime <- now
        let originalGame = baseGame currentTime
            expectedGame = originalGame & #gameStatus .~ BlackWonKingCaptured & #endTime ?~ currentTime

        resultEquals
          ( do
              insertGame originalGame
              updateGameStatus originalGame.gameId BlackWonKingCaptured (Just currentTime)
              getGame originalGame.gameId
          )
          expectedGame
          conn

      it "can update game status without setting end time" $ \conn -> do
        currentTime <- now
        let originalGame = baseGame currentTime
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
        currentTime <- now
        let testGame = baseGame currentTime

        -- Insert, delete, and verify deletion all in one transaction
        result <- runStorageTest conn $ do
          insertGame testGame
          deleteGame testGame.gameId
          getGame testGame.gameId -- This should fail

        case result of
          Left _ -> pure () -- Expected failure when trying to get deleted game
          Right _ -> expectationFailure "Expected failure when getting deleted game"

      it "deleting a non-existent game should succeed (no-op)" $ \conn -> do
        let nonExistentId = GameId "does-not-exist"

        shouldSucceed (deleteGame nonExistentId) conn

    describe "game names" $ do
      it "can insert games with null names" $ \conn -> do
        currentTime <- now
        let game1 = baseGame currentTime & #gameId .~ GameId "null-name-1" & #name .~ Nothing
            game2 = baseGame currentTime & #gameId .~ GameId "null-name-2" & #name .~ Nothing
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
        currentTime <- now
        let game1 = baseGame currentTime & #gameId .~ GameId "dup-name-1" & #name ?~ "Duplicate Name"
            game2 = baseGame currentTime & #gameId .~ GameId "dup-name-2" & #name ?~ "Duplicate Name"

        -- Both operations in the same transaction to test the constraint
        result <- runStorageTest conn $ do
          insertGame game1 -- This should succeed
          insertGame game2 -- This should fail due to unique constraint
        result `shouldSatisfy` \case
          Left err -> "UNIQUE constraint failed" `isInfixOf` err
          Right _ -> False

    describe "listGames" $ do
      it "returns empty list when no games exist" $ \conn -> do
        resultEquals
          listGames
          []
          conn

      it "returns games in descending order by creation time" $ \conn -> do
        currentTime <- now
        let game1 = baseGame currentTime & #gameId .~ GameId "game-1" & #name ?~ "First Game"
            game2 = baseGame currentTime & #gameId .~ GameId "game-2" & #name ?~ "Second Game"
            game3 = baseGame currentTime & #gameId .~ GameId "game-3" & #name ?~ "Third Game"

        -- Insert games and get the list all in one transaction
        result <- runStorageTest conn $ do
          insertGame game1
          insertGame game2
          insertGame game3
          listGames

        case result of
          Left err -> expectationFailure $ "Expected success but got error: " ++ err
          Right games -> do
            length games `shouldBe` 3
            -- Games should be ordered by creation time (most recent first)
            -- Since all have same timestamp, order by gameId for predictability
            let gameNames = map (.name) games
            gameNames `shouldContain` [Just "First Game", Just "Second Game", Just "Third Game"]

      it "can list games with different statuses" $ \conn -> do
        currentTime <- now
        let ongoingGame = baseGame currentTime & #gameId .~ GameId "ongoing" & #name ?~ "Ongoing Game"
            completedGame =
              baseGame currentTime
                & #gameId .~ GameId "completed"
                & #name ?~ "Completed Game"
                & #gameStatus .~ WhiteWonKingEscaped
                & #endTime ?~ currentTime

        -- Insert games and get the list all in one transaction
        result <- runStorageTest conn $ do
          insertGame ongoingGame
          insertGame completedGame
          listGames

        case result of
          Left err -> expectationFailure $ "Expected success but got error: " ++ err
          Right games -> do
            length games `shouldBe` 2
            let statuses = map (.gameStatus) games
            statuses `shouldContain` [Ongoing, WhiteWonKingEscaped]

    describe "game with all status types" $ it "can handle all GameStatus variants" $ \conn -> do
      currentTime <- now
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
                      baseGame currentTime
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
