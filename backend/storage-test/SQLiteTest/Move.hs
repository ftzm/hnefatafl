{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SQLiteTest.Move where

import Data.Time (getCurrentTime)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Storage.Effect
import Optics
import SQLiteTest.Util
import Test.Hspec (Spec, around, describe, it)

spec_SQLite_Move :: Spec
spec_SQLite_Move =
  around withSharedDB $ do
    describe "insertMove" $ do
      it "can insert a move without error" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            testMove = baseMove now

        shouldSucceed
          ( do
              insertGame testGame
              insertMove testGame.gameId testMove
          )
          conn

      it "can insert multiple moves for the same game" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            moves = generateMoves now [Move 0 1, Move 9 10]

        shouldSucceed
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves
          )
          conn

    describe "getMove" $ it "can retrieve a move that was inserted" $ \conn -> do
      now <- getCurrentTime
      let testGame = baseGame now
          testMove =
            baseMove now
              & #move
              .~ Move 2 3
              & #boardStateAfter
              .~ ExternBoard{black = Layer 0 0, white = Layer 0 0, king = 27}

      resultEquals
        ( do
            insertGame testGame
            insertMove testGame.gameId testMove
            getMove testMove.moveId
        )
        testMove
        conn

    describe "getMovesForGame" $ do
      it "can retrieve all moves for a game in correct order" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            expectedMoves = generateMoves now [Move 0 1, Move 12 13, Move 24 25]

        resultEquals
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) expectedMoves -- Insert in order (sequential constraint)
              getMovesForGame testGame.gameId
          )
          expectedMoves
          conn

      it "returns empty list for game with no moves" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now

        resultEquals
          ( do
              insertGame testGame
              getMovesForGame testGame.gameId
          )
          ([] :: [GameMove])
          conn

    describe "getLatestMoveForGame" $ do
      it "returns the latest move by move number" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            moves = generateMoves now [Move 0 1, Move 12 13]
            expectedLatest = fromMaybe (error "empty moves") (viaNonEmpty last moves)

        resultEquals
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves
              getLatestMoveForGame testGame.gameId
          )
          (Just expectedLatest)
          conn

      it "returns Nothing for game with no moves" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now

        resultEquals
          ( do
              insertGame testGame
              getLatestMoveForGame testGame.gameId
          )
          (Nothing :: Maybe GameMove)
          conn

    describe "getMoveCountForGame" $ do
      it "returns correct count for game with moves" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            moves = generateMoves now [Move 0 1, Move 12 13, Move 24 25]

        resultEquals
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves
              getMoveCountForGame testGame.gameId
          )
          3
          conn

      it "returns 0 for game with no moves" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now

        resultEquals
          ( do
              insertGame testGame
              getMoveCountForGame testGame.gameId
          )
          0
          conn

    describe "deleteMove" $ do
      it "can delete a move and verify deletion" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            testMove = baseMove now

        shouldSucceed
          ( do
              insertGame testGame
              insertMove testGame.gameId testMove
              deleteMove testMove.moveId
          )
          conn

        -- Verify move no longer exists
        shouldFail (getMove testMove.moveId) conn

      it "deleting a non-existent move should succeed (no-op)" $ \conn -> do
        let nonExistentId = MoveId "does-not-exist"

        shouldSucceed (deleteMove nonExistentId) conn

      it "can delete moves and track game state correctly" $ \conn -> do
        now <- getCurrentTime
        let testGame = baseGame now
            moves = generateMoves now [Move 0 1, Move 12 13]
            [_move1, move2] = moves

        resultEquals
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves

              -- Check that we have 2 moves
              moveCount1 <- getMoveCountForGame testGame.gameId

              -- Delete the last move
              deleteMove move2.moveId

              -- Check that we now have 1 move
              moveCount2 <- getMoveCountForGame testGame.gameId

              pure (moveCount1 == 2 && moveCount2 == 1)
          )
          True
          conn

    describe "move integration tests" $ it "can perform a complete move workflow" $ \conn -> do
      now <- getCurrentTime
      let whitePlayer =
            baseHumanPlayer
              & #playerId
              .~ PlayerId "workflow-white"
              & #name
              .~ "White Player"
              & #email
              .~ Nothing
          blackPlayer =
            baseHumanPlayer
              & #playerId
              .~ PlayerId "workflow-black"
              & #name
              .~ "Black Player"
              & #email
              .~ Nothing
          testGame =
            baseGame now
              & #whitePlayerId
              ?~ whitePlayer.playerId
              & #blackPlayerId
              ?~ blackPlayer.playerId
          moves = generateMoves now [Move 0 1, Move 12 13, Move 24 25]

      resultEquals
        ( do
            -- Setup players and game
            insertHumanPlayer whitePlayer
            insertHumanPlayer blackPlayer
            insertGame testGame

            -- Add moves
            mapM_ (insertMove testGame.gameId) moves

            -- Test various queries
            allMoves <- getMovesForGame testGame.gameId
            latestMove <- getLatestMoveForGame testGame.gameId
            moveCount <- getMoveCountForGame testGame.gameId

            -- Verify results
            pure
              (allMoves == moves && latestMove == viaNonEmpty last moves && moveCount == 3)
        )
        True
        conn
