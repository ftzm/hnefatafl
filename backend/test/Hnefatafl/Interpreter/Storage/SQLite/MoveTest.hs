{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hnefatafl.Interpreter.Storage.SQLite.MoveTest where

import Chronos (now, Time)
import Hnefatafl.Core.Data (MoveResult (..))
import Hnefatafl.Core.Data as CoreData hiding (MoveResult (..))
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite.Util (shouldBeTrue, shouldFail, resultEquals, shouldSucceed, withSharedDB, generateMoves, baseGame, baseMove)
import Optics
import Test.Hspec (Spec, around, describe, it)
import TestUtil (realMoveResults)


-- | Create GameMoves from real move results with actual board states
-- Takes count and timestamp
makeRealGameMoves :: Int -> Time -> [GameMove]
makeRealGameMoves count timestamp =
  take count $ map
    (\(MoveResult move board _ wasBlackTurn) ->
      GameMove
        { playerColor = if wasBlackTurn then Black else White
        , move = move
        , boardStateAfter = board
        , timestamp = timestamp
        })
    (toList realMoveResults)

spec_Move :: Spec
spec_Move =
  around withSharedDB $ do
    describe "insertMove" $ do
      it "can insert a move without error" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            testMove = baseMove currentTime

        shouldSucceed
          ( do
              insertGame testGame
              insertMove testGame.gameId testMove
          )
          conn

      it "can insert multiple moves for the same game" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 9 10]

        shouldSucceed
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves
          )
          conn

    describe "insertMoves" $ do
      it "can insert multiple moves at once" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 9 10, Move 18 19]

        shouldSucceed
          ( do
              insertGame testGame
              insertMoves testGame.gameId moves
          )
          conn

      it "batch insertion behaves same as individual insertions" $ \conn -> do
        currentTime <- now
        let testGame1 =
              baseGame currentTime
                & #gameId
                .~ GameId "batch-test-1"
                & #name
                ?~ "Batch Test 1"
            testGame2 =
              baseGame currentTime
                & #gameId
                .~ GameId "batch-test-2"
                & #name
                ?~ "Batch Test 2"
            moves1 = generateMoves currentTime [Move 0 1, Move 9 10, Move 18 19]
            -- Same moves for game2 since we don't have unique ID constraints anymore
            moves2 = moves1

        shouldBeTrue
          ( do
              -- Insert moves individually for game1
              insertGame testGame1
              mapM_ (insertMove testGame1.gameId) moves1
              retrievedMoves1 <- getMovesForGame testGame1.gameId
              count1 <- getMoveCountForGame testGame1.gameId

              -- Insert moves as batch for game2
              insertGame testGame2
              insertMoves testGame2.gameId moves2
              retrievedMoves2 <- getMovesForGame testGame2.gameId
              count2 <- getMoveCountForGame testGame2.gameId

              -- Results should be identical in structure (count and move data, ignoring IDs)
              let structurallyEqual =
                    count1 == count2
                      && count1 == 3
                      && retrievedMoves1 == retrievedMoves2
              pure structurallyEqual
          )
          conn

      it "handles real game data with board layer preservation and ordering" $ \conn -> do
        currentTime <- now
        let testGame =
              baseGame currentTime
                & #gameId
                .~ GameId "real-game-comprehensive"
                & #name
                ?~ "Real Game Comprehensive Test"
            realGameMoves = makeRealGameMoves (length $ toList realMoveResults) currentTime

        shouldBeTrue
          ( do
              insertGame testGame
              insertMoves testGame.gameId realGameMoves

              -- Verify all moves were inserted with correct count and board states
              retrievedMoves <- getMovesForGame testGame.gameId
              moveCount <- getMoveCountForGame testGame.gameId

              -- Check count matches expected
              let expectedCount = length realGameMoves
                  countMatches = moveCount == expectedCount && length retrievedMoves == expectedCount

              -- Check that all board layer data round-trips correctly
              let layersMatch = all
                    (\(original, retrieved) ->
                      original.boardStateAfter == retrieved.boardStateAfter)
                    (zip realGameMoves retrievedMoves)

              -- Check that moves are returned in correct order
              let orderingCorrect = retrievedMoves == realGameMoves

              pure (countMatches && layersMatch && orderingCorrect)
          )
          conn

    describe "getMove" $ it "can retrieve a move that was inserted" $ \conn -> do
      currentTime <- now
      let testGame = baseGame currentTime
          testMove =
            baseMove currentTime
              & #move
              .~ Move 2 3
              & #boardStateAfter
              .~ ExternBoard{black = Layer 0 0, white = Layer 0 0, king = 27}

      resultEquals
        ( do
            insertGame testGame
            insertMove testGame.gameId testMove
            getMove testGame.gameId 0  -- First move has move number 0
        )
        testMove
        conn

    describe "getMovesForGame and empty state behavior" $ do
      it "handles empty games and populated games correctly" $ \conn -> do
        currentTime <- now
        let emptyGame =
              baseGame currentTime
                & #gameId .~ GameId "empty-test"
                & #name ?~ "Empty Test Game"
            populatedGame =
              baseGame currentTime
                & #gameId .~ GameId "populated-test"
                & #name ?~ "Populated Test Game"
            expectedMoves = makeRealGameMoves 5 currentTime

        shouldBeTrue
          ( do
              -- Test empty game behavior across multiple functions
              insertGame emptyGame
              emptyMoves <- getMovesForGame emptyGame.gameId
              emptyLatestMove <- getLatestMoveForGame emptyGame.gameId
              emptyCount <- getMoveCountForGame emptyGame.gameId
              insertMoves emptyGame.gameId [] -- Test empty insertMoves

              -- Test populated game
              insertGame populatedGame
              mapM_ (insertMove populatedGame.gameId) expectedMoves
              populatedMoves <- getMovesForGame populatedGame.gameId
              populatedCount <- getMoveCountForGame populatedGame.gameId

              pure ( emptyMoves == []
                  && emptyLatestMove == Nothing
                  && emptyCount == 0
                  && populatedMoves == expectedMoves
                  && populatedCount == 5
                   )
          )
          conn

    describe "getLatestMoveForGame" $ do
      it "returns the latest move by move number" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 12 13]
            expectedLatest = fromMaybe (error "empty moves") (viaNonEmpty last moves)

        resultEquals
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves
              getLatestMoveForGame testGame.gameId
          )
          (Just expectedLatest)
          conn

    describe "deleteMove" $ do
      it "can delete a move and verify deletion" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            testMove = baseMove currentTime

        shouldSucceed
          ( do
              insertGame testGame
              insertMove testGame.gameId testMove
              deleteMove testGame.gameId 0  -- Delete first move (move number 0)
          )
          conn

        -- Verify move no longer exists
        shouldFail (getMove testGame.gameId 0) conn  -- Verify first move is gone

      it "deleting a non-existent move should succeed (no-op)" $ \conn -> do
        let nonExistentGameId = GameId "does-not-exist"

        shouldSucceed (deleteMove nonExistentGameId 999) conn

      it "can delete moves and track game state correctly" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 12 13]
            [_move1, _move2] = moves

        shouldBeTrue
          ( do
              insertGame testGame
              mapM_ (insertMove testGame.gameId) moves

              -- Check that we have 2 moves
              moveCount1 <- getMoveCountForGame testGame.gameId

              -- Delete the last move
              deleteMove testGame.gameId 1  -- Delete second move (move number 1)

              -- Check that we now have 1 move
              moveCount2 <- getMoveCountForGame testGame.gameId

              pure (moveCount1 == 2 && moveCount2 == 1)
          )
          conn

