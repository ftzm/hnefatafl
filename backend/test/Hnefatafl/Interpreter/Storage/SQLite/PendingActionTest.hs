{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.PendingActionTest where

import Chronos (now)
import Hnefatafl.Core.Data (Game (..), Move (..), PlayerColor (..))
import Hnefatafl.Effect.Storage
import Hnefatafl.Game.Common (PendingAction (..), PendingActionType (..))
import Hnefatafl.Interpreter.Storage.SQLite.Util (
  baseGame,
  generateMoves,
  resultEquals,
  shouldBeTrue,
  shouldSucceed,
  withSharedDB,
 )
import Test.Hspec (Spec, around, describe, it)

spec_PendingAction :: Spec
spec_PendingAction =
  around withSharedDB $ do
    describe "insertPendingAction" $ do
      it "can insert and retrieve a draw offer" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            pa = PendingAction DrawOffer Black

        resultEquals
          ( do
              insertGame testGame
              insertPendingAction testGame . gameId pa currentTime
              getPendingAction testGame . gameId
          )
          (Just pa)
          conn

      it "can insert and retrieve an undo request" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            pa = PendingAction UndoRequest White

        resultEquals
          ( do
              insertGame testGame
              insertPendingAction testGame . gameId pa currentTime
              getPendingAction testGame . gameId
          )
          (Just pa)
          conn

    describe "getPendingAction" $ do
      it "returns Nothing when no pending action exists" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime

        resultEquals
          ( do
              insertGame testGame
              getPendingAction testGame . gameId
          )
          (Nothing :: Maybe PendingAction)
          conn

    describe "deletePendingAction" $ do
      it "removes a pending action" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            pa = PendingAction DrawOffer Black

        resultEquals
          ( do
              insertGame testGame
              insertPendingAction testGame . gameId pa currentTime
              deletePendingAction testGame . gameId
              getPendingAction testGame . gameId
          )
          (Nothing :: Maybe PendingAction)
          conn

      it "is a no-op when no pending action exists" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime

        shouldSucceed
          ( do
              insertGame testGame
              deletePendingAction testGame . gameId
          )
          conn

    describe "deleteLastNMoves" $ do
      it "deletes the last 1 move" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 9 10, Move 18 19]

        shouldBeTrue
          ( do
              insertGame testGame
              insertMoves testGame . gameId moves
              deleteLastNMoves testGame . gameId 1
              count <- getMoveCountForGame testGame . gameId
              pure (count == 2)
          )
          conn

      it "deletes the last 2 moves" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 9 10, Move 18 19, Move 27 28]

        shouldBeTrue
          ( do
              insertGame testGame
              insertMoves testGame . gameId moves
              deleteLastNMoves testGame . gameId 2
              count <- getMoveCountForGame testGame . gameId
              remaining <- getMovesForGame testGame . gameId
              pure (count == 2 && remaining == take 2 moves)
          )
          conn

      it "deletes all moves when n equals move count" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime
            moves = generateMoves currentTime [Move 0 1, Move 9 10]

        shouldBeTrue
          ( do
              insertGame testGame
              insertMoves testGame . gameId moves
              deleteLastNMoves testGame . gameId 2
              count <- getMoveCountForGame testGame . gameId
              pure (count == 0)
          )
          conn

      it "is a no-op on an empty game" $ \conn -> do
        currentTime <- now
        let testGame = baseGame currentTime

        shouldSucceed
          ( do
              insertGame testGame
              deleteLastNMoves testGame . gameId 1
          )
          conn
