module Hnefatafl.App.HotseatTest where

import Effectful (Eff, IOE, (:>))
import Hnefatafl.App.Hotseat qualified as Hotseat
import Hnefatafl.App.TestUtil (runHotseatTest)
import Hnefatafl.Bindings (startBoard)
import Hnefatafl.Core.Data (
  Game (..),
  GameId,
  Move (..),
  MoveResult (..),
  Outcome (..),
  PlayerColor (..),
 )
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Game.Hotseat (Phase (..), State (..))
import Hnefatafl.Interpreter.Storage.SQLite.Util (withSharedDB)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import TestUtil (realMoveResults)

assertRight :: Show e => Either e a -> IO a
assertRight (Right a) = pure a
assertRight (Left e) = fail $ "Expected Right, got Left: " <> show e

spec_hotseat_integration :: Spec
spec_hotseat_integration = around withSharedDB $ do
  describe "Hotseat integration" $ do
    it "creates a game and loads initial state" $ \connVar -> do
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        st <- Hotseat.loadGameState g.gameId
        let State brd _ phase = st
        liftIO $ do
          brd `shouldBe` startBoard
          case phase of
            Awaiting Black _ -> pure ()
            other -> fail $ "expected Awaiting Black, got: " <> show other

    it "makes a move and round-trips through DB" $ \connVar -> do
      let firstMove = (.move) $ head realMoveResults
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        result <- Hotseat.makeMove g.gameId firstMove
        liftIO $ result `shouldSatisfy` isRight
        st <- liftIO $ assertRight result
        reloaded <- Hotseat.loadGameState g.gameId
        liftIO $ reloaded `shouldBe` st

    it "plays multiple moves and verifies state after each" $ \connVar -> do
      let moves = take 6 $ map (.move) (toList realMoveResults)
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        for_ moves $ \mv -> do
          result <- Hotseat.makeMove g.gameId mv
          liftIO $ result `shouldSatisfy` isRight
          st <- liftIO $ assertRight result
          reloaded <- Hotseat.loadGameState g.gameId
          liftIO $ reloaded `shouldBe` st

    it "undoes a move" $ \connVar -> do
      let firstMove = (.move) $ head realMoveResults
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        initialSt <- Hotseat.loadGameState g.gameId
        _ <- Hotseat.makeMove g.gameId firstMove
        undoResult <- Hotseat.undoMove g.gameId
        liftIO $ undoResult `shouldSatisfy` isRight
        reloaded <- Hotseat.loadGameState g.gameId
        liftIO $ reloaded `shouldBe` initialSt

    it "resigns" $ \connVar -> do
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        result <- Hotseat.resign g.gameId Black
        liftIO $ case result of
          Right (State _ _ (Finished (ResignedBy Black))) -> pure ()
          other -> fail $ "expected Finished ResignedBy Black, got: " <> show other

    it "agrees to draw" $ \connVar -> do
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        result <- Hotseat.agreeDraw g.gameId
        liftIO $ case result of
          Right (State _ _ (Finished Draw)) -> pure ()
          other -> fail $ "expected Finished Draw, got: " <> show other

    it "plays a full game to completion" $ \connVar -> do
      let allMoves = map (.move) (toList realMoveResults)
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        playAllMoves g.gameId allMoves
        finalSt <- Hotseat.loadGameState g.gameId
        liftIO $ case finalSt of
          State _ _ (Finished _) -> pure ()
          other -> fail $ "expected game to finish, got: " <> show other

    it "rejects an invalid move" $ \connVar -> do
      let invalidMove = Move 0 0
      runHotseatTest connVar $ do
        g <- Hotseat.createGame
        result <- Hotseat.makeMove g.gameId invalidMove
        liftIO $ result `shouldSatisfy` isLeft
        st <- Hotseat.loadGameState g.gameId
        let State brd _ phase = st
        liftIO $ do
          brd `shouldBe` startBoard
          case phase of
            Awaiting Black _ -> pure ()
            other -> fail $ "expected Awaiting Black, got: " <> show other

-- | Play moves until the game ends or we run out of moves.
playAllMoves ::
  (Storage :> es, Clock :> es, IOE :> es, Trace :> es) =>
  GameId ->
  [Move] ->
  Eff es ()
playAllMoves _ [] = pure ()
playAllMoves gameId (mv : rest) = do
  result <- Hotseat.makeMove gameId mv
  case result of
    Left _ -> pure ()
    Right (State _ _ (Finished _)) -> pure ()
    Right _ -> playAllMoves gameId rest
