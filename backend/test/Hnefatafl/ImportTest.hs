{-# LANGUAGE BlockArguments #-}

module Hnefatafl.ImportTest where

import Chronos (timeFromYmdhms)
import Data.Aeson (decode, encode)
import Data.Text (isInfixOf)
import Database.SQLite.Simple (Connection)
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Hnefatafl.Core.Data (
  Game (..),
  GameStatus (..),
  HumanPlayer (..),
  Move (..),
  MoveResult (..),
 )
import Hnefatafl.Effect.Clock
import Hnefatafl.Effect.IdGen
import Hnefatafl.Effect.Storage
import Hnefatafl.Import (GameImport (..), importGame)
import Hnefatafl.Interpreter.Clock.IO
import Hnefatafl.Interpreter.IdGen.UUIDv7
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Test.Hspec (Spec, around, describe, it)
import Test.Hspec.Expectations.Pretty
import TestUtil (realMoveResults)

spec_GameImportRoundTrip :: Spec
spec_GameImportRoundTrip =
  describe "GameImport JSON round-trip" $ do
    it "should serialize and deserialize correctly with all fields" $ do
      let testTime = timeFromYmdhms 2023 11 15 14 30 0
          testMoves = Move 1 2 :| [Move 5 77, Move 110 109]
          original =
            GameImport
              { gameName = Just "Test Game"
              , blackPlayerName = "Alice"
              , whitePlayerName = "Bob"
              , startTime = Just testTime
              , endTime = Just testTime
              , gameStatus = Just "Ongoing"
              , moves = testMoves
              }
          encoded = encode original
          decoded = decode encoded

      decoded `shouldBe` Just original

    it "should serialize and deserialize correctly with minimal fields" $ do
      let testMoves = Move 10 20 :| []
          original =
            GameImport
              { gameName = Nothing
              , blackPlayerName = "Player1"
              , whitePlayerName = "Player2"
              , startTime = Nothing
              , endTime = Nothing
              , gameStatus = Nothing
              , moves = testMoves
              }
          encoded = encode original
          decoded = decode encoded

      decoded `shouldBe` Just original

-- | Helper function to run ImportGame with all required effects
runImportTest ::
  MVar Connection ->
  ( forall es.
    (IOE :> es, Error String :> es, Storage :> es, Clock :> es, IdGen :> es) =>
    Eff es a
  ) ->
  IO (Either String a)
runImportTest connectionVar action =
  runEff $
    runErrorNoCallStack @String $
      runStorageSQLiteWithRollback connectionVar $
        runClockIO $
          runIdGenUUIDv7 action

spec_ImportGame :: Spec
spec_ImportGame =
  around withSharedDB $ do
    describe "importGame" $ do
      it "can import a simple game with valid moves" $ \conn -> do
        let testMoves = take 2 (map move (toList realMoveResults))
            testMovesNE = fromList testMoves
            gameImport =
              GameImport
                { gameName = Just "Test Import Game"
                , blackPlayerName = "Alice"
                , whitePlayerName = "Bob"
                , startTime = Nothing
                , endTime = Nothing
                , gameStatus = Nothing
                , moves = testMovesNE
                }

        result <- runImportTest conn $ do
          importResult <- importGame gameImport
          case importResult of
            Left importErr -> error $ "Import failed: " <> importErr
            Right _ -> do
              -- Verify players were created
              alice <- humanPlayerFromName "Alice"
              bob <- humanPlayerFromName "Bob"

              -- Verify game was imported
              games <- listGames
              let importedGame = find (\g -> g.name == Just "Test Import Game") games

              case (alice, bob, importedGame) of
                (Just alicePlayer, Just bobPlayer, Just game) -> do
                  -- Verify moves were stored
                  moves <- getMovesForGame game.gameId
                  moveCount <- getMoveCountForGame game.gameId
                  pure (alicePlayer, bobPlayer, game, moves, moveCount)
                _ -> error "Players or game not found after import"

        case result of
          Left err -> expectationFailure $ "Expected success but got error: " ++ err
          Right (alice, bob, game, moves, moveCount) -> do
            -- Verify players
            alice.name `shouldBe` "Alice"
            bob.name `shouldBe` "Bob"

            -- Verify game properties
            game.name `shouldBe` Just "Test Import Game"
            game.gameStatus `shouldBe` Ongoing
            isJust game.blackPlayerId `shouldBe` True
            isJust game.whitePlayerId `shouldBe` True

            -- Verify moves
            moveCount `shouldBe` 2
            length moves `shouldBe` 2

      it "can import a game with explicit status" $ \conn -> do
        let testMoves = take 1 (map move (toList realMoveResults))
            testMovesNE = fromList testMoves
            testTime = timeFromYmdhms 2023 11 15 14 30 0
            gameImport =
              GameImport
                { gameName = Just "Completed Game"
                , blackPlayerName = "Player 1"
                , whitePlayerName = "Player 2"
                , startTime = Just testTime
                , endTime = Just testTime
                , gameStatus = Just "white_won_king_escaped"
                , moves = testMovesNE
                }

        result <- runImportTest conn $ do
          importResult <- importGame gameImport
          case importResult of
            Left importErr -> error $ "Import failed: " <> importErr
            Right _ -> do
              -- Verify players were created
              player1 <- humanPlayerFromName "Player 1"
              player2 <- humanPlayerFromName "Player 2"
              pure (player1, player2)

        case result of
          Left err -> expectationFailure $ "Expected success but got error: " ++ err
          Right (player1, player2) -> do
            isJust player1 `shouldBe` True
            isJust player2 `shouldBe` True

      it "fails gracefully with invalid moves" $ \conn -> do
        let testMoves = Move 255 255 :| []
            gameImport =
              GameImport
                { gameName = Just "Invalid Game"
                , blackPlayerName = "Alice"
                , whitePlayerName = "Bob"
                , startTime = Nothing
                , endTime = Nothing
                , gameStatus = Nothing
                , moves = testMoves
                }

        result <- runImportTest conn (importGame gameImport)
        case result of
          Left err -> expectationFailure $ "Test setup failed: " ++ err
          Right (Left _) -> pure () -- Expected failure due to invalid moves
          Right (Right _) -> expectationFailure "Expected import to fail with invalid moves"

      it "fails gracefully with duplicate game name" $ \conn -> do
        let testMoves = take 1 (map move (toList realMoveResults))
            testMovesNE = fromList testMoves
            gameImport =
              GameImport
                { gameName = Just "Duplicate Name Game"
                , blackPlayerName = "Alice"
                , whitePlayerName = "Bob"
                , startTime = Nothing
                , endTime = Nothing
                , gameStatus = Nothing
                , moves = testMovesNE
                }

        result <- runImportTest conn $ do
          -- First import should succeed
          firstResult <- importGame gameImport
          case firstResult of
            Left err -> error $ "First import should succeed but got: " <> err
            Right _ -> do
              -- Second import with same name should fail gracefully
              secondResult <- importGame gameImport
              case secondResult of
                Left err ->
                  if "Import failed" `isInfixOf` err
                    then pure () -- Expected failure with correct error message
                    else error $ "Expected error to contain 'Import failed' but got: " <> err
                Right _ -> error "Expected import to fail with duplicate name"

        case result of
          Left err -> expectationFailure $ "Test failed: " ++ err
          Right _ -> pure ()
