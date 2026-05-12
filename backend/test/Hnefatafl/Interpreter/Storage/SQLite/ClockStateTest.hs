{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.ClockStateTest where

import Chronos (now)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Effect.Storage
import Hnefatafl.Game.TestUtil (mkRemaining)
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Optics
import Test.Hspec (Spec, around, describe, it)

spec_ClockState :: Spec
spec_ClockState =
  around withSharedDB $ do
    describe "clock state" $ do
      it "returns Nothing for a game with no clock state" $ \conn -> do
        currentTime <- now
        let game =
              baseGame currentTime
                & #gameId
                .~ GameId "no-clock-game"
                & #mode
                .~ Online Nothing Nothing
        resultEquals
          ( runTransaction $ do
              insertGame game
              getOnlineClockState game.gameId
          )
          Nothing
          conn

      it "round-trips a clock state" $ \conn -> do
        currentTime <- now
        let game =
              baseGame currentTime
                & #gameId
                .~ GameId "clock-game"
                & #mode
                .~ Online Nothing Nothing
            cs =
              ClockState
                (mkRemaining 300)
                (mkRemaining 300)
                currentTime
        resultEquals
          ( runTransaction $ do
              insertGame game
              setOnlineClockState game.gameId cs
              getOnlineClockState game.gameId
          )
          (Just cs)
          conn
