{-# LANGUAGE BlockArguments #-}

module Hnefatafl.Interpreter.Storage.SQLite.TimeControlTest where

import Chronos (now)
import Hnefatafl.Core.Data as CoreData
import Hnefatafl.Effect.Storage
import Hnefatafl.Interpreter.Storage.SQLite.Util
import Optics
import Test.Hspec (Spec, around, describe, it)

spec_TimeControl :: Spec
spec_TimeControl =
  around withSharedDB $ do
    describe "time control" $ do
      it "returns Nothing for a game with no time control" $ \conn -> do
        currentTime <- now
        let game =
              baseGame currentTime
                & #gameId .~ GameId "untimed-game"
                & #mode .~ Online Nothing Nothing
        resultEquals
          ( runTransaction $ do
              insertGame game
              getOnlineTimeControl game.gameId
          )
          Nothing
          conn

      it "round-trips a time control" $ \conn -> do
        currentTime <- now
        let game =
              baseGame currentTime
                & #gameId .~ GameId "timed-game"
                & #mode .~ Online Nothing Nothing
            tc = TimeControl (Seconds 300) (Seconds 3)
        resultEquals
          ( runTransaction $ do
              insertGame game
              setOnlineTimeControl game.gameId tc
              getOnlineTimeControl game.gameId
          )
          (Just tc)
          conn
