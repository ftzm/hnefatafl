module Hnefatafl.App.OnlineCreateTest where

import Hnefatafl.App.Online (CreateGameResult (..))
import Hnefatafl.App.Online qualified as Online
import Hnefatafl.App.TestUtil (runHotseatTest)
import Hnefatafl.Core.Data (
  Game (..),
  Seconds (..),
  TimeControl (..),
 )
import Hnefatafl.Effect.Storage (
  getOnlineTimeControl,
  runTransaction,
 )
import Hnefatafl.Interpreter.Storage.SQLite.Util (withSharedDB)
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec_onlineCreate :: Spec
spec_onlineCreate = around withSharedDB $ do
  describe "online create game" $ do
    it "creates an untimed game when no time control is provided" $ \connVar -> do
      runHotseatTest connVar $ do
        result <- Online.createGame Nothing
        tc <- runTransaction $ getOnlineTimeControl result.game.gameId
        liftIO $ tc `shouldBe` Nothing

    it "persists time control when provided" $ \connVar -> do
      runHotseatTest connVar $ do
        let tc = TimeControl (Seconds 300) (Seconds 3)
        result <- Online.createGame (Just tc)
        retrieved <- runTransaction $ getOnlineTimeControl result.game.gameId
        liftIO $ retrieved `shouldBe` Just tc
