module Hnefatafl.App.OnlineSessionTest where

import Data.List ((!!))

import Chronos (Time (..), Timespan (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Database.SQLite.Simple (Connection)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.STM qualified as STM
import Effectful.Katip (KatipE)
import Hnefatafl.Api.Types (Position (..))
import Hnefatafl.Api.Types.WS.Online (OnlineClientMessage (..))
import Hnefatafl.App.Online (CreateGameResult (..), GameSessions)
import Hnefatafl.App.Online qualified as Online
import Hnefatafl.App.TestUtil (
  Inbox (..),
  TestConnPair (..),
  clientRecvJSON,
  clientSendAuth,
  clientSendMsg,
  expectMessages,
  mkTestConnPair,
  runOnlineTest,
  runOnlineTestTimed,
 )
import Hnefatafl.Core.Data (
  GameParticipantToken (..),
  Move (..),
  MoveResult (..),
  Seconds (..),
  TimeControl (..),
 )
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Hnefatafl.Interpreter.Storage.SQLite.Util (withSharedDB)
import Hnefatafl.Metrics (HMetrics)
import Refined.Unsafe (reallyUnsafeRefine)
import StmContainers.Map qualified as STMMap
import Test.Hspec (Spec, around, describe, it, shouldBe)
import TestUtil (realMoveResults)
import Torsor (add)

-------------------------------------------------------------------------------
-- Move fixtures from a known valid game. Alternates black/white.

validMoves :: [OnlineClientMessage]
validMoves =
  map
    ( \mr ->
        OnlineMove
          (Position $ fromIntegral mr.move.orig)
          (Position $ fromIntegral mr.move.dest)
    )
    (toList realMoveResults)

-------------------------------------------------------------------------------
-- Helpers

msgType :: Aeson.Value -> Maybe Text
msgType (Aeson.Object obj) =
  case KeyMap.lookup "type" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
msgType _ = Nothing

msgField :: Text -> Aeson.Value -> Maybe Text
msgField key (Aeson.Object obj) =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
msgField _ _ = Nothing

type TestEff es =
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , Concurrent :> es
  , WebSocket :> es
  , KatipE :> es
  , Trace :> es
  , HMetrics :> es
  , IOE :> es
  )

data TestGame = TestGame
  { sessions :: GameSessions
  , result :: CreateGameResult
  }

-- | Create an untimed game with a shared session map.
setupGame :: TestEff es => Eff es TestGame
setupGame = do
  sessions <- STM.atomically STMMap.new
  result <- Online.createGame Nothing
  pure TestGame{sessions, result}

-- | Create a timed game with 1-second clock and 0 increment.
setupTimedGame :: TestEff es => Eff es TestGame
setupTimedGame = do
  sessions <- STM.atomically STMMap.new
  let tc =
        TimeControl
          (reallyUnsafeRefine (Seconds 1))
          (reallyUnsafeRefine (Seconds 0))
  result <- Online.createGame (Just tc)
  pure TestGame{sessions, result}

-- | The test-facing handle for a connected player. Contains only
-- what test bodies need: a way to send and a way to receive.
data Player = Player
  { inbox :: Inbox
  , send :: OnlineClientMessage -> IO ()
  }

-- Internal: full connection state needed for lifecycle management.
data ConnectedPlayer = ConnectedPlayer
  { player :: Player
  , wsThread :: Async.Async ()
  }

-- | Connect a player via the full handleWebSocket path.
connectPlayer ::
  TestEff es =>
  TestGame ->
  GameParticipantToken ->
  Eff es (ConnectedPlayer, Aeson.Value)
connectPlayer game token = do
  tc <- liftIO mkTestConnPair
  wsThread <-
    Async.async $
      Online.handleWebSocket game.sessions tc.connection
  liftIO $ clientSendAuth tc token.token
  initialState <- liftIO $ clientRecvJSON tc
  let player = Player{inbox = Inbox tc.serverToClient, send = clientSendMsg tc}
  pure (ConnectedPlayer{player, wsThread}, initialState)

disconnectPlayer :: Concurrent :> es => ConnectedPlayer -> Eff es ()
disconnectPlayer = Async.cancel . (.wsThread)

-- | Connect both players, drain opponentJoined, run an action
-- with the two Player handles, then disconnect both.
withBothPlayers ::
  (forall es. TestEff es => Player -> Player -> Eff es ()) ->
  MVar Connection ->
  IO ()
withBothPlayers = withBothPlayersUsing setupGame

-- | Like withBothPlayers but for timed games with a controllable
-- test clock. The action receives the clock TVar to advance time.
withBothPlayersTimed ::
  (forall es. TestEff es => TVar Time -> Player -> Player -> Eff es ()) ->
  MVar Connection ->
  IO ()
withBothPlayersTimed action connVar =
  runOnlineTestTimed connVar $ \clock -> do
    game <- setupTimedGame
    (white, _) <- connectPlayer game game.result.whiteToken
    (black, _) <- connectPlayer game game.result.blackToken
    _ <-
      liftIO $
        expectMessages white.player.inbox ["opponentJoined"] black.player.inbox []
    action clock white.player black.player
    disconnectPlayer white
    disconnectPlayer black

withBothPlayersUsing ::
  (forall es. TestEff es => Eff es TestGame) ->
  (forall es. TestEff es => Player -> Player -> Eff es ()) ->
  MVar Connection ->
  IO ()
withBothPlayersUsing setup action connVar =
  runOnlineTest connVar $ do
    game <- setup
    (white, _) <- connectPlayer game game.result.whiteToken
    (black, _) <- connectPlayer game game.result.blackToken
    _ <-
      liftIO $
        expectMessages white.player.inbox ["opponentJoined"] black.player.inbox []
    action white.player black.player
    disconnectPlayer white
    disconnectPlayer black

-------------------------------------------------------------------------------
-- Tests

spec_onlineSession :: Spec
spec_onlineSession = around withSharedDB $ do
  describe "online session" $ do
    it "sends game state on connect" $ \connVar -> do
      runOnlineTest connVar $ do
        game <- setupGame
        (player, gameState) <- connectPlayer game game.result.whiteToken
        liftIO $ do
          msgType gameState `shouldBe` Just "gameState"
          msgField "playerColor" gameState `shouldBe` Just "white"
        disconnectPlayer player

    it "notifies first player when second connects" $ \connVar -> do
      runOnlineTest connVar $ do
        game <- setupGame
        (white, _) <- connectPlayer game game.result.whiteToken
        (black, _) <- connectPlayer game game.result.blackToken
        _ <-
          liftIO $
            expectMessages white.player.inbox ["opponentJoined"] black.player.inbox []
        disconnectPlayer white
        disconnectPlayer black

    it "delivers move to opponent" $ withBothPlayers $ \white black -> do
      liftIO $ black.send (validMoves !! 0)
      _ <- liftIO $ expectMessages white.inbox ["moveMade"] black.inbox []
      pass

    it "sends error for invalid move without crashing" $
      withBothPlayers $ \white black -> do
        liftIO $ white.send (validMoves !! 1)
        _ <- liftIO $ expectMessages white.inbox ["error"] black.inbox []
        liftIO $ black.send (validMoves !! 0)
        _ <- liftIO $ expectMessages white.inbox ["moveMade"] black.inbox []
        pass

    it "resign ends the game and notifies both" $
      withBothPlayers $ \white black -> do
        liftIO $ black.send OnlineResign
        _ <- liftIO $ expectMessages white.inbox ["gameOver"] black.inbox ["gameOver"]
        pass

    it "disconnect notifies remaining player" $ \connVar -> do
      runOnlineTest connVar $ do
        game <- setupGame
        (white, _) <- connectPlayer game game.result.whiteToken
        (black, _) <- connectPlayer game game.result.blackToken
        _ <-
          liftIO $
            expectMessages white.player.inbox ["opponentJoined"] black.player.inbox []
        disconnectPlayer black
        _ <-
          liftIO $
            expectMessages white.player.inbox ["opponentLeft"] black.player.inbox []
        disconnectPlayer white

    it "reconnect receives resumed game state with history" $ \connVar -> do
      runOnlineTest connVar $ do
        game <- setupGame
        (white, _) <- connectPlayer game game.result.whiteToken
        (black, _) <- connectPlayer game game.result.blackToken
        _ <-
          liftIO $
            expectMessages white.player.inbox ["opponentJoined"] black.player.inbox []
        liftIO $ black.player.send (validMoves !! 0)
        _ <-
          liftIO $
            expectMessages white.player.inbox ["moveMade"] black.player.inbox []
        disconnectPlayer black
        _ <-
          liftIO $
            expectMessages white.player.inbox ["opponentLeft"] black.player.inbox []
        (black2, gameState) <- connectPlayer game game.result.blackToken
        liftIO $ case gameState of
          Aeson.Object obj ->
            case KeyMap.lookup "history" obj of
              Just (Aeson.Array arr) ->
                length arr `shouldBe` 1
              other -> fail $ "Expected history array, got " <> show other
          other -> fail $ "Expected object, got " <> show other
        disconnectPlayer white
        disconnectPlayer black2

    it "multiple moves maintain correct turn order" $
      withBothPlayers $ \white black -> do
        liftIO $ black.send (validMoves !! 0)
        _ <- liftIO $ expectMessages white.inbox ["moveMade"] black.inbox []
        liftIO $ white.send (validMoves !! 1)
        _ <- liftIO $ expectMessages black.inbox ["moveMade"] white.inbox []
        liftIO $ black.send (validMoves !! 2)
        _ <- liftIO $ expectMessages white.inbox ["moveMade"] black.inbox []
        pass

    it "timeout fires and ends timed game" $
      withBothPlayersTimed $ \clock white black -> do
        liftIO $ black.send (validMoves !! 0)
        _ <-
          liftIO $
            expectMessages white.inbox ["moveMade"] black.inbox ["clockUpdated"]
        -- Advance clock past the 1-second timeout
        STM.atomically $ STM.modifyTVar' clock (add (Timespan 1_200_000_000))
        _ <-
          liftIO $
            expectMessages white.inbox ["gameOver"] black.inbox ["gameOver"]
        pass

    it "move resets timeout timer" $
      withBothPlayersTimed $ \clock white black -> do
        liftIO $ black.send (validMoves !! 0)
        _ <-
          liftIO $
            expectMessages white.inbox ["moveMade"] black.inbox ["clockUpdated"]
        -- Advance 800ms (not enough to timeout)
        STM.atomically $ STM.modifyTVar' clock (add (Timespan 800_000_000))
        liftIO $ white.send (validMoves !! 1)
        _ <-
          liftIO $
            expectMessages black.inbox ["moveMade"] white.inbox ["clockUpdated"]
        -- Advance another 800ms (1.6s total > 1s, but timer was reset)
        STM.atomically $ STM.modifyTVar' clock (add (Timespan 800_000_000))
        liftIO $ black.send (validMoves !! 2)
        _ <-
          liftIO $
            expectMessages white.inbox ["moveMade"] black.inbox ["clockUpdated"]
        pass

    it "resignation cancels pending timeout" $
      withBothPlayersTimed $ \clock white black -> do
        liftIO $ black.send (validMoves !! 0)
        _ <-
          liftIO $
            expectMessages white.inbox ["moveMade"] black.inbox ["clockUpdated"]
        -- White's clock is ticking. Black resigns before timeout.
        liftIO $ black.send OnlineResign
        _ <-
          liftIO $
            expectMessages white.inbox ["gameOver"] black.inbox ["gameOver"]
        -- Advance clock well past what would have been the timeout.
        -- If timer wasn't cancelled, this would enqueue a stale
        -- TimeoutFired and cause problems on next queue read.
        STM.atomically $ STM.modifyTVar' clock (add (Timespan 5_000_000_000))
        pass

    it "draw offer does not reset timeout" $
      withBothPlayersTimed $ \clock white black -> do
        liftIO $ black.send (validMoves !! 0)
        _ <-
          liftIO $
            expectMessages white.inbox ["moveMade"] black.inbox ["clockUpdated"]
        -- Advance 800ms
        STM.atomically $ STM.modifyTVar' clock (add (Timespan 800_000_000))
        -- White offers a draw (no clock change, timer should NOT reset)
        liftIO $ white.send OnlineOfferDraw
        _ <-
          liftIO $
            expectMessages black.inbox ["drawOffered"] white.inbox []
        -- Advance another 300ms (total 1.1s > 1s timeout)
        -- If timer was incorrectly reset on draw offer, this wouldn't timeout
        STM.atomically $ STM.modifyTVar' clock (add (Timespan 300_000_000))
        _ <-
          liftIO $
            expectMessages white.inbox ["gameOver"] black.inbox ["gameOver"]
        pass
