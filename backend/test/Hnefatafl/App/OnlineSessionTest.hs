module Hnefatafl.App.OnlineSessionTest where

import Data.List ((!!))

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.STM qualified as STM
import Effectful.Katip (KatipE)
import Hnefatafl.Api.Types (Position (..))
import Hnefatafl.Api.Types.WS.Online (OnlineClientMessage (..))
import Hnefatafl.Core.Data (Move (..), MoveResult (..))
import Hnefatafl.App.Online (CreateGameResult (..), GameSessions)
import Hnefatafl.App.Online qualified as Online
import Hnefatafl.App.TestUtil (
  TestConnPair (..),
  clientRecvJSON,
  clientSendAuth,
  clientSendMsg,
  expectFrom,
  mkTestConnPair,
  runOnlineTest,
 )
import Hnefatafl.Core.Data (
  GameParticipantToken (..),
 )
import TestUtil (realMoveResults)
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Effect.Trace (Trace)
import Hnefatafl.Effect.WebSocket (WebSocket)
import Database.SQLite.Simple (Connection)
import Hnefatafl.Interpreter.Storage.SQLite.Util (withSharedDB)
import Hnefatafl.Metrics (HMetrics)
import StmContainers.Map qualified as STMMap
import Test.Hspec (Spec, around, describe, it, shouldBe)

-------------------------------------------------------------------------------
-- Move fixtures from a known valid game. Alternates black/white.

validMoves :: [OnlineClientMessage]
validMoves =
  map
    (\mr -> OnlineMove (Position $ fromIntegral mr.move.orig) (Position $ fromIntegral mr.move.dest))
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

-- | Create a game with a shared session map.
setupGame :: TestEff es => Eff es TestGame
setupGame = do
  sessions <- STM.atomically STMMap.new
  result <- Online.createGame Nothing
  pure TestGame{sessions, result}

data ConnectedPlayer = ConnectedPlayer
  { conn :: TestConnPair
  , wsThread :: Async.Async ()
  }

-- | Connect a player via the full handleWebSocket path.
-- Authenticates and returns after receiving the initial game state.
connectPlayer ::
  TestEff es =>
  TestGame ->
  GameParticipantToken ->
  Eff es (ConnectedPlayer, Aeson.Value)
connectPlayer game token = do
  tc <- liftIO mkTestConnPair
  wsThread <- Async.async $
    Online.handleWebSocket game.sessions tc.connection
  liftIO $ clientSendAuth tc token.token
  initialState <- liftIO $ clientRecvJSON tc
  pure (ConnectedPlayer{conn = tc, wsThread}, initialState)

disconnectPlayer :: Concurrent :> es => ConnectedPlayer -> Eff es ()
disconnectPlayer = Async.cancel . (.wsThread)

-- | Connect both players, drain the opponentJoined notification,
-- run an action, then disconnect both. The action receives the
-- game setup and both players with clean message queues.
withBothPlayers ::
  ( forall es.
    TestEff es =>
    TestGame -> ConnectedPlayer -> ConnectedPlayer -> Eff es ()
  ) ->
  MVar Connection ->
  IO ()
withBothPlayers action connVar =
  runOnlineTest connVar $ do
    game <- setupGame
    (white, _) <- connectPlayer game game.result.whiteToken
    (black, _) <- connectPlayer game game.result.blackToken
    _ <- liftIO $ clientRecvJSON white.conn -- opponentJoined
    action game white black
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
        msg <- liftIO $ clientRecvJSON white.conn
        liftIO $ msgType msg `shouldBe` Just "opponentJoined"
        disconnectPlayer white
        disconnectPlayer black

    it "delivers move to opponent" $ withBothPlayers $ \_ white black -> do
      liftIO $ clientSendMsg black.conn (validMoves !! 0)
      moveMsg <- liftIO $ expectFrom white.conn black.conn
      liftIO $ msgType moveMsg `shouldBe` Just "moveMade"

    it "sends error for invalid move without crashing" $
      withBothPlayers $ \_ white black -> do
        liftIO $ clientSendMsg white.conn (validMoves !! 1)
        errMsg <- liftIO $ expectFrom white.conn black.conn
        liftIO $ msgType errMsg `shouldBe` Just "error"
        liftIO $ clientSendMsg black.conn (validMoves !! 0)
        moveMsg <- liftIO $ expectFrom white.conn black.conn
        liftIO $ msgType moveMsg `shouldBe` Just "moveMade"

    it "resign ends the game and notifies both" $
      withBothPlayers $ \_ white black -> do
        liftIO $ clientSendMsg black.conn OnlineResign
        whiteMsg <- liftIO $ clientRecvJSON white.conn
        blackMsg <- liftIO $ clientRecvJSON black.conn
        liftIO $ do
          msgType whiteMsg `shouldBe` Just "gameOver"
          msgType blackMsg `shouldBe` Just "gameOver"

    it "disconnect notifies remaining player" $ \connVar -> do
      runOnlineTest connVar $ do
        game <- setupGame
        (white, _) <- connectPlayer game game.result.whiteToken
        (black, _) <- connectPlayer game game.result.blackToken
        _ <- liftIO $ clientRecvJSON white.conn -- opponentJoined
        disconnectPlayer black
        msg <- liftIO $ clientRecvJSON white.conn
        liftIO $ msgType msg `shouldBe` Just "opponentLeft"
        disconnectPlayer white

    it "reconnect receives resumed game state with history" $ \connVar -> do
      runOnlineTest connVar $ do
        game <- setupGame
        (white, _) <- connectPlayer game game.result.whiteToken
        (black, _) <- connectPlayer game game.result.blackToken
        _ <- liftIO $ clientRecvJSON white.conn -- opponentJoined
        liftIO $ clientSendMsg black.conn (validMoves !! 0)
        _ <- liftIO $ expectFrom white.conn black.conn -- moveMade
        disconnectPlayer black
        _ <- liftIO $ clientRecvJSON white.conn -- opponentLeft
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
      withBothPlayers $ \_ white black -> do
        liftIO $ clientSendMsg black.conn (validMoves !! 0)
        msg1 <- liftIO $ expectFrom white.conn black.conn
        liftIO $ msgType msg1 `shouldBe` Just "moveMade"
        liftIO $ clientSendMsg white.conn (validMoves !! 1)
        msg2 <- liftIO $ expectFrom black.conn white.conn
        liftIO $ msgType msg2 `shouldBe` Just "moveMade"
        liftIO $ clientSendMsg black.conn (validMoves !! 2)
        msg3 <- liftIO $ expectFrom white.conn black.conn
        liftIO $ msgType msg3 `shouldBe` Just "moveMade"
