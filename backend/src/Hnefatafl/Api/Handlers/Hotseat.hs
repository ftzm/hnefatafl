module Hnefatafl.Api.Handlers.Hotseat (
  hotseatServer,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Hnefatafl.Api.Routes.Hotseat (HotseatRoutes (..))
import Hnefatafl.Api.Types (
  ActionResponse (..),
  ApiGameMove (..),
  ApiGameState (..),
  ApiMove,
  boardFromExtern,
  gameStatusFromDomain,
  moveFromDomain,
  moveToDomain,
  positionsFromLayer,
 )
import Hnefatafl.App.Hotseat qualified as Hotseat
import Hnefatafl.Core.Data (
  Game (..),
  GameId,
  MoveWithCaptures (..),
  PlayerColor (..),
 )
import Hnefatafl.Effect.Clock (Clock)
import Hnefatafl.Effect.IdGen (IdGen)
import Hnefatafl.Effect.Log (
  KatipE,
  Severity (..),
  katipAddNamespace,
  logTM,
  ls,
 )
import Hnefatafl.Effect.Storage (Storage)
import Hnefatafl.Game.Common qualified as Common
import Hnefatafl.Game.Hotseat qualified as HotseatGame
import Servant (ServerError (..), err400)
import Servant.Server.Generic (AsServerT)

hotseatServer ::
  ( Storage :> es
  , Clock :> es
  , IdGen :> es
  , KatipE :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  HotseatRoutes (AsServerT (Eff es))
hotseatServer =
  HotseatRoutes
    { create = createHandler
    , get = getHandler
    , move = moveHandler
    , undo = undoHandler
    , resign = resignHandler
    , draw = drawHandler
    }

-- Conversion helpers

toApiGameMove :: Common.AppliedMove -> ApiGameMove
toApiGameMove am =
  ApiGameMove
    { playerColor = am.side
    , move = moveFromDomain (MoveWithCaptures am.move am.captures)
    , captures = positionsFromLayer am.captures
    }

toApiGameState :: GameId -> HotseatGame.State -> ApiGameState
toApiGameState gId (HotseatGame.State board moves phase) =
  ApiGameState
    { gameId = gId
    , board = boardFromExtern board
    , turn = case phase of
        HotseatGame.Awaiting t _ -> t
        HotseatGame.Finished _ -> Black
    , status = case phase of
        HotseatGame.Awaiting _ _ -> gameStatusFromDomain Nothing
        HotseatGame.Finished outcome -> gameStatusFromDomain (Just outcome)
    , history = map toApiGameMove moves
    , validMoves = case phase of
        HotseatGame.Awaiting _ vm -> map moveFromDomain vm
        HotseatGame.Finished _ -> []
    }

toActionResponse :: HotseatGame.State -> ActionResponse
toActionResponse (HotseatGame.State _ _ phase) =
  ActionResponse
    { turn = case phase of
        HotseatGame.Awaiting t _ -> t
        HotseatGame.Finished _ -> Black
    , status = case phase of
        HotseatGame.Awaiting _ _ -> gameStatusFromDomain Nothing
        HotseatGame.Finished outcome -> gameStatusFromDomain (Just outcome)
    , validMoves = case phase of
        HotseatGame.Awaiting _ vm -> map moveFromDomain vm
        HotseatGame.Finished _ -> []
    }

badRequest :: Error ServerError :> es => Text -> Eff es a
badRequest msg = throwError err400{errBody = encodeUtf8 msg}

-- Handlers

createHandler ::
  (Storage :> es, Clock :> es, IdGen :> es, KatipE :> es) =>
  Eff es ApiGameState
createHandler = katipAddNamespace "hotseat" $ do
  game <- Hotseat.createGame
  gameState <- Hotseat.loadGameState game.gameId
  pure $ toApiGameState game.gameId gameState

getHandler ::
  (Storage :> es, KatipE :> es) =>
  GameId -> Eff es ApiGameState
getHandler gameId = katipAddNamespace "hotseat" $ do
  gameState <- Hotseat.loadGameState gameId
  pure $ toApiGameState gameId gameState

moveHandler ::
  (Storage :> es, Clock :> es, KatipE :> es, Error ServerError :> es) =>
  GameId -> ApiMove -> Eff es ActionResponse
moveHandler gameId apiMove = katipAddNamespace "hotseat" $ do
  result <- Hotseat.makeMove gameId (moveToDomain apiMove)
  case result of
    Left err -> do
      $(logTM) WarningS $ ls @Text ("Move failed: " <> show err)
      badRequest "invalid_move"
    Right gu -> pure $ toActionResponse gu

undoHandler ::
  (Storage :> es, Clock :> es, KatipE :> es, Error ServerError :> es) =>
  GameId -> Eff es ActionResponse
undoHandler gameId = katipAddNamespace "hotseat" $ do
  result <- Hotseat.undoMove gameId
  case result of
    Left err -> do
      $(logTM) WarningS $ ls @Text ("Undo failed: " <> show err)
      badRequest "undo_failed"
    Right gu -> pure $ toActionResponse gu

resignHandler ::
  (Storage :> es, Clock :> es, KatipE :> es, Error ServerError :> es) =>
  GameId -> PlayerColor -> Eff es ActionResponse
resignHandler gameId color = katipAddNamespace "hotseat" $ do
  result <- Hotseat.resign gameId color
  case result of
    Left err -> do
      $(logTM) WarningS $ ls @Text ("Resign failed: " <> show err)
      badRequest "resign_failed"
    Right gu -> pure $ toActionResponse gu

drawHandler ::
  (Storage :> es, Clock :> es, KatipE :> es, Error ServerError :> es) =>
  GameId -> Eff es ActionResponse
drawHandler gameId = katipAddNamespace "hotseat" $ do
  result <- Hotseat.agreeDraw gameId
  case result of
    Left err -> do
      $(logTM) WarningS $ ls @Text ("Draw failed: " <> show err)
      badRequest "draw_failed"
    Right gu -> pure $ toActionResponse gu
