module Hnefatafl.App.Hotseat (
  createGame,
  loadGameState,
  makeMove,
  undoMove,
  resign,
  agreeDraw,
) where

import Chronos (Time)
import Effectful (Eff, (:>))
import Effectful.Katip (KatipE, logTM)
import Hnefatafl.App.Storage (gameMoveToAppliedMoves, persistenceCommandsToTx)
import Hnefatafl.Core.Data (
  Game (..),
  GameId (..),
  GameMode (..),
  Move (..),
  PlayerColor (..),
 )
import Hnefatafl.Effect.Clock (Clock, now)
import Hnefatafl.Effect.IdGen (IdGen, generateId)
import Hnefatafl.Effect.Storage (
  Storage,
  StorageTx,
  getGame,
  getMovesForGame,
  insertGame,
  runTransaction,
 )
import Hnefatafl.Game.Common (
  TransitionError (..),
  currentBoard,
 )
import Hnefatafl.Game.Hotseat qualified as Hotseat
import Katip (Severity (..))

mkGame :: GameId -> Time -> Game
mkGame gameId time =
  Game
    { gameId = gameId
    , name = Nothing
    , mode = Hotseat Nothing
    , startTime = time
    , endTime = Nothing
    , outcome = Nothing
    , createdAt = time
    }

loadHotseatState :: GameId -> StorageTx Hotseat.State
loadHotseatState gameId = do
  game <- getGame gameId
  gameMoves <- getMovesForGame gameId
  let appliedMoves = gameMoveToAppliedMoves gameMoves
      board = currentBoard appliedMoves
  pure $
    Hotseat.reconstruct board appliedMoves game.outcome

processEvent ::
  (Storage :> es, Clock :> es) =>
  GameId ->
  Hotseat.Event ->
  Eff es (Either TransitionError Hotseat.State)
processEvent gameId event = do
  currentTime <- now
  runTransaction $ do
    gameState <- loadHotseatState gameId
    case Hotseat.transition gameState event of
      Left err -> pure (Left err)
      Right result -> do
        let cmds = [cmd | Hotseat.Persist cmd <- result.commands]
        persistenceCommandsToTx gameId currentTime cmds
        pure (Right result.newState)

createGame ::
  (Storage :> es, Clock :> es, IdGen :> es, KatipE :> es) =>
  Eff es Game
createGame = do
  game <- mkGame <$> generateId <*> now
  runTransaction $ insertGame game
  $(logTM) InfoS "game created"
  pure game

loadGameState ::
  Storage :> es =>
  GameId ->
  Eff es Hotseat.State
loadGameState gameId =
  runTransaction $ loadHotseatState gameId

undoMove ::
  (Storage :> es, Clock :> es) =>
  GameId ->
  Eff es (Either TransitionError Hotseat.State)
undoMove gameId = processEvent gameId Hotseat.Undo

resign ::
  (Storage :> es, Clock :> es) =>
  GameId ->
  PlayerColor ->
  Eff es (Either TransitionError Hotseat.State)
resign gameId color = processEvent gameId (Hotseat.Resign color)

agreeDraw ::
  (Storage :> es, Clock :> es) =>
  GameId ->
  Eff es (Either TransitionError Hotseat.State)
agreeDraw gameId = processEvent gameId Hotseat.AgreeDraw

makeMove ::
  (Storage :> es, Clock :> es) =>
  GameId ->
  Move ->
  Eff es (Either TransitionError Hotseat.State)
makeMove gameId move = do
  currentTime <- now
  processEvent gameId (Hotseat.MakeMove move currentTime)
