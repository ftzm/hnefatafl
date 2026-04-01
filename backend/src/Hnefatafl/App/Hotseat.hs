module Hnefatafl.App.Hotseat (
  createGame,
  loadGameState,
  processEvent,
  gameMoveToAppliedMoves,
) where

import Effectful (Eff, (:>))
import Hnefatafl.App.Storage (persistenceCommandsToTx)
import Hnefatafl.Bindings (applyMoveSequence)
import Hnefatafl.Core.Data (
  Game (..),
  GameId,
  GameMode (..),
  GameMove (..),
  GameStatus (..),
  MoveResult (..),
 )
import Hnefatafl.Effect.Clock (Clock, now)
import Hnefatafl.Effect.IdGen (IdGen, generateId)
import Hnefatafl.Effect.Storage (
  Storage,
  getGame,
  getMovesForGame,
  insertGame,
  runTransaction,
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  TransitionError,
  gameStatusToOutcome,
 )
import Hnefatafl.Game.Hotseat qualified as Hotseat

-- | Recover AppliedMoves (with zobrist hashes) from stored GameMoves
-- by replaying the move sequence through the C engine.
gameMoveToAppliedMoves :: [GameMove] -> [AppliedMove]
gameMoveToAppliedMoves [] = []
gameMoveToAppliedMoves gameMoves =
  let moves = map (.move) gameMoves
      moveResults = toList $ fst $ applyMoveSequence (fromList moves)
   in zipWith toApplied gameMoves moveResults
 where
  toApplied :: GameMove -> MoveResult -> AppliedMove
  toApplied gm mr =
    AppliedMove
      { move = gm.move
      , side = gm.playerColor
      , captures = gm.captures
      , boardAfter = gm.boardStateAfter
      , zobristHash = mr.zobristHash
      , timestamp = gm.timestamp
      }

-- | Create a new hotseat game. Returns the generated GameId.
createGame ::
  (Storage :> es, Clock :> es, IdGen :> es) =>
  Eff es GameId
createGame = do
  gameId <- generateId
  currentTime <- now
  let game =
        Game
          { gameId = gameId
          , name = Nothing
          , mode = Hotseat Nothing
          , startTime = currentTime
          , endTime = Nothing
          , gameStatus = Ongoing
          , createdAt = currentTime
          }
  runTransaction $ insertGame game
  pure gameId

-- | Load a hotseat game's state from the database.
loadGameState ::
  Storage :> es =>
  GameId ->
  Eff es (Game, Hotseat.State)
loadGameState gameId = do
  (game, gameMoves) <-
    runTransaction $
      (,) <$> getGame gameId <*> getMovesForGame gameId
  let appliedMoves = gameMoveToAppliedMoves gameMoves
      outcome = gameStatusToOutcome game.gameStatus
      state = Hotseat.reconstruct appliedMoves outcome Nothing
  pure (game, state)

-- | Process an event against a hotseat game. Loads state, applies the
-- transition, and persists any resulting commands atomically.
processEvent ::
  (Storage :> es, Clock :> es) =>
  GameId ->
  Hotseat.Event ->
  Eff es (Either TransitionError Hotseat.State)
processEvent gameId event = do
  currentTime <- now
  runTransaction $ do
    game <- getGame gameId
    gameMoves <- getMovesForGame gameId
    let appliedMoves = gameMoveToAppliedMoves gameMoves
        outcome = gameStatusToOutcome game.gameStatus
        state = Hotseat.reconstruct appliedMoves outcome Nothing
    case Hotseat.transition state event of
      Left err -> pure (Left err)
      Right result -> do
        persistenceCommandsToTx
          gameId
          currentTime
          [cmd | Hotseat.Persist cmd <- result.commands]
        pure (Right result.newState)
