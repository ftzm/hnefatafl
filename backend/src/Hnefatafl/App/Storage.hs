module Hnefatafl.App.Storage (
  persistenceCommandsToTx,
  gameMoveToAppliedMoves,
) where

import Chronos (Time)
import Hnefatafl.Bindings (applyMoveSequence)
import Hnefatafl.Core.Data (
  GameId,
  GameMove (..),
  MoveResult (..),
 )
import Hnefatafl.Effect.Storage (
  StorageTx,
  deleteLastNMoves,
  deletePendingAction,
  insertMove,
  insertPendingAction,
  setOutcome,
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  PersistenceCommand (..),
  toGameMove,
 )

persistenceCommandsToTx ::
  GameId -> Time -> [PersistenceCommand] -> StorageTx ()
persistenceCommandsToTx gameId time = traverse_ go
 where
  go = \case
    PersistMove am -> insertMove gameId (toGameMove am)
    DeleteMoves n -> deleteLastNMoves gameId n
    PersistOutcome o -> setOutcome gameId o (Just time)
    PersistPendingAction pa -> insertPendingAction gameId pa time
    ClearPendingAction -> deletePendingAction gameId
    NoOp -> pure ()

-- | Recover AppliedMoves (with zobrist hashes) from stored GameMoves
-- by replaying the move sequence through the C engine.
gameMoveToAppliedMoves :: [GameMove] -> [AppliedMove]
gameMoveToAppliedMoves [] = []
gameMoveToAppliedMoves gameMoves =
  let moves = map (.move) gameMoves
      moveResults = toList $ fst $ applyMoveSequence (fromList moves)
   in zipWith toApplied gameMoves (map (.zobristHash) moveResults)
 where
  toApplied :: GameMove -> Word64 -> AppliedMove
  toApplied gm z =
    AppliedMove
      { move = gm.move
      , side = gm.playerColor
      , captures = gm.captures
      , boardAfter = gm.boardStateAfter
      , zobristHash = z
      , timestamp = gm.timestamp
      }
