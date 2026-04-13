module Hnefatafl.App.Storage (
  persistEvents,
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
  DomainEvent (..),
  PendingAction (..),
  PendingActionType (..),
  toGameMove,
 )

persistEvents :: GameId -> Time -> [DomainEvent] -> StorageTx ()
persistEvents gameId time = traverse_ $ \case
  MovePlayed am -> insertMove gameId (toGameMove am)
  GameEnded outcome -> setOutcome gameId outcome (Just time)
  MovesUndone n -> deleteLastNMoves gameId n
  DrawOffered color -> insertPendingAction gameId (PendingAction DrawOffer color) time
  UndoRequested color -> insertPendingAction gameId (PendingAction UndoRequest color) time
  DrawDeclined -> deletePendingAction gameId
  UndoDeclined -> deletePendingAction gameId
  OfferCancelled -> deletePendingAction gameId

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
