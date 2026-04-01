module Hnefatafl.App.Storage (
  persistenceCommandsToTx,
) where

import Chronos (Time)
import Hnefatafl.Core.Data (GameId)
import Hnefatafl.Effect.Storage
import Hnefatafl.Game.Common (
  PersistenceCommand (..),
  outcomeToGameStatus,
  toGameMove,
 )

persistenceCommandsToTx ::
  GameId -> Time -> [PersistenceCommand] -> StorageTx ()
persistenceCommandsToTx gameId time = traverse_ go
 where
  go = \case
    PersistMove am -> insertMove gameId (toGameMove am)
    DeleteMoves n -> deleteLastNMoves gameId n
    PersistOutcome o -> updateGameStatus gameId (outcomeToGameStatus o) (Just time)
    PersistPendingAction pa -> insertPendingAction gameId pa time
    ClearPendingAction -> deletePendingAction gameId
    NoOp -> pure ()
