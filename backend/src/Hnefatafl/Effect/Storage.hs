{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.Storage (
  -- * Command algebra
  StorageCmd (..),

  -- * Transaction monad
  StorageTx (..),
  liftCmd,

  -- * Smart constructors
  insertHumanPlayer,
  getHumanPlayer,
  humanPlayerFromName,
  insertEnginePlayer,
  getEnginePlayer,
  getPlayer,
  deletePlayer,
  insertGame,
  getGame,
  listGames,
  updateGameStatus,
  deleteGame,
  insertMove,
  insertMoves,
  getMove,
  getMovesForGame,
  getLatestMoveForGame,
  getMoveCountForGame,
  deleteMove,
  createGameParticipantToken,
  getTokenByText,
  getActiveTokenByGameAndRole,
  insertPendingAction,
  getPendingAction,
  deletePendingAction,
  deleteLastNMoves,

  -- * Effect
  Storage (..),
  runTransaction,
) where

import Chronos (Time)
import Effectful
import Effectful.Dispatch.Dynamic
import Hnefatafl.Core.Data
import Hnefatafl.Game.Common (PendingAction)

--------------------------------------------------------------------------------
-- Command algebra

data StorageCmd a where
  InsertHumanPlayer :: HumanPlayer -> StorageCmd ()
  GetHumanPlayer :: PlayerId -> StorageCmd HumanPlayer
  HumanPlayerFromName :: Text -> StorageCmd (Maybe HumanPlayer)
  InsertEnginePlayer :: EnginePlayer -> StorageCmd ()
  GetEnginePlayer :: PlayerId -> StorageCmd EnginePlayer
  GetPlayer :: PlayerId -> StorageCmd Player
  DeletePlayer :: PlayerId -> StorageCmd ()
  InsertGame :: Game -> StorageCmd ()
  GetGame :: GameId -> StorageCmd Game
  ListGames :: StorageCmd [Game]
  UpdateGameStatus :: GameId -> GameStatus -> Maybe Time -> StorageCmd ()
  DeleteGame :: GameId -> StorageCmd ()
  InsertMove :: GameId -> GameMove -> StorageCmd ()
  InsertMoves :: GameId -> [GameMove] -> StorageCmd ()
  GetMove :: GameId -> Int -> StorageCmd GameMove
  GetMovesForGame :: GameId -> StorageCmd [GameMove]
  GetLatestMoveForGame :: GameId -> StorageCmd (Maybe GameMove)
  GetMoveCountForGame :: GameId -> StorageCmd Int
  DeleteMove :: GameId -> Int -> StorageCmd ()
  CreateGameParticipantToken :: GameParticipantToken -> StorageCmd ()
  GetTokenByText :: Text -> StorageCmd (Maybe GameParticipantToken)
  GetActiveTokenByGameAndRole ::
    GameId -> PlayerColor -> StorageCmd (Maybe GameParticipantToken)
  InsertPendingAction :: GameId -> PendingAction -> Time -> StorageCmd ()
  GetPendingAction :: GameId -> StorageCmd (Maybe PendingAction)
  DeletePendingAction :: GameId -> StorageCmd ()
  DeleteLastNMoves :: GameId -> Int -> StorageCmd ()

--------------------------------------------------------------------------------
-- Transaction monad

data StorageTx a where
  PureTx :: a -> StorageTx a
  BindTx :: StorageCmd b -> (b -> StorageTx a) -> StorageTx a

instance Functor StorageTx where
  fmap f (PureTx a) = PureTx (f a)
  fmap f (BindTx cmd k) = BindTx cmd (fmap f . k)

instance Applicative StorageTx where
  pure = PureTx
  mf <*> ma = mf >>= \f -> ma >>= \a -> pure (f a)

instance Monad StorageTx where
  PureTx a >>= f = f a
  BindTx cmd k >>= f = BindTx cmd (k >=> f)

liftCmd :: StorageCmd a -> StorageTx a
liftCmd cmd = BindTx cmd PureTx

--------------------------------------------------------------------------------
-- Smart constructors

insertHumanPlayer :: HumanPlayer -> StorageTx ()
insertHumanPlayer = liftCmd . InsertHumanPlayer

getHumanPlayer :: PlayerId -> StorageTx HumanPlayer
getHumanPlayer = liftCmd . GetHumanPlayer

humanPlayerFromName :: Text -> StorageTx (Maybe HumanPlayer)
humanPlayerFromName = liftCmd . HumanPlayerFromName

insertEnginePlayer :: EnginePlayer -> StorageTx ()
insertEnginePlayer = liftCmd . InsertEnginePlayer

getEnginePlayer :: PlayerId -> StorageTx EnginePlayer
getEnginePlayer = liftCmd . GetEnginePlayer

getPlayer :: PlayerId -> StorageTx Player
getPlayer = liftCmd . GetPlayer

deletePlayer :: PlayerId -> StorageTx ()
deletePlayer = liftCmd . DeletePlayer

insertGame :: Game -> StorageTx ()
insertGame = liftCmd . InsertGame

getGame :: GameId -> StorageTx Game
getGame = liftCmd . GetGame

listGames :: StorageTx [Game]
listGames = liftCmd ListGames

updateGameStatus :: GameId -> GameStatus -> Maybe Time -> StorageTx ()
updateGameStatus gid s t = liftCmd $ UpdateGameStatus gid s t

deleteGame :: GameId -> StorageTx ()
deleteGame = liftCmd . DeleteGame

insertMove :: GameId -> GameMove -> StorageTx ()
insertMove gid m = liftCmd $ InsertMove gid m

insertMoves :: GameId -> [GameMove] -> StorageTx ()
insertMoves gid ms = liftCmd $ InsertMoves gid ms

getMove :: GameId -> Int -> StorageTx GameMove
getMove gid n = liftCmd $ GetMove gid n

getMovesForGame :: GameId -> StorageTx [GameMove]
getMovesForGame = liftCmd . GetMovesForGame

getLatestMoveForGame :: GameId -> StorageTx (Maybe GameMove)
getLatestMoveForGame = liftCmd . GetLatestMoveForGame

getMoveCountForGame :: GameId -> StorageTx Int
getMoveCountForGame = liftCmd . GetMoveCountForGame

deleteMove :: GameId -> Int -> StorageTx ()
deleteMove gid n = liftCmd $ DeleteMove gid n

createGameParticipantToken :: GameParticipantToken -> StorageTx ()
createGameParticipantToken = liftCmd . CreateGameParticipantToken

getTokenByText :: Text -> StorageTx (Maybe GameParticipantToken)
getTokenByText = liftCmd . GetTokenByText

getActiveTokenByGameAndRole ::
  GameId -> PlayerColor -> StorageTx (Maybe GameParticipantToken)
getActiveTokenByGameAndRole gid role = liftCmd $ GetActiveTokenByGameAndRole gid role

insertPendingAction :: GameId -> PendingAction -> Time -> StorageTx ()
insertPendingAction gid pa t = liftCmd $ InsertPendingAction gid pa t

getPendingAction :: GameId -> StorageTx (Maybe PendingAction)
getPendingAction = liftCmd . GetPendingAction

deletePendingAction :: GameId -> StorageTx ()
deletePendingAction = liftCmd . DeletePendingAction

deleteLastNMoves :: GameId -> Int -> StorageTx ()
deleteLastNMoves gid n = liftCmd $ DeleteLastNMoves gid n

--------------------------------------------------------------------------------
-- Effectful effect

data Storage :: Effect where
  RunTransaction :: StorageTx a -> Storage m a

type instance DispatchOf Storage = 'Dynamic

runTransaction :: (HasCallStack, Storage :> es) => StorageTx a -> Eff es a
runTransaction tx = send (RunTransaction tx)
