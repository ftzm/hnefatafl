{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hnefatafl.Effect.Storage (
  module Hnefatafl.Effect.Storage,
) where

import Data.Time (UTCTime)
import Effectful
import Effectful.TH
import Hnefatafl.Core.Data

data Storage :: Effect where
  InsertHumanPlayer :: HumanPlayer -> Storage m ()
  GetHumanPlayer :: PlayerId -> Storage m HumanPlayer
  InsertEnginePlayer :: EnginePlayer -> Storage m ()
  GetEnginePlayer :: PlayerId -> Storage m EnginePlayer
  GetPlayer :: PlayerId -> Storage m Player
  DeletePlayer :: PlayerId -> Storage m ()
  InsertGame :: Game -> Storage m ()
  GetGame :: GameId -> Storage m Game
  UpdateGameStatus :: GameId -> GameStatus -> Maybe UTCTime -> Storage m ()
  DeleteGame :: GameId -> Storage m ()
  InsertMove :: GameId -> GameMove -> Storage m ()
  GetMove :: MoveId -> Storage m GameMove
  GetMovesForGame :: GameId -> Storage m [GameMove]
  GetLatestMoveForGame :: GameId -> Storage m (Maybe GameMove)
  GetMoveCountForGame :: GameId -> Storage m Int
  DeleteMove :: MoveId -> Storage m ()
  CreateGameParticipantToken :: GameParticipantToken -> Storage m ()
  GetTokenByText :: Text -> Storage m (Maybe GameParticipantToken)
  GetActiveTokenByGameAndRole :: GameId -> PlayerColor -> Storage m (Maybe GameParticipantToken)

makeEffect ''Storage