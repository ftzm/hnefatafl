{-# LANGUAGE DeriveAnyClass #-}

module Event where

import Data.Aeson (FromJSON, ToJSON)
import Command (Command)

data BoardUpdate = BoardUpdate {commands :: Map Int Command, board :: Text}
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data Resolution = BlackVictory | WhiteVictory | Draw
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data Event
  = UpdateBoard BoardUpdate
  | UpdateStatus Text
  | PushMessage
  | Error Text
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
