{-# LANGUAGE DeriveAnyClass #-}

module UIUpdate where

-- import Data.Aeson (FromJSON, ToJSON)
-- import Command (Command)
--
-- data BoardUpdate = BoardUpdate {commands :: Map Int Command, board :: Text}
--   deriving (Generic, Show, Eq)
--   deriving anyclass (ToJSON, FromJSON)
--
-- data Resolution = BlackVictory | WhiteVictory | Draw
--   deriving (Generic, Show)
--   deriving anyclass (ToJSON, FromJSON)
--
-- data UIUpdate
--   = UpdateBoard BoardUpdate
--   | UpdateStatus Text
--   | PushMessage
--   | Alert Text
--   | AwaitingAi
--   | Error Text
--   deriving (Generic, Show, Eq)
--   deriving anyclass (ToJSON, FromJSON)
