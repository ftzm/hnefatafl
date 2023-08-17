{-# LANGUAGE DeriveAnyClass #-}

module Command where

import Board.Board (PieceType, Board)
import Data.Aeson (FromJSON, ToJSON)

data IndexedMove = IndexedMove {pieceType :: PieceType, dest :: Int}
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Move = Move {pieceType :: PieceType, orig :: Int, dest :: Int}
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data Command
  = SeeMoves Int
  | UnSeeMoves
  | MakeMove Move
  | CommitAiMove Board
  | Concede
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
