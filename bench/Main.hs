{-# LANGUAGE QuasiQuotes #-}

module Main where

import Criterion
import Criterion.Main
import GHC.Conc

import Board.Board
import Board.Move
import Data.Bits (Bits (..))
import Data.Vector qualified as BV
import Data.Vector.Unboxed qualified as V
import Data.WideWord (Word128)
import AI.Assessment
import AI.NegamaxABZ

simpleCapture =
  [b|
 .  .  X  .  X  O  .  .  O  .  .
 .  .  O  .  .  .  .  .  X  .  .
 X  O  .  .  .  .  .  .  .  X  O
 .  .  O  .  .  .  .  .  X  .  .
 #  .  X  .  .  .  .  .  O  .  .
 X  .  .  .  .  .  O  .  .  .  O
 O  .  .  .  .  .  .  .  .  .  X
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  X  O  .  .  .  .  .
|]

startBoard =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  O  O  .  X  X
 X  .  .  .  O  O  O  .  .  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

main = do
  print numCapabilities
  defaultMain
    [ bgroup
        "captures"
        [ bench "captures" $ nf (captures simpleCapture.blackPawns simpleCapture.whitePawns) 90
        , bench "2" $ nf (northMoves 0) 1
        , bench "escape" $ nf kingEscaped startBoard
        ]
    , bgroup
        "moves"
        [ bench "northMoves" $ nf (northMoves 0) 1
        , bench "northMoveCount" $ nf (northMoveCount 0) 1
        ]
    , bgroup
        "manyMoves"
        [ bench "blackMoves" $ nf blackMoves startBoard
        , bench "nextBoardsBlack" $ nf nextBoardsBlack startBoard
        -- , bench "blackBoards" $ nf nextBoardsBlack startBoard
        -- , bench "nextBoardsBlack1" $ nf nextBoardsBlack1 startBoard
        -- , bench "nextBoardsBlack2" $ nf nextBoardsBlack2 startBoard
        -- , bench "nextBoardsBlack3" $ nf nextBoardsBlack3 startBoard
        -- , bench "nextBoardsBlack4" $ nf nextBoardsBlack4 startBoard
        -- , bench "nextBoards4Sum" $ nf nextBoards4Sum startBoard
        -- , bench "nextBoards5Sum" $ nf nextBoards5Sum startBoard
        ]
    , bgroup
        "negamaxABZ"
        [ bench "current" $ nfIO $ runSearch simpleCapture Black
        -- , bench "candidate" $ nfIO $ runSearch' simpleCapture Black
        -- , bench "candidate 2" $ nfIO $ runSearch'' simpleCapture Black
        ]
    ]
