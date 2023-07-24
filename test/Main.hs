{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Board.Board
import Board.Move
import Data.Bits
import Data.Vector qualified as BV
import Data.Vector.Unboxed qualified as V
import Data.WideWord.Word128
import GHC.Conc

simpleCapture =
  [b|
 .  .  X  .  X  O  .  .  O  .  .
 .  .  O  .  .  .  .  .  X  .  .
 X  O  .  .  .  .  .  .  .  X  O
 .  .  O  .  .  .  .  .  X  .  .
 .  .  X  .  .  .  .  .  O  .  .
 X  .  .  .  .  X  O  .  .  .  O
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

startBoard' =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  O  .  .  X  X
 X  .  .  .  O  O  O  .  .  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  .  .  .  O  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

escapeBoard =
  [b|
 .  .  .  X  X  X  X  X  .  .  #
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  .  O  .  .  X  X
 X  .  .  .  O  O  O  .  .  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  .  .  .  O  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

kingTest =
  [b|
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  #  O  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
|]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- putStrLn $ printBoard (setBit 0 60)
  -- putStrLn $ printMoves $ pieceMoves 0 60
  -- putStrLn $ printMoves $ pieceMoves 0 71
  -- putStrLn $ printMoves $ pieceMoves 0 49
  -- putStrLn $ printMoves $ pieceMoves 0 59
  -- putStrLn $ printMoves $ pieceMoves 0 61
  -- print $ northMoves 0 0
  -- putStrLn $ showWord128Board $ captures simpleCapture.blackPawns simpleCapture.whitePawns 90
  -- putStrLn ""
  -- putStrLn $ showWord128Board $ captures simpleCapture.whitePawns simpleCapture.blackPawns 96
  -- putStrLn $ show $ captures simpleCapture White 96 -- 3
  -- putStrLn $ show $ captures simpleCapture Black 45 -- []
  -- putStrLn $ show $ captures simpleCapture White 53 -- []
  -- putStrLn $ show $ southMoves (setBit 0 27) 60
  -- putStrLn $ show $ eastMoves (setBit 0 27) 60
  -- putStrLn $ show $ westMoves (setBit 0 27) 16
  -- putStrLn "Test suite not yet implemented."
  -- putStrLn ""
  -- putStrLn $ printFullBoard startBoard
  -- let bm = blackMoves startBoard
  -- showAllMoves bm
  -- let blackBoards = nextBoardsBlack startBoard
  -- showAllBoards blackBoards
  -- let whiteBoards = nextBoardsWhite startBoard
  -- showAllBoards whiteBoards
  -- let kingBoards = nextBoardsWhite kingTest
  -- showAllBoards kingBoards
  -- print numCapabilities
  -- print $ nextBoards4Sum startBoard
  -- print $ northMoveCount 0 41
  -- print $ whiteMoveCount startBoard
  -- print $ score White startBoard
  -- print $ whiteMoveCount startBoard'
  -- print $ score White startBoard'
  -- print $ score White escapeBoard
  -- print $ score Black escapeBoard
  putStrLn $ showBoard $ (\(_, b, _, _) -> b) $ minimax startBoard Black 2
  pure ()
