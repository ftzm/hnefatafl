{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Prelude hiding (fromString)
import Data.UUID (fromString, UUID)
import Test.QuickCheck
import Board.Board
import Board.Move
import Data.Bits
import Data.Vector qualified as BV
import Data.Vector.Unboxed qualified as V
import Data.WideWord.Word128
import GHC.Conc
import Control.Exception
import AI.NegamaxABZ
import Database.SQLite.Simple
import Data.Maybe (fromJust)
import DB.Game

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

shouldCapture =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  O  O  .  X  X
 X  .  .  .  O  O  O  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 .  .  .  .  .  O  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

shouldCapture2 =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  O  .  .  .  O  O  .  .  .  X
 X  X  .  O  O  #  O  O  .  X  X
 X  .  .  .  O  O  O  .  .  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  X  .  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  .  X  X  X  .  .  .
|]

response =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  O  O  .  X  X
 X  .  .  .  O  O  O  .  .  .  X
 X  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

testBoard =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  .  O  .  X  X
 .  .  X  .  O  O  .  .  O  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  X  .  .  .  .  .  .
 .  .  .  .  .  X  O  .  .  .  .
 .  .  .  X  .  X  X  X  .  .  .
|]

sameP1 =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  O  O  .  X  X
 X  .  O  .  .  O  O  .  .  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

sameP2 =
  [b|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  O  .  .  .  .  X
 X  .  .  .  O  O  O  .  .  .  X
 X  X  .  O  O  #  O  O  .  X  X
 X  .  .  .  O  O  .  .  .  .  X
 X  .  .  .  .  O  .  .  .  .  X
 .  .  .  .  .  .  O  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

--------------------------------------------------------------------------------

-- prop_teamMovesCEqual :: Word128 -> Word128 -> Bool
-- prop_teamMovesCEqual team occ =
--   cTeamMoveCount team occ ==

-- prop_teamMovesCEqual :: Word128 -> Word128 -> Bool

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

  -- let result = minimax startBoard Black 3
  -- putStrLn $ showBoard $ board result
  -- putStrLn $ show $ tally result

  -- let resultAB = alphaBeta (0,0) [startBoard] Black Black 4 0 (-1000000) (1000000)
  -- mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ resultAB.board
  -- putStrLn $ "tally" <> (show resultAB.tally)
  -- putStrLn $ "score" <> (show resultAB.score)

  -- resultABneg <- negamaxAB (0,0) [startBoard] Black 4 0 (-1000000) (1000000)
  -- mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ resultABneg.board
  -- putStrLn $ "tally: " <> (show resultABneg.tally)
  -- putStrLn $ "score: " <> (show resultABneg.score)

  -- let capP = negamaxAB (0,0) [startBoard] Black 3 0 (minBound + 10) (maxBound - 10)
  -- mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ board capP
  -- putStrLn $ "tally" <> (show $ tally capP)
  -- putStrLn $ "score" <> (show $ score capP)

  -- capP <- negamaxAB (0,0) [startBoard] Black 5 0 (minBound + 10) (maxBound - 10)
  -- mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ board capP
  -- putStrLn $ "tally" <> (show $ tally capP)
  -- putStrLn $ "score" <> (show $ score capP)

  -- let capP = negamaxAB (0,0) [response] White 3 0 (minBound + 10) (maxBound - 10)
  -- mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ board capP
  -- putStrLn $ "tally" <> (show $ tally capP)
  -- putStrLn $ "score" <> (show $ score capP)

  -- print $ teamMoves (setBit 0 10) 0

  -- (result, stats) <- runSearch startBoard Black
  -- mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ result.board
  -- print stats
  -- putStrLn $ "tally" <> (show result.tally)
  -- putStrLn $ "score" <> (show result.score)

  -- putStrLn $ showBoard testBoard
  -- print $ scoreBoard Black testBoard
  -- putStrLn ""
  -- putStrLn $ showBoard $ rotateBoard90 testBoard
  -- print $ scoreBoard Black $ rotateBoard90 testBoard
  -- putStrLn ""
  -- putStrLn $ showBoard $ rotateBoard180 testBoard
  -- print $ scoreBoard Black $ rotateBoard180 testBoard
  -- putStrLn ""
  -- putStrLn $ showBoard $ rotateBoard270 testBoard
  -- print $ scoreBoard Black $ rotateBoard270 testBoard

  (result, stats) <- runSearch startBoard Black
  mapM_ ((\b -> putStrLn "" >> putStrLn b) . showBoard) $ reverse $ result.board
  print stats
  putStrLn $ "tally: " <> (show result.tally)
  putStrLn $ "score: " <> (show result.score)

  -- pure ()

  -- -- db test
  -- conn <- open "db.db"
  -- let id1 :: UUID = fromJust $ fromString "1e966faf-4de6-470c-9821-34f3341c9d74"
  -- let game = Game id1 testBoard True
  -- let hotseat = Hotseat id1
  -- --insertGame conn game
  -- --insertHotseat conn hotseat
  -- outg <- selectGame conn id1
  -- ouths <- selectHotseat conn id1
  -- putStrLn $ showBoard outg.board
  -- putStrLn $ showBoard ouths.board

  -- _ <- updateBoard conn id1 startBoard

  -- ouths' <- selectHotseat conn id1
  -- putStrLn $ showBoard ouths'.board
