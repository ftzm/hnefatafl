{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module MyLib where

{-

import Control.DeepSeq (NFData (..))
import Control.Monad
import Control.Monad.ST
import Control.Parallel
import Control.Parallel.Strategies
import Data.Bits
import Data.Bool
import Data.Data (Data)
import Data.List.Split
import Data.Vector qualified as BV
import Data.Vector.Generic qualified as GV
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as V
import Data.WideWord.Word128
import Data.Word (bitReverse64)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (liftData)
import Text.RawString.QQ


columns :: [Word128]
columns =
  let column0 = foldl' setBit 0 [0, 11 .. 110]
   in map (shiftL column0) [0 .. 11]

findColumn :: Int8 -> Int8
findColumn = flip mod 11

rows :: [Word128]
rows =
  let column0 = foldl' setBit (0 :: Word128) [0 .. 10]
   in map (shiftL column0) [0, 11 .. 110]

findRow :: Int8 -> Int8
findRow = flip div 11

printMoves :: V.Vector Int8 -> String
printMoves = printBoard . V.foldl' (\acc x -> setBit acc $ fromIntegral x) 0

blackMoves' :: Board -> BV.Vector (Int8, V.Vector Int8)
blackMoves' b =
  let
    occ = b.whitePawns .|. b.king .|. b.blackPawns
    bp = b.blackPawns
    go :: Int -> Int -> MV.MVector s (Int8, V.Vector Int8) -> ST s (MV.STVector s (Int8, V.Vector Int8))
    go 120 !i !v =
      if testBit bp 120
        then do
          v' <- MV.grow v 1
          MV.write v' i (120, pieceMoves occ 120)
          pure v'
        else pure v
    go !s !i !v =
      if testBit bp s
        then do
          v' <- MV.grow v 1
          MV.write v' i (fromIntegral s, pieceMoves occ $ fromIntegral s)
          go (s + 1) (i +1) v'
        else go (s + 1) i v
   in runST $ do
      v <- MV.new 0
      v' <- go 0 0 v
      BV.unsafeFreeze v'

showAllMoves :: V.Vector (Int8, Int8) -> IO ()
showAllMoves ms = do
  let group = V.groupBy (\a b -> fst a == fst b) ms
  forM_ group $ \moves -> do
    putStrLn "--------------"
    putStrLn $ printBoard $ (setBit 0 $ fromIntegral $ fst $ V.head moves)
    putStrLn ""
    V.forM_ moves $ \x -> do
      putStrLn $ printBoard $ setBit 0 $ fromIntegral (snd x)
      putStrLn ""

showAllBoards :: BV.Vector Board -> IO ()
showAllBoards bs =
  BV.forM_ bs $ \b -> do
    putStrLn $ printFullBoard b
    putStrLn ""

-- Use pragmas liberally to help typeclass

-- can have static lookup table for adjacent foes and backing allies locations
-- for each square (or calculate if too expensive)

-}
