module Hnefatafl.Board (printBoard, moveResultToArray, printMoveResult) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits (FiniteBits (..), (.&.))
import Data.List.Split (chunksOf)
import Data.Text (intercalate)
import Hnefatafl.Core.Data (
  ExternBoard (..),
  Layer (..),
  Move (..),
  MoveResult (..),
 )
import Prelude hiding (intercalate)

clearLSB :: (FiniteBits a, Num a) => a -> a
clearLSB n = n .&. (n - 1)

setBitPositions :: (FiniteBits a, Num a) => a -> [Int]
setBitPositions = unfoldr $ \case
  0 -> Nothing
  n -> Just (countTrailingZeros n, clearLSB n)

layerPositions :: Layer -> [Int]
layerPositions Layer{lower, upper} =
  setBitPositions lower <> map (+ 64) (setBitPositions upper)

data Piece = Black | White | King
data Square = Empty | Orig | Captured | Static Piece | Dest Piece

boardToArray :: ExternBoard -> ST s (STArray s Int Square)
boardToArray b = do
  arr <- newArray (0, 120) Empty :: ST s (STArray s Int Square)
  traverse_ (\i -> writeArray arr i $ Static Black) $ layerPositions b.black
  traverse_ (\i -> writeArray arr i $ Static White) $ layerPositions b.white
  writeArray arr (fromIntegral b.king) $ Static King
  return arr

updateMArrayIndex ::
  (MArray a1 a2 m, Ix i) => a1 i a2 -> i -> (a2 -> a2) -> m (a1 i a2)
updateMArrayIndex arr i f = do
  orig <- readArray arr i
  _ <- writeArray arr i $ f orig
  return arr

moveResultToArray :: MoveResult -> Array Int Square
moveResultToArray moveResult = runST $ do
  arr <- boardToArray moveResult.board
  traverse_ (\i -> writeArray arr i Captured) capturePositions
  _ <- writeArray arr (fromIntegral moveResult.move.orig) Orig
  _ <- updateMArrayIndex arr (fromIntegral moveResult.move.dest) toArrival
  freeze arr
 where
  capturePositions = layerPositions moveResult.captures
  toArrival s = case s of
    Static p -> Dest p
    other -> other

formatElem :: Square -> Text
formatElem = \case
  Empty -> " . "
  Orig -> "[ ]"
  Captured -> " ! "
  Static p -> " " <> formatPiece p <> " "
  Dest p -> "[" <> formatPiece p <> " "
 where
  formatPiece = \case
    Black -> "X"
    White -> "O"
    King -> "#"

-- | Arrange the pieces into a formatted 11x11 board grid
formatSquares :: Array Int Square -> Text
formatSquares = intercalate "\n" . map mconcat . chunksOf 11 . map formatElem . elems

printBoard :: ExternBoard -> Text
printBoard b = formatSquares $ runST $ freeze =<< boardToArray b

printMoveResult :: MoveResult -> Text
printMoveResult = formatSquares . moveResultToArray
