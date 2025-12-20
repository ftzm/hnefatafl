module Hnefatafl.Board (printBoard, moveResultToArray, formatMoveResult, formatGameMove) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits (FiniteBits (..), (.&.))
import Data.List (zipWith3)
import Data.List.Split (chunksOf)
import Data.Text (intercalate)
import Hnefatafl.Core.Data (
  ExternBoard (..),
  GameMove (..),
  Layer (..),
  Move (..),
  MoveResult (..),
 )
import Text.Printf (printf)
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
  Dest p -> "[" <> formatPiece p <> "]"
 where
  formatPiece = \case
    Black -> "X"
    White -> "O"
    King -> "#"

-- | Arrange the pieces into a formatted 11x11 board grid
formatSquares :: Array Int Square -> Text
formatSquares arr = top <> intercalate "\n" labeledRows <> bottom <> bottomLabels
 where
  rows :: [Text] = map mconcat . chunksOf 11 . map formatElem $ reverse $ elems arr
  rankLabels :: [Text] = map (toText @String . printf "%2d  |") [11 :: Integer, 10 ..]
  labeledRows = zipWith3 (\a b c -> a <> b <> c) rankLabels rows $ repeat "|"
  top = "    +---------------------------------+\n"
  bottom = "\n    +---------------------------------+"
  bottomLabels = "\n      a  b  c  d  e  f  g  h  i  j  k  \n"

printBoard :: ExternBoard -> Text
printBoard b = formatSquares $ runST $ freeze =<< boardToArray b

formatMoveResult :: MoveResult -> Text
formatMoveResult = formatSquares . moveResultToArray

-- | Format a GameMove with move and capture highlighting
formatGameMove :: GameMove -> Text
formatGameMove gameMove = formatSquares $ gameMoveToBoardArray gameMove
 where
  gameMoveToBoardArray :: GameMove -> Array Int Square
  gameMoveToBoardArray gm = runST $ do
    arr <- boardToArray gm.boardStateAfter
    -- Mark captures
    traverse_ (\i -> writeArray arr i Captured) (layerPositions gm.captures)
    -- Mark move origin and destination
    _ <- writeArray arr (fromIntegral gm.move.orig) Orig
    _ <- updateMArrayIndex arr (fromIntegral gm.move.dest) toArrival
    freeze arr

  toArrival s = case s of
    Static p -> Dest p
    other -> other
