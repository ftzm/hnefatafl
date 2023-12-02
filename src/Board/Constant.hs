{-# LANGUAGE QuasiQuotes #-}

module Board.Constant where

import Board.Board
import Data.Bits (Bits (..))
import Data.WideWord (Word128(..))

corners :: Word128
corners =
  [bl|
 X  .  .  .  .  .  .  .  .  .  X
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  .  .  .  .  .  X
|]

throne :: Word128
throne =
  [bl|
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
|]

outside :: Word128
outside =
  [bl|
 X  X  X  X  X  X  X  X  X  X  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  X  X  X  X  X  X  X  X  X  X
|]

whiteAlliedSquares :: Word128
whiteAlliedSquares = corners .|. throne

pawnIllegalDestinations :: Word128
pawnIllegalDestinations = corners .|. throne

blackStart :: Word128
blackStart =
  [bl|
 .  .  .  X  X  X  X  X  .  .  .
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 X  X  .  .  .  .  .  .  .  X  X
 X  .  .  .  .  .  .  .  .  .  X
 X  .  .  .  .  .  .  .  .  .  X
 .  .  .  .  .  X  .  .  .  .  .
 .  .  .  .  .  .  .  .  .  .  .
 .  .  .  X  X  X  X  X  .  .  .
|]

startBoard :: Board
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
