{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf #-}

module AI.Assessment where

import Board.Board (Board (..), Team (..), testBoardBit)
import Board.Constant (pawnIllegalDestinations)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.WideWord (Word128 (..))
import Foreign.C (CInt (..), CULong (..))
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

scoreBoard :: Team -> Board -> Int
scoreBoard t b =
  let
    (whiteMod, blackMod) = case t of
      White -> (id, negate)
      Black -> (negate, id)
    whitePoints =
      sum
        [ whitePieceCount b * 1000
        , whiteMoveCount b
        , kingMoveCount b * 100
        ]
    blackPoints =
      sum
        [ blackPieceCount b * 1000
        , blackMoveCount b
        ]
   in
    if
        | kingEscaped b -> whiteMod 1000000
        | kingCaptured b -> blackMod 1000000
        | otherwise -> whiteMod whitePoints + blackMod blackPoints

--------------------------------------------------------------------------------
-- Win Conditions

kingEscaped :: Board -> Bool
kingEscaped b =
  let kingIndex = countTrailingZeros b.king
   in kingIndex == 0 || kingIndex == 10 || kingIndex == 110 || kingIndex == 120

kingCaptured :: Board -> Bool
kingCaptured b =
  let
    kingIndex = countTrailingZeros b.king
    surroundMask =
      setBit
        ( setBit
            ( setBit
                (setBit 0 (kingIndex + 11))
                (kingIndex - 11)
            )
            (kingIndex + 1)
        )
        (kingIndex - 1)
   in
    if
        | kingIndex > 109 -> False
        | kingIndex < 11 -> False
        | mod kingIndex 11 == 0 -> False
        | mod (kingIndex + 1) 11 == 0 -> False
        | popCount (b.blackPawns .&. surroundMask) == 4 -> True
        | otherwise -> False

whiteMovesExist :: Board -> Bool
whiteMovesExist b = whiteMoveCount b > 0

blackMovesExist :: Board -> Bool
blackMovesExist b = blackMoveCount b > 0

whiteVictory :: Board -> Bool
whiteVictory b = kingEscaped b || not (blackMovesExist b)

blackVictory :: Board -> Bool
blackVictory b = kingCaptured b || not (whiteMovesExist b)

--------------------------------------------------------------------------------

blackPieceCount :: Board -> Int
blackPieceCount b = popCount b.blackPawns

whitePieceCount :: Board -> Int
whitePieceCount b = popCount b.whitePawns

blackMoveCount :: Board -> Int
blackMoveCount Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in cTeamMoveCount blackPawns occ

whiteMoveCount :: Board -> Int
whiteMoveCount Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in cTeamMoveCount whitePawns occ

--------------------------------------------------------------------------------

northMoveCount ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  Int -- moves
northMoveCount !occ !orig =
  let
    limit = 10 - div orig 11
    go !count =
      if count == limit || testBoardBit occ (orig + (11 * (count + 1)))
        then count
        else go $ count + 1
   in
    fromIntegral $ go 0

southMoveCount ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  Int -- moves
southMoveCount !occ !orig =
  let
    limit = div orig 11
    go !count =
      if count == limit || testBoardBit occ (orig - (11 * (count + 1)))
        then count
        else go $ count + 1
   in
    fromIntegral $ go 0

eastMoveCount ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  Int -- moves
eastMoveCount !occ !orig =
  let
    limit = 10 - mod orig 11
    go !count =
      if count == limit || testBoardBit occ (orig + (count + 1))
        then count
        else go $ count + 1
   in
    fromIntegral $ go 0

westMoveCount ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  Int -- moves
westMoveCount !occ !orig =
  let
    limit = mod orig 11
    go !count =
      if count == limit || testBoardBit occ (orig - (count + 1))
        then count
        else go $ count + 1
   in
    fromIntegral $ go 0

pieceMoveCount ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  Int -- moves
pieceMoveCount occ orig =
  northMoveCount occ orig
    + southMoveCount occ orig
    + eastMoveCount occ orig
    + westMoveCount occ orig

kingMoveCount :: Board -> Int
kingMoveCount Board{whitePawns, king, blackPawns} =
  pieceMoveCount (whitePawns .|. blackPawns) $
    fromIntegral $
      countTrailingZeros king

--------------------------------------------------------------------------------
-- C interop

foreign import ccall "bump" c_bump :: CULong -> CULong -> IO CULong

bump :: Word64 -> Int
-- {-# NOINLINE bump #-}
bump b = unsafePerformIO $ do
  result <- c_bump (fromIntegral b) (fromIntegral b)
  return (fromIntegral result)

foreign import ccall "pieceMoveCount" c_northMoveCount :: CULong -> CULong -> CInt -> IO CInt

cPieceMoveCount :: Word128 -> Int -> Int
-- {-# NOINLINE cPieceMoveCount #-}
cPieceMoveCount occ pos = unsafePerformIO $ do
  result <-
    c_northMoveCount
      (fromIntegral occ.word128Hi64)
      (fromIntegral occ.word128Lo64)
      (fromIntegral pos)
  return (fromIntegral result)

foreign import ccall "teamMoveCount" c_teamMoveCount :: CULong -> CULong -> CULong -> CULong -> IO CInt

cTeamMoveCount :: Word128 -> Word128 -> Int
-- {-# NOINLINE cTeamMoveCount #-}
cTeamMoveCount team occ = unsafePerformIO $ do
  result <-
    c_teamMoveCount
      (fromIntegral team.word128Hi64)
      (fromIntegral team.word128Lo64)
      (fromIntegral occ.word128Hi64)
      (fromIntegral occ.word128Lo64)
  return (fromIntegral result)
