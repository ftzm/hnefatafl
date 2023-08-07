{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Board.Move where

import Board.Board (
  Board (..),
  Move (..),
  Moves (..),
  PieceType (..),
  testBoardBit,
 )
import Board.Constant (pawnIllegalDestinations)
import Board.Zobrist (MultiZobrist, updateMultiZobrist)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Vector.Unboxed qualified as V
import Data.WideWord (Word128 (..))
import Foreign.C.Types (CInt, CULong (..))
import Foreign.ForeignPtr (
  FinalizerPtr,
  newForeignPtr,
  withForeignPtr,
 )
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek))
import Foreign.Storable.Tuple ()
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Generate moves from a square

northMoves ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  V.Vector Int8 -- moves
northMoves !occ !orig =
  V.unfoldrN
    (fromIntegral $ 10 - div orig 11)
    ( \i ->
        let j = i + 11
         in if not (testBoardBit occ j)
              then Just (j, j)
              else Nothing
    )
    orig

southMoves ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  V.Vector Int8 -- moves
southMoves !occ !orig =
  V.unfoldrN
    (fromIntegral $ div orig 11)
    ( \i ->
        let j = i - 11
         in if not (testBit occ $ fromIntegral j)
              then Just (j, j)
              else Nothing
    )
    orig

eastMoves ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  V.Vector Int8 -- moves
eastMoves !occ !orig =
  V.unfoldrN
    (fromIntegral $ 10 - mod orig 11)
    ( \prev ->
        let j = prev + 1
         in if not (testBit occ $ fromIntegral j)
              then Just (j, j)
              else Nothing
    )
    orig

westMoves ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  V.Vector Int8 -- moves
westMoves !occ !orig =
  V.unfoldrN
    (fromIntegral $ mod orig 11)
    ( \prev ->
        let j = prev - 1
         in if not (testBit occ $ fromIntegral j)
              then Just (j, j)
              else Nothing
    )
    orig

pieceMoves ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  V.Vector Int8 -- moves
pieceMoves occ orig =
  northMoves occ orig
    V.++ southMoves occ orig
    V.++ eastMoves occ orig
    V.++ westMoves occ orig

--------------------------------------------------------------------------------
-- Capture

captures :: Word128 -> Word128 -> Int8 -> Word128
captures !friendBoard !foeBoard !dest =
  let
    modDest = mod dest 11
    withNorthCapture x =
      let !target = dest + 11
       in if dest < 99
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest + 22)
            then setBit x $ fromIntegral target
            else x
    withSouthCapture x =
      let !target = dest - 11
       in if dest > 23
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest - 22)
            then setBit x $ fromIntegral target
            else x
    withEastCapture x =
      let !target = dest + 1
       in if modDest < 8
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest + 2)
            then setBit x $ fromIntegral target
            else x
    withWestCapture x =
      let !target = dest - 1
       in if modDest > 2
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest - 2)
            then setBit x $ fromIntegral target
            else x
   in
    withNorthCapture . withSouthCapture . withEastCapture . withWestCapture $ 0

captures' :: Word128 -> Word128 -> Int8 -> [Int8]
captures' !friendBoard !foeBoard !dest =
  let
    modDest = mod dest 11
    withNorthCapture x =
      let !target = dest + 11
       in if dest < 99
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest + 22)
            then target : x
            else x
    withSouthCapture x =
      let !target = dest - 11
       in if dest > 23
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest - 22)
            then target : x
            else x
    withEastCapture x =
      let !target = dest + 1
       in if modDest < 8
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest + 2)
            then target : x
            else x
    withWestCapture x =
      let !target = dest - 1
       in if modDest > 2
            && testBit foeBoard (fromIntegral target)
            && testBit friendBoard (fromIntegral $ dest - 2)
            then target : x
            else x
   in
    withNorthCapture . withSouthCapture . withEastCapture . withWestCapture $ []

--------------------------------------------------------------------------------
-- Generate moves for a team for a given board

blackMoves :: Board -> V.Vector (Int8, Int8)
blackMoves Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.concatMap (\i -> V.map (i,) (pieceMoves occ i)) $
        V.filter (testBit blackPawns . fromIntegral) $
          V.fromList [0 .. 120]

blackMoves' :: Board -> [(Int8, Int8)]
blackMoves' Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in map (\(Move orig dest) -> (orig, dest)) $ unMoves $ teamMoves blackPawns occ

whiteMoves :: Board -> V.Vector (Int8, Int8)
whiteMoves Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.concatMap (\i -> V.map (i,) (pieceMoves occ i)) $
        V.filter (testBit whitePawns . fromIntegral) $
          V.fromList [0 .. 120]

whiteMoves' :: Board -> [(Int8, Int8)]
whiteMoves' Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in map (\(Move orig dest) -> (orig, dest)) $ unMoves $ teamMoves whitePawns occ

kingMoves :: Board -> V.Vector (Int8, Int8)
kingMoves Board{whitePawns, king, blackPawns} =
  V.map (fromIntegral $ popCount king,) $
    pieceMoves (whitePawns .|. blackPawns) $
      fromIntegral $
        countTrailingZeros king

kingMoves' :: Board -> [(Int8, Int8)]
kingMoves' Board{whitePawns, king, blackPawns} =
  map (fromIntegral $ popCount king,) $
    V.toList $
      pieceMoves (whitePawns .|. blackPawns) $
        fromIntegral $
          countTrailingZeros king

--------------------------------------------------------------------------------
-- Generate baords

applyMoveBlack' :: Board -> Word128 -> (Int8, Int8) -> Board
applyMoveBlack' board opps (orig, dest) =
  let
    departedBlacks = clearBit board.blackPawns $ fromIntegral orig
    capturedSquares = captures departedBlacks opps dest
   in
    board
      { blackPawns = setBit departedBlacks $ fromIntegral dest
      , whitePawns = xor board.whitePawns capturedSquares
      }

applyMoveBlack :: Board -> (Int8, Int8) -> Board
applyMoveBlack board (orig, dest) =
  let
    opps = board.whitePawns .|. board.king
    departedBlacks = clearBit board.blackPawns $ fromIntegral orig
    capturedSquares = captures departedBlacks opps dest
   in
    board
      { blackPawns = setBit departedBlacks $ fromIntegral dest
      , whitePawns = xor board.whitePawns capturedSquares
      }

nextBoardsBlack :: Board -> V.Vector Board
nextBoardsBlack board =
  let
    ms = blackMoves board
    opps = board.whitePawns .|. board.king
    applyChanges :: Int8 -> Int8 -> Board
    applyChanges orig dest =
      let
        departedBlacks = clearBit board.blackPawns $ fromIntegral orig
        capturedSquares = captures departedBlacks opps dest
       in
        board
          { blackPawns = setBit departedBlacks $ fromIntegral dest
          , whitePawns = xor board.whitePawns capturedSquares
          }
   in
    V.map (uncurry applyChanges) ms

nextMoveBoardsBlack :: Board -> V.Vector ((Int8, Int8), Board)
nextMoveBoardsBlack board =
  let
    ms = blackMoves board
    opps = board.whitePawns .|. board.king
    applyChanges :: Int8 -> Int8 -> Board
    applyChanges orig dest =
      let
        departedBlacks = clearBit board.blackPawns $ fromIntegral orig
        capturedSquares = captures departedBlacks opps dest
       in
        board
          { blackPawns = setBit departedBlacks $ fromIntegral dest
          , whitePawns = xor board.whitePawns capturedSquares
          }
   in
    V.map (\m -> (m, uncurry applyChanges m)) ms

nextMoveBoardsBlack' :: Board -> [((Int8, Int8), Board)]
nextMoveBoardsBlack' board =
  let
    ms = blackMoves' board
    opps = board.whitePawns .|. board.king
    applyChanges :: Int8 -> Int8 -> Board
    applyChanges orig dest =
      let
        departedBlacks = clearBit board.blackPawns $ fromIntegral orig
        capturedSquares = captures departedBlacks opps dest
       in
        board
          { blackPawns = setBit departedBlacks $ fromIntegral dest
          , whitePawns = xor board.whitePawns capturedSquares
          }
   in
    map (\m -> (m, uncurry applyChanges m)) ms

nextMoveBoardsBlackZ ::
  Board ->
  MultiZobrist ->
  [((Int8, Int8), Board, MultiZobrist)]
nextMoveBoardsBlackZ board zobrist =
  let
    ms = blackMoves' board
    opps = board.whitePawns .|. board.king
    applyChanges :: Int8 -> Int8 -> ((Int8, Int8), Board, MultiZobrist)
    applyChanges orig dest =
      let
        departedBlacks = clearBit board.blackPawns $ fromIntegral orig
        capturedSquares = captures' departedBlacks opps dest
        updatedBoard =
          board
            { blackPawns = setBit departedBlacks $ fromIntegral dest
            , whitePawns = xor board.whitePawns $ foldl' (\acc x -> setBit acc $ fromIntegral x) 0 capturedSquares
            }
        zPieces =
          (BlackType, dest)
            : (BlackType, orig)
            : map (WhiteType,) capturedSquares
        updatedZobrist = updateMultiZobrist zPieces zobrist
       in
        ((orig, dest), updatedBoard, updatedZobrist)
   in
    map (uncurry applyChanges) ms

applyMoveWhitePawn :: Board -> (Int8, Int8) -> Board
applyMoveWhitePawn board (orig, dest) =
  let
    opps = board.blackPawns
    departed = clearBit board.whitePawns $ fromIntegral orig
    capturedSquares = captures departed opps dest
   in
    board
      { whitePawns = setBit departed $ fromIntegral dest
      , blackPawns = xor board.blackPawns capturedSquares
      }

applyMoveKing :: Board -> Int8 -> Board
applyMoveKing board dest =
  let
    opps = board.blackPawns
   in
    board
      { king = setBit 0 $ fromIntegral dest
      , blackPawns = xor board.blackPawns $ captures 0 opps dest
      }

nextBoardsWhite :: Board -> V.Vector Board
nextBoardsWhite board =
  let
    opps = board.blackPawns
    pawnMs = whiteMoves board
    applyPawnChanges :: Int8 -> Int8 -> Board
    applyPawnChanges orig dest =
      let
        departed = clearBit board.whitePawns $ fromIntegral orig
        capturedSquares = captures departed opps dest
       in
        board
          { whitePawns = setBit departed $ fromIntegral dest
          , blackPawns = xor board.blackPawns capturedSquares
          }
    kingChanges :: V.Vector Board
    kingChanges =
      let
        arriveAndCapture dest =
          board
            { king = setBit 0 $ fromIntegral dest
            , blackPawns = xor board.blackPawns $ captures 0 opps dest
            }
       in
        V.map (arriveAndCapture . snd) $ V.convert $ kingMoves board
   in
    kingChanges V.++ V.map (uncurry applyPawnChanges) pawnMs

nextMoveBoardsWhite :: Board -> V.Vector ((Int8, Int8), Board)
nextMoveBoardsWhite board =
  let
    opps = board.blackPawns
    pawnMs = whiteMoves board
    applyPawnChanges :: Int8 -> Int8 -> Board
    applyPawnChanges orig dest =
      let
        departed = clearBit board.whitePawns $ fromIntegral orig
        capturedSquares = captures departed opps dest
       in
        board
          { whitePawns = setBit departed $ fromIntegral dest
          , blackPawns = xor board.blackPawns capturedSquares
          }
    kingChanges :: V.Vector ((Int8, Int8), Board)
    kingChanges =
      let
        arriveAndCapture dest =
          board
            { king = setBit 0 $ fromIntegral dest
            , blackPawns = xor board.blackPawns $ captures 0 opps dest
            }
       in
        V.map (\m -> (m, arriveAndCapture . snd $ m)) $ V.convert $ kingMoves board
   in
    kingChanges V.++ V.map (\m -> (m, uncurry applyPawnChanges m)) pawnMs

nextMoveBoardsWhite' :: Board -> [((Int8, Int8), Board)]
nextMoveBoardsWhite' board =
  let
    opps = board.blackPawns
    pawnMs = whiteMoves' board
    applyPawnChanges :: Int8 -> Int8 -> Board
    applyPawnChanges orig dest =
      let
        departed = clearBit board.whitePawns $ fromIntegral orig
        capturedSquares = captures departed opps dest
       in
        board
          { whitePawns = setBit departed $ fromIntegral dest
          , blackPawns = xor board.blackPawns capturedSquares
          }
    kingChanges :: [((Int8, Int8), Board)]
    kingChanges =
      let
        arriveAndCapture dest =
          board
            { king = setBit 0 $ fromIntegral dest
            , blackPawns = xor board.blackPawns $ captures 0 opps dest
            }
       in
        map (\m -> (m, arriveAndCapture . snd $ m)) $ kingMoves' board
   in
    kingChanges ++ map (\m -> (m, uncurry applyPawnChanges m)) pawnMs

nextMoveBoardsWhiteZ ::
  Board ->
  MultiZobrist ->
  [((Int8, Int8), Board, MultiZobrist)]
nextMoveBoardsWhiteZ board zobrist =
  let
    opps = board.blackPawns
    pawnMs = whiteMoves' board
    applyPawnChanges :: Int8 -> Int8 -> ((Int8, Int8), Board, MultiZobrist)
    applyPawnChanges orig dest =
      let
        departed = clearBit board.whitePawns $ fromIntegral orig
        capturedSquares = captures' departed opps dest
        updatedBoard =
          board
            { whitePawns = setBit departed $ fromIntegral dest
            , blackPawns = xor board.blackPawns $ foldl' (\acc x -> setBit acc $ fromIntegral x) 0 capturedSquares
            }
        zPieces =
          (WhiteType, dest)
            : (WhiteType, orig)
            : map (BlackType,) capturedSquares
        updatedZobrist = updateMultiZobrist zPieces zobrist
       in
        ((orig, dest), updatedBoard, updatedZobrist)
    kingMs = kingMoves' board
    applyKingChanges :: Int8 -> Int8 -> ((Int8, Int8), Board, MultiZobrist)
    applyKingChanges orig dest =
      let
        capturedSquares = captures board.whitePawns opps dest
        updatedBoard =
          board
            { king = setBit 0 $ fromIntegral dest
            , blackPawns = xor board.blackPawns capturedSquares
            }
        zPieces =
          (KingType, dest)
            : (KingType, orig)
            : [(BlackType, i) | i <- [0 .. 120], testBoardBit capturedSquares i]
        updatedZobrist = updateMultiZobrist zPieces zobrist
       in
        ((orig, dest), updatedBoard, updatedZobrist)
   in
    map (uncurry applyKingChanges) kingMs
      ++ map (uncurry applyPawnChanges) pawnMs

-- haskell compact library may be good for reducing gc overhead on long-lived
-- lookup tables

-- first, use bit shifting and masking to generate a board of potentially
-- vulnerable squares.

-- then we iterate over this board, the team board and the opp boards in
-- parallel, and as we go we can keep a running record of the latest
-- piece+color in a given row/column.

-- If we imagine that we start iterating at the top left and work our way left
-- to right and down, by the time we encounter a vulnerable square we'll know
-- if there are opposition pieces in the north and west that can attack that
-- piece.

-- To handle the south and east the easiest thing to do would be to repeat the
-- above iteration but go bottom up this time. there may also be a smarter
-- method for this.

-- I think there may also be a similar method for move generation, could be
-- worth trying and benchmarking.

--------------------------------------------------------------------------------
-- C interop

-- Generic free function; likely must be used with castFunPtr
foreign import ccall unsafe "stdlib.h &free" c_free_ptr :: FinalizerPtr CInt

foreign import ccall unsafe "&free_team_moves" c_free_team_moves :: FinalizerPtr Moves

foreign import ccall "team_moves" c_team_moves :: CULong -> CULong -> CULong -> CULong -> IO (Ptr Moves)

teamMoves :: Word128 -> Word128 -> Moves
{-# NOINLINE teamMoves #-}
teamMoves team occ =
  unsafePerformIO $
    c_team_moves
      (fromIntegral team.word128Hi64)
      (fromIntegral team.word128Lo64)
      (fromIntegral occ.word128Hi64)
      (fromIntegral occ.word128Lo64)
      >>= newForeignPtr c_free_team_moves
      >>= flip withForeignPtr peek
