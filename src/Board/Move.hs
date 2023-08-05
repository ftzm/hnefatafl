{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}

module Board.Move where

import Board.Board (Board (..), Move (..), Moves (..), showBoard)
import Board.Constant (pawnIllegalDestinations)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Vector qualified as BV
import Data.Vector.Hashtables as VH
import Data.Vector.Storable.Mutable qualified as VM
import Data.Vector.Unboxed qualified as V
import Data.WideWord (Word128 (..))
import Foreign.Storable.Tuple ()
import System.Random

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

--------------------------------------------------------------------------------

data Team = White | Black
  deriving (Show, Generic, Eq)
  deriving anyclass (NFData)

data PieceType = WhiteType | KingType | BlackType
  deriving (Show, Generic, Eq)
  deriving anyclass (NFData)

opp :: Team -> Team
opp White = Black
opp Black = White

--------------------------------------------------------------------------------

testBoardBit :: Word128 -> Int8 -> Bool
testBoardBit w i = testBit w $ fromIntegral i

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

pieceMoves ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  V.Vector Int8 -- moves
pieceMoves occ orig =
  northMoves occ orig
    V.++ southMoves occ orig
    V.++ eastMoves occ orig
    V.++ westMoves occ orig

pieceMoveCount ::
  Word128 -> -- occupied squares
  Int8 -> -- start square
  Int -- moves
pieceMoveCount occ orig =
  northMoveCount occ orig
    + southMoveCount occ orig
    + eastMoveCount occ orig
    + westMoveCount occ orig

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

blackMoveCount :: Board -> Int
blackMoveCount Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.sum $
        V.map (pieceMoveCount occ) $
          V.filter (testBit blackPawns . fromIntegral) $
            V.fromList [0 .. 120]

blackMoveCount' :: Board -> Int
blackMoveCount' Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in cTeamMoveCount blackPawns occ

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

whiteMoveCount :: Board -> Int
whiteMoveCount Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.sum $
        V.map (pieceMoveCount occ) $
          V.filter (testBit whitePawns . fromIntegral) $
            V.fromList [0 .. 120]

whiteMoveCount' :: Board -> Int
whiteMoveCount' Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in cTeamMoveCount whitePawns occ

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

kingMoveCount :: Board -> Int
kingMoveCount Board{whitePawns, king, blackPawns} =
  pieceMoveCount (whitePawns .|. blackPawns) $
    fromIntegral $
      countTrailingZeros king

--------------------------------------------------------------------------------
-- Generate baords

-- nextBoardsBlack :: Board -> BV.Vector Board
-- nextBoardsBlack board =
-- let
-- ms = blackMoves board
-- opps = board.whitePawns .|. board.king
-- applyChanges :: Int8 -> V.Vector Int8 -> BV.Vector Board
-- applyChanges orig dests =
-- let
-- departedBlacks = clearBit board.blackPawns $ fromIntegral orig
-- arriveAndCapture dest =
-- let
-- capturedSquares = captures departedBlacks opps dest
-- in
-- board
-- { blackPawns = setBit departedBlacks $ fromIntegral dest
-- , whitePawns = xor board.whitePawns capturedSquares
-- }
-- in
-- BV.map arriveAndCapture $ BV.convert dests
-- in
-- join $ BV.map (uncurry applyChanges) ms

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
  RotatedZobrist ->
  [((Int8, Int8), Board, RotatedZobrist)]
nextMoveBoardsBlackZ board zobrist =
  let
    ms = blackMoves' board
    opps = board.whitePawns .|. board.king
    applyChanges :: Int8 -> Int8 -> ((Int8, Int8), Board, RotatedZobrist)
    applyChanges orig dest =
      let
        departedBlacks = clearBit board.blackPawns $ fromIntegral orig
        capturedSquares = captures departedBlacks opps dest
        updatedBoard =
          board
            { blackPawns = setBit departedBlacks $ fromIntegral dest
            , whitePawns = xor board.whitePawns capturedSquares
            }
        zPieces =
          (BlackType, dest)
            : (BlackType, orig)
            : [(WhiteType, i) | i <- [0 .. 120], testBoardBit capturedSquares i]
        updatedZobrist = updateRotatedZobrist zPieces zobrist
       in
        ((orig, dest), updatedBoard, updatedZobrist)
   in
    map (uncurry applyChanges) ms

-- nextBoardsBlack'x :: Board -> V.Vector Board
-- nextBoardsBlack'x board =
-- let
-- ms = blackMoves' board
-- opps = board.whitePawns .|. board.king
-- applyChanges :: Int8 -> Int8 -> Board
-- applyChanges orig dest =
-- let
-- departedBlacks = clearBit board.blackPawns $ fromIntegral orig
-- capturedSquares = captures departedBlacks opps dest
-- in
-- board
-- { blackPawns = setBit departedBlacks $ fromIntegral dest
-- , whitePawns = xor board.whitePawns capturedSquares
-- }
-- in
-- V.map (uncurry applyChanges) ms
--
-- nextBoardsBlack'' :: Board -> V.Vector Board
-- nextBoardsBlack'' board =
-- let
-- ms = blackMoves'' board
-- opps = board.whitePawns .|. board.king
-- applyChanges :: Word128 -> Int8 -> Board
-- applyChanges departedBlacks dest =
-- let
-- capturedSquares = captures departedBlacks opps dest
-- in
-- board
-- { blackPawns = setBit departedBlacks $ fromIntegral dest
-- , whitePawns = xor board.whitePawns capturedSquares
-- }
-- in
-- V.map (uncurry applyChanges) ms
--
-- nextBoardsBlack''' :: Board -> V.Vector Board
-- nextBoardsBlack''' board@Board{whitePawns, king, blackPawns} =
-- let
-- opps = whitePawns .|. king
-- occ = pawnIllegalDestinations .|. opps .|. blackPawns
-- in
-- V.concatMap
-- ( \i ->
-- let
-- departedBlacks = clearBit board.blackPawns $ fromIntegral i
-- in
-- V.map
-- ( \dest ->
-- let
-- capturedSquares = captures departedBlacks opps dest
-- in
-- board
-- { blackPawns = setBit departedBlacks $ fromIntegral dest
-- , whitePawns = xor whitePawns capturedSquares
-- }
-- )
-- (pieceMoves occ i)
-- )
-- \$ V.filter (testBit blackPawns . fromIntegral)
-- \$ V.fromList [0 .. 120]

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
  RotatedZobrist ->
  [((Int8, Int8), Board, RotatedZobrist)]
nextMoveBoardsWhiteZ board zobrist =
  let
    opps = board.blackPawns
    pawnMs = whiteMoves' board
    applyPawnChanges :: Int8 -> Int8 -> ((Int8, Int8), Board, RotatedZobrist)
    applyPawnChanges orig dest =
      let
        departed = clearBit board.whitePawns $ fromIntegral orig
        capturedSquares = captures departed opps dest
        updatedBoard =
          board
            { whitePawns = setBit departed $ fromIntegral dest
            , blackPawns = xor board.blackPawns capturedSquares
            }
        zPieces =
          (WhiteType, dest)
            : (WhiteType, orig)
            : [(BlackType, i) | i <- [0 .. 120], testBoardBit capturedSquares i]
        updatedZobrist = updateRotatedZobrist zPieces zobrist
       in
        ((orig, dest), updatedBoard, updatedZobrist)
    kingMs = kingMoves' board
    applyKingChanges :: Int8 -> Int8 -> ((Int8, Int8), Board, RotatedZobrist)
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
        updatedZobrist = updateRotatedZobrist zPieces zobrist
       in
        ((orig, dest), updatedBoard, updatedZobrist)
   in
    map (uncurry applyKingChanges) kingMs
      ++ map (uncurry applyPawnChanges) pawnMs

--------------------------------------------------------------------------------
-- experimentla move list

fromMovesList ::
  V.Vector (Int8, Int8) ->
  -- BV.Vector (Maybe Int8, Maybe Int8, Maybe Int8, Maybe Int8)
  V.Vector (Int8, (Int8, Int8, Int8, Int8))
fromMovesList input =
  -- BV.fromList $
  --   M.elems $
  V.fromList $
    M.assocs $
      M.mapWithKey positions $
        M.unionsWith (++) $
          map (\(orig, dest) -> M.singleton dest [orig]) $
            V.toList input
 where
  positions ::
    Int8 ->
    [Int8] ->
    (Int8, Int8, Int8, Int8)
  positions key xs =
    let
      minBase = max key 10
      maxBase = min key 110
     in
      ( fromMaybe key $ viaNonEmpty head $ filter (> maxBase + 10) xs
      , fromMaybe key $ viaNonEmpty head $ filter (< minBase - 10) xs
      , fromMaybe key $ viaNonEmpty head $ filter (\x -> key < x && x <= maxBase + 10) xs
      , fromMaybe key $ viaNonEmpty head $ filter (\x -> key > x && x >= minBase - 10) xs
      )

consJust :: Maybe a -> BV.Vector a -> BV.Vector a
consJust (Just a) v = BV.cons a v
consJust _ v = v

nextBoardsFromMovesList :: Board -> V.Vector (Int8, (Int8, Int8, Int8, Int8)) -> BV.Vector Board
nextBoardsFromMovesList board ms =
  let
    opps = board.whitePawns .|. board.king
    applyChanges :: (Int8, (Int8, Int8, Int8, Int8)) -> BV.Vector Board
    applyChanges (dest, (n, s, e, w)) =
      let
        arrivedBlacks = setBit board.blackPawns $ fromIntegral dest
        arriveAndCapture orig =
          let
            newBlacks = clearBit arrivedBlacks $ fromIntegral orig
            capturedSquares = captures newBlacks opps dest
           in
            board
              { blackPawns = newBlacks
              , whitePawns = xor board.whitePawns capturedSquares
              }
        northBoard = if n == dest then BV.empty else BV.singleton $ arriveAndCapture n
        southBoard = if s == dest then BV.empty else BV.singleton $ arriveAndCapture s
        eastBoard = if e == dest then BV.empty else BV.singleton $ arriveAndCapture e
        westBoard = if w == dest then BV.empty else BV.singleton $ arriveAndCapture w
       in
        westBoard <> eastBoard <> southBoard <> northBoard
   in
    BV.concatMap applyChanges $ BV.convert ms

--------------------------------------------------------------------------------
-- Win Conditions

kingEscaped :: Board -> Bool
kingEscaped b =
  let kingIndex = countTrailingZeros b.king
   in kingIndex == 0 || kingIndex == 10 || kingIndex == 109 || kingIndex == 120

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
-- board judgement

blackPieceCount :: Board -> Int
blackPieceCount b = popCount b.blackPawns

whitePieceCount :: Board -> Int
whitePieceCount b = popCount b.whitePawns

scoreBoard :: Team -> Board -> Int
scoreBoard t b =
  let
    (whiteMod, blackMod) = case t of
      White -> (id, negate)
      Black -> (negate, id)
    whitePoints =
      sum
        [ whitePieceCount b * 1000
        , whiteMoveCount' b
        ]
    blackPoints =
      sum
        [ blackPieceCount b * 1000
        , blackMoveCount' b
        ]
   in
    if
        | kingEscaped b -> whiteMod 1000000
        | kingCaptured b -> blackMod 1000000
        | otherwise -> whiteMod whitePoints + blackMod blackPoints

--------------------------------------------------------------------------------
-- AI

data Tree a = Node a [Tree a]

data SearchState

type Depth = Int

type Score = Int

type Tally = Int

type Visited = Int

type Alpha = Int

type Beta = Int

data Result = Result
  { move :: (Int8, Int8)
  , board :: [Board]
  , score :: Score
  , tally :: Visited
  }

data ZobristResult = ZobristResult
  { move :: (Int8, Int8)
  , board :: [Board]
  , score :: Score
  , tally :: Visited
  , zobrist :: RotatedZobrist
  }

-- minimax :: Board -> Team -> Depth -> Result
-- minimax b t d =
--   let
--     startBoards = case t of
--       Black -> map (\m -> (m, applyMoveBlack b m)) $ V.toList $ blackMoves b
--       White ->
--         map (\m -> (m, applyMoveKing b $ snd m)) (V.toList $ kingMoves b)
--           ++ map (\m -> (m, applyMoveWhitePawn b m)) (V.toList $ whiteMoves b)
--     judge = scoreBoard t
--     go :: Team -> Depth -> Board -> (Score, Visited)
--     go _ 0 b' = (judge b', 1)
--     go t' d' b' =
--       let order = if t' == t then V.maximumBy else V.minimumBy
--           nextBoards = case t' of
--             White -> nextBoardsBlack b'
--             Black -> nextBoardsWhite b'
--           base = V.map (go (opp t') (d' - 1)) nextBoards
--           best = order (comparing fst) base
--           visited = V.sum $ V.map snd base
--        in (snd best, visited)
--     base =
--       map
--         ( \(m, b) ->
--             let (score', visited) = go t d b
--              in Result m b score' visited
--         )
--         startBoards
--    in
--     (maximumBy (comparing score) base){tally = sum $ map tally base}

-- minimaxAlphaBeta :: Board -> Team -> Depth -> Result
-- minimaxAlphaBeta b t d =
--   let
--     startBoards = case t of
--       Black -> map (\m -> (m, applyMoveBlack b m)) $ V.toList $ blackMoves b
--       White ->
--         map (\m -> (m, applyMoveKing b $ snd m)) (V.toList $ kingMoves b)
--           ++ map (\m -> (m, applyMoveWhitePawn b m)) (V.toList $ whiteMoves b)
--     judge = scoreBoard t
--     go :: Team -> Depth -> Board -> Alpha -> Beta -> (Score, Visited)
--     go _ 0 b' alpha beta = (judge b', 1)
--     go t' d' b' alpha beta =
--       let order = if t' == t then V.maximumBy else V.minimumBy
--           nextBoards = case t' of
--             White -> nextBoardsBlack b'
--             Black -> nextBoardsWhite b'
--           base' = V.map (go (opp t') (d' - 1)) nextBoards
--           best = order (comparing fst) base
--           visited = V.sum $ V.map snd base
--        in (snd best, visited)
--     base =
--       map
--         ( \(m, b) ->
--             let (score', visited) = go t d b
--              in Result m b score' visited
--         )
--         startBoards
--    in
--     (maximumBy (comparing score) base){visited = sum $ map visited base}

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = if f x >= f y then x else y

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = if f x <= f y then x else y

alphaBeta :: (Int8, Int8) -> [Board] -> Team -> Team -> Depth -> Tally -> Alpha -> Beta -> Result
alphaBeta move board maximizer _ 0 tally _ _ =
  Result move board (scoreBoard maximizer (L.head board)) (tally + 1)
alphaBeta _ board maximizer current depth tally alpha beta =
  let
    nextBoards :: [((Int8, Int8), [Board])] = case current of
      White -> map (\(m, b) -> (m, b : board)) $ V.toList $ nextMoveBoardsWhite (L.head board)
      Black -> map (\(m, b) -> (m, b : board)) $ V.toList $ nextMoveBoardsBlack (L.head board)

    -- Maximize
    maximize :: [((Int8, Int8), [Board])] -> Tally -> Alpha -> Beta -> Result
    maximize [] _ _ _ = error "this should probably be nonempty"
    maximize [(move', board')] tally' alpha' beta' =
      alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
    maximize ((move', board') : ms) tally' alpha' beta' =
      let
        result = alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
        newAlpha = max alpha' result.score
        ~remainder = maximize ms result.tally newAlpha beta'
       in
        if result.score > beta' then result else maxBy (.score) (result{tally = remainder.tally} :: Result) remainder

    -- Minimize
    minimize :: [((Int8, Int8), [Board])] -> Tally -> Alpha -> Beta -> Result
    minimize [] _ _ _ = error "this should probably be nonempty"
    minimize [(move', board')] tally' alpha' beta' =
      alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
    minimize ((move', board') : ms) tally' alpha' beta' =
      let
        result = alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
        newBeta = min beta' result.score
        ~remainder = minimize ms result.tally alpha' newBeta
        selected = if result.score < alpha' then result else minBy (.score) (result{tally = remainder.tally} :: Result) remainder
       in
        selected
   in
    if maximizer == current
      then maximize nextBoards tally alpha beta
      else minimize nextBoards tally alpha beta

negamaxAB :: (Int8, Int8) -> [Board] -> Team -> Depth -> Tally -> Alpha -> Beta -> IO Result
negamaxAB move board current 0 tally _ _ =
  pure $ Result move board (scoreBoard current (L.head board)) (tally + 1)
negamaxAB _ board current depth tally alpha beta =
  let
    nextBoards :: [((Int8, Int8), [Board])] = case current of
      White ->
        map (\(m, b) -> (m, b : board)) $
          nextMoveBoardsWhite' (L.head board)
      Black ->
        map (\(m, b) -> (m, b : board)) $
          nextMoveBoardsBlack' (L.head board)

    negateScore :: Result -> Result
    negateScore result@Result{score} = result{score = negate score}

    {-
        go :: [((Int8, Int8), [Board])] -> Tally -> Alpha -> Beta -> Result
        go [] _ _ _ = error "this should probably be nonempty"
        go [(move', board')] tally' alpha' beta' =
          negateScore $
            negamaxAB
              move'
              board'
              (opp current)
              (depth - 1)
              tally'
              (negate beta')
              (negate alpha')
        go ((move', board') : ms) tally' alpha' beta' =
          let
            result = negateScore $ negamaxAB move' board' (opp current) (depth - 1) tally' (negate beta') (negate alpha')
            newAlpha = max alpha' result.score
            ~remainder = go ms result.tally newAlpha beta'
           in
            if newAlpha >= beta' then result else maxBy (.score) (result{tally = remainder.tally}) remainder
    -}

    initial =
      negateScore
        <$> uncurry
          negamaxAB
          (L.head nextBoards)
          (opp current)
          (depth - 1)
          tally
          (negate beta)
          (negate alpha)
    go' :: [((Int8, Int8), [Board])] -> Result -> Alpha -> Beta -> IO Result
    go' [] prev _ _ = pure prev
    go' ((move', board') : ms) prev alpha' beta' =
      do
        result <- negateScore <$> negamaxAB move' board' (opp current) (depth - 1) prev.tally (negate beta') (negate alpha')
        let newAlpha = max alpha' result.score
            best = maxBy (.score) (prev{tally = result.tally} :: Result) result
        if newAlpha >= beta' then pure best else go' ms best newAlpha beta'
   in
    do
      -- go nextBoards tally alpha beta
      i' <- initial
      go' nextBoards i' alpha beta

{-
negamaxABF :: (Int8, Int8) -> [Board] -> Team -> Depth -> Tally -> Alpha -> Beta -> Result
negamaxABF move board current 0 tally _ _ =
  Result move board (scoreBoard current (L.head board)) (tally + 1)
negamaxABF _ board current depth tally alpha beta =
  let
    nextBoards :: [((Int8, Int8), [Board])] = case current of
      White -> map (\(m, b) -> (m, b : board)) $ V.toList $ nextMoveBoardsWhite (L.head board)
      Black -> map (\(m, b) -> (m, b : board)) $ V.toList $ nextMoveBoardsBlack (L.head board)
    firstResult =
      let orig =
            uncurry
              negamaxAB
              (L.head nextBoards)
              (opp current)
              (depth - 1)
              tally
              (-beta)
              (-alpha)
       in orig{score = negate orig.score}
    step ::
      ((Move, [Board]), Alpha) ->
      (((Move, [Board]), Alpha) -> ((Move, [Board]), Alpha)) ->
      ((Move, [Board]), Alpha) ->
      ((Move, [Board]), Alpha)
    step = undefined
   in
    -- processRemainder ::

    undefined

-- https://stackoverflow.com/questions/55170949/fold-thats-both-constant-space-and-short-circuiting
-- convert to my stuff
myProduct :: (Foldable t, Eq n, Num n) => t n -> n
myProduct xs = foldr step id xs 1
 where
  step 0 f acc = 0
  step x f acc = f $! acc * x

-- https://stackoverflow.com/questions/55170949/fold-thats-both-constant-space-and-short-circuiting
-- convert to my stuff
-- myThing :: (Foldable t, Eq n, Num n) => t n -> n
myThing xs = foldr step id xs "start"
 where
  step "two" f acc = acc
  step x f acc = f $! acc ++ x

-}

data ZobristStats = ZobristStats
  { misses :: Integer
  , unrotatedHits :: Integer
  , rotatedHits :: Integer
  , hitDepth :: Map Int Int
  }
  deriving (Generic, Show)

negamaxABZ ::
  (Int8, Int8) ->
  [Board] ->
  Team ->
  Depth ->
  Tally ->
  Alpha ->
  Beta ->
  RotatedZobrist ->
  IORef ZobristStats ->
  IORef VectorHashTable ->
  IO ZobristResult
negamaxABZ move board current 0 tally alpha beta z zs htr = do
  ht <- readIORef htr
  let
    selectedHash = maxZobrist z
    isRotatedHash = selectedHash /= z.rotated0
  score <-
    VH.lookup ht selectedHash >>= \case
      Just (cachedDepth, cachedScore, flag, cachedBoard) | cachedDepth == 0 ->
        do
          when (not $ L.head board `elem` allVariations cachedBoard) $ do
            putStrLn $ showBoard cachedBoard
            putStrLn ""
            putStrLn $ showBoard $ L.head board
            (fail "explode")
          when (not $ cachedScore == scoreBoard current (L.head board)) $ do
            putStrLn $ showBoard cachedBoard
            putStrLn ""
            putStrLn $ showBoard $ L.head board
            (fail "explode")
          modifyIORef
            zs
            ( \zs' ->
                if isRotatedHash
                  then zs'{rotatedHits = zs'.rotatedHits + 1}
                  else zs'{unrotatedHits = zs'.unrotatedHits + 1}
            )
          pure cachedScore
      _ -> do
        modifyIORef zs (\zs' -> zs'{misses = zs'.misses + 1})
        let freshScore = scoreBoard current (L.head board)
            flag =
              if
                  | freshScore <= alpha -> 3
                  | freshScore >= beta -> 1
                  | otherwise -> 2
        VH.insert ht (maxZobrist z) (0, freshScore, flag, L.head board)
        pure freshScore
  {-
  let score = scoreBoard current (L.head board)
  -}
  pure $ ZobristResult move board score (tally + 1) z
negamaxABZ move board current depth tally alpha beta z zsr htr =
  let
    nextBoards :: [((Int8, Int8), [Board], RotatedZobrist)] = case current of
      White ->
        map (\(m, b, z') -> (m, b : board, z')) $
          nextMoveBoardsWhiteZ (L.head board) z
      Black ->
        map (\(m, b, z') -> (m, b : board, z')) $
          nextMoveBoardsBlackZ (L.head board) z

    negateScore :: ZobristResult -> ZobristResult
    negateScore result@ZobristResult{score} = result{score = negate score}

    initial alpha' beta' =
      L.head nextBoards & \(move', board', zobrist') ->
        negateScore
          <$> negamaxABZ
            move'
            board'
            (opp current)
            (depth - 1)
            tally
            (negate beta')
            (negate alpha')
            zobrist'
            zsr
            htr

    go' :: [((Int8, Int8), [Board], RotatedZobrist)] -> ZobristResult -> Alpha -> Beta -> IO ZobristResult
    go' [] prev _ _ = pure prev
    go' ((move', board', zobrist') : ms) prev alpha' beta' =
      do
        result <- negateScore <$> negamaxABZ move' board' (opp current) (depth - 1) prev.tally (negate beta') (negate alpha') zobrist' zsr htr
        let newAlpha = max alpha' result.score
            best :: ZobristResult
            best = maxBy (.score) (prev{tally = result.tally} :: ZobristResult) result
        if newAlpha >= beta' then pure best else go' ms best newAlpha beta'
   in
    do
      ht <- readIORef htr
      let
        selectedHash = maxZobrist z
        isRotatedHash = selectedHash /= z.rotated0
      (scoreOverride, alphaOverride, betaOverride) <-
        VH.lookup ht selectedHash >>= \case
          Just (cachedDepth, cachedScore, flag, cachedBoard) | cachedDepth == depth ->
            do
              modifyIORef
                zsr
                ( \zs' ->
                    if isRotatedHash
                      then zs'{ rotatedHits = zs'.rotatedHits + 1
                              , hitDepth = M.alter (Just . maybe 1 (+1)) depth zs'.hitDepth
                              }
                      else zs'{unrotatedHits = zs'.unrotatedHits + 1}
                )
              pure $ case flag of
                1 ->
                  let alphaOverride = max alpha cachedScore
                   in if alphaOverride > beta
                        then (Just cachedScore, alphaOverride, beta)
                        else (Nothing, alphaOverride, beta)
                2 -> (Just cachedScore, alpha, beta)
                _ ->
                  let betaOverride = min beta cachedScore
                   in if alpha > betaOverride
                        then (Just cachedScore, alpha, betaOverride)
                        else (Nothing, alpha, betaOverride)
          -- when (not $ L.head board `elem` allVariations cachedBoard) $ do
          --   putStrLn $ showBoard cachedBoard
          --   putStrLn ""
          --   putStrLn $ showBoard $ L.head board
          --   (fail "explode: branch: boards not equal")
          -- when (result.score /= scoreToUse) $ do
          --   print cachedDepth
          --   print current
          --   putStrLn $ showBoard cachedBoard
          --   putStrLn ""
          --   putStrLn $ showBoard $ L.head board
          --   (fail "explode: branch: scores not equal")
          _ -> do
            modifyIORef zsr (\zs' -> zs'{misses = zs'.misses + 1})
            pure (Nothing, alpha, beta)
      case scoreOverride of
        Just override ->
          pure $ ZobristResult move board override (tally + 1) z
        Nothing -> do
          i' <- initial alphaOverride betaOverride
          result <- go' nextBoards i' alphaOverride betaOverride
          let flag =
                if
                    | result.score <= alpha -> 3
                    | result.score >= betaOverride -> 1
                    | otherwise -> 2
          VH.insert ht (maxZobrist z) (depth, result.score, flag, L.head board)
          pure $ ZobristResult result.move result.board result.score result.tally z

-- type VectorHashTable = VH.Dictionary (PrimState IO) VM.MVector Word64 VM.MVector (Int, Int)

negamaxABZ' ::
  (Int8, Int8) ->
  [Board] ->
  Team ->
  Depth ->
  Tally ->
  Alpha ->
  Beta ->
  RotatedZobrist ->
  IORef ZobristStats ->
  IORef VectorHashTable ->
  IO ZobristResult
negamaxABZ' move board current depth tally alpha beta z zsr htr =
  let
    inner :: Int -> Int -> IO ZobristResult
    inner innerAlpha innerBeta
      | depth == 0 =
          pure $
            ZobristResult
              move
              board
              (scoreBoard current (L.head board))
              (tally + 1)
              z
      | otherwise =
          let
            nextBoards :: [((Int8, Int8), [Board], RotatedZobrist)] = case current of
              White ->
                map (\(m, b, z') -> (m, b : board, z')) $
                  nextMoveBoardsWhiteZ (L.head board) z
              Black ->
                map (\(m, b, z') -> (m, b : board, z')) $
                  nextMoveBoardsBlackZ (L.head board) z

            negateScore :: ZobristResult -> ZobristResult
            negateScore result = result{score = negate result.score}

            initial =
              L.head nextBoards & \(move', board', zobrist') ->
                negateScore
                  <$> negamaxABZ'
                    move'
                    board'
                    (opp current)
                    (depth - 1)
                    tally
                    (negate innerBeta)
                    (negate innerAlpha)
                    zobrist'
                    zsr
                    htr

            go ::
              ZobristResult ->
              [((Int8, Int8), [Board], RotatedZobrist)] ->
              Alpha ->
              Beta ->
              IO ZobristResult
            go prev [] _ _ = pure prev
            go prev ((move', board', zobrist') : ms) alpha' beta' = do
              let newAlpha = max prev.score alpha'
              if newAlpha >= beta'
                then pure prev
                else do
                  result <-
                    negateScore
                      <$> negamaxABZ'
                        move'
                        board'
                        (opp current)
                        (depth - 1)
                        prev.tally
                        (negate innerBeta)
                        (negate newAlpha)
                        zobrist'
                        zsr
                        htr
                  let best = maxBy (.score) (prev{tally = result.tally} :: ZobristResult) result
                  go best ms newAlpha beta'
           in
            do
              i' <- initial
              result <- go i' (L.tail nextBoards) innerAlpha innerBeta
              pure $ ZobristResult result.move result.board result.score result.tally z
    getTransposition :: IO (Maybe Int, Int, Int)
    getTransposition = do
        let
          selectedHash = maxZobrist z
          isRotatedHash = selectedHash /= z.rotated0
        ht <- readIORef htr
        VH.lookup ht selectedHash >>= \case
          Just (cachedDepth, cachedScore, flag, cachedBoard) | cachedDepth >= depth ->
            do
              modifyIORef
                zsr
                ( \zs' ->
                    if isRotatedHash
                      then zs'{rotatedHits = zs'.rotatedHits + 1
                              , hitDepth = M.alter (Just . maybe 1 (+1)) depth zs'.hitDepth
                              }
                      else zs'{unrotatedHits = zs'.unrotatedHits + 1
                              , hitDepth = M.alter (Just . maybe 1 (+1)) depth zs'.hitDepth
                              }
                )
              pure $ case flag of
                -- exact
                2 -> (Just cachedScore, alpha, beta)
                -- lowerbound
                1 ->
                  let alphaOverride = max alpha cachedScore
                   in if alphaOverride >= beta
                        then (Just cachedScore, alphaOverride, beta)
                        else (Nothing, alphaOverride, beta)
                -- upperbound
                _ ->
                  let betaOverride = min beta cachedScore
                   in if alpha >= betaOverride
                        then (Just cachedScore, alpha, betaOverride)
                        else (Nothing, alpha, betaOverride)
          _ -> do
            modifyIORef zsr (\zs' -> zs'{misses = zs'.misses + 1})
            pure (Nothing, alpha, beta)
   in do
    (ttScore, ttAlpha, ttBeta) <- getTransposition
    case ttScore of
      Just s -> pure $ ZobristResult move board s tally z
      Nothing -> do
        result <- inner ttAlpha ttBeta

        when (depth > 0) $
          let flag = if
                | result.score <= alpha -> 3
                | result.score >= beta -> 1
                | otherwise -> 2
          in do
            ht <- readIORef htr
            VH.insert ht (maxZobrist z) (depth, result.score, flag, L.head board)

        pure result

-- | (depth, score, 1 = lowerbound | 2 = exact | 3 = upperbound, board)
type VectorHashTable = VH.Dictionary (PrimState IO) VM.MVector Word64 V.MVector (Int, Int, Int, Board)

newtype Test = Test Int

runSearch :: Board -> Team -> IO (ZobristResult, ZobristStats)
runSearch board team = do
  ht <- VH.initialize 10000 :: IO VectorHashTable
  htr <- newIORef ht
  zsr <- newIORef $ ZobristStats 0 0 0 mempty
  let startZobrist = boardToRotatedZobrist board True
  result <- negamaxABZ (0, 0) [board] team 4 0 (minBound + 10) (maxBound - 10) startZobrist zsr htr
  statResults <- readIORef zsr
  pure (result, statResults)

runSearch' :: Board -> Team -> IO (ZobristResult, ZobristStats)
runSearch' board team = do
  ht <- VH.initialize 10000 :: IO VectorHashTable
  htr <- newIORef ht
  zsr <- newIORef $ ZobristStats 0 0 0 mempty
  let startZobrist = boardToRotatedZobrist board True
  result <- negamaxABZ' (0, 0) [board] team 5 0 (minBound + 10) (maxBound - 10) startZobrist zsr htr
  statResults <- readIORef zsr
  pure (result, statResults)

--------------------------------------------------------------------------------
-- bench

-- nextBoardsBlack1 :: Board -> Int
-- nextBoardsBlack1 board = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack $ nextBoardsBlack board
--
-- nextBoardsBlack2 :: Board -> (Int, Int)
-- nextBoardsBlack2 board =
--   let
--     !seed = nextBoardsBlack board
--     !(!s1, !s2) = BV.splitAt (div (BV.length seed) 2) seed
--     ~r1 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s1
--     ~r2 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s2
--    in
--     par r1 (seq r2 (r1, r2))
--
-- nextBoardsBlack3 :: Board -> (Int, Int, Int)
-- nextBoardsBlack3 board =
--   let
--     !seed = nextBoardsBlack board
--     !(!s1, !s2') = BV.splitAt (div (BV.length seed) 3) seed
--     !(!s2, !s3) = BV.splitAt (div (BV.length seed) 2) s2'
--     ~r1 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s1
--     ~r2 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s2
--     ~r3 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s3
--    in
--     par r1 (par r2 (seq r3 (r1, r2, r3)))
--
-- nextBoardsBlack4 :: Board -> (Int, Int, Int, Int)
-- nextBoardsBlack4 board =
--   let
--     !seed = nextBoardsBlack board
--     !(!s1, !s2') = BV.splitAt (div (BV.length seed) 4) seed
--     !(!s2, !s3') = BV.splitAt (div (BV.length seed) 3) s2'
--     !(!s3, !s4) = BV.splitAt (div (BV.length seed) 2) s3'
--     ~r1 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s1
--     ~r2 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s2
--     ~r3 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s3
--     ~r4 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsBlack s4
--    in
--     par r1 (par r2 (par r3 (seq r4 (r1, r2, r3, r4))))
--
-- nextBoards4 :: Board -> (Int, Int, Int, Int)
-- nextBoards4 board =
--   let
--     !seed = nextBoardsBlack board
--     !(!s1, !s2') = BV.splitAt (div (BV.length seed) 4) seed
--     !(!s2, !s3') = BV.splitAt (div (BV.length seed) 3) s2'
--     !(!s3, !s4) = BV.splitAt (div (BV.length seed) 2) s3'
--     ~r1 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite s1
--     ~r2 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite s2
--     ~r3 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite s3
--     ~r4 = getSum $ BV.foldMap (Sum . popCount . blackPawns) $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite s4
--    in
--     par r1 (par r2 (par r3 (seq r4 (r1, r2, r3, r4))))
--
-- nextBoards4Sum :: Board -> Int
-- nextBoards4Sum board =
--   BV.length $ join $ BV.map nextBoardsWhite $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite $ nextBoardsBlack board
--
-- nextBoards5Sum :: Board -> Int
-- nextBoards5Sum board =
--   BV.length $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite $ join $ BV.map nextBoardsBlack $ join $ BV.map nextBoardsWhite $ nextBoardsBlack board

--------------------------------------------------------------------------------
-- Zobrist hash

whitePawnMasks :: V.Vector Word64
whitePawnMasks = V.unfoldrN 121 (Just . random) $ mkStdGen 125

kingMasks :: V.Vector Word64
kingMasks = V.unfoldrN 121 (Just . random) $ mkStdGen 7234

blackPawnMasks :: V.Vector Word64
blackPawnMasks = V.unfoldrN 121 (Just . random) $ mkStdGen 938457

blackTurnMask :: Word64
blackTurnMask = fst $ random $ mkStdGen 374

-- generate initial zobrist hash for board
boardToZobrist ::
  -- | white pawn masks
  V.Vector Word64 ->
  -- | king masks
  V.Vector Word64 ->
  -- | black pawn masks
  V.Vector Word64 ->
  Board ->
  Bool ->
  Word64
boardToZobrist wpms kms bpms b isBlackTurn =
  foldl'
    (\hash i -> maybe hash (xor hash) (selectMask i))
    (if isBlackTurn then blackTurnMask else 0)
    [0 .. 120]
 where
  selectMask i =
    if
        | testBit b.whitePawns i -> Just $ wpms V.! i
        | testBit b.king i -> Just $ kms V.! i
        | testBit b.blackPawns i -> Just $ bpms V.! i
        | otherwise -> Nothing

boardToZobrist0 :: Board -> Bool -> Word64
boardToZobrist0 =
  boardToZobrist whitePawnMasks kingMasks blackPawnMasks

boardToZobrist90 :: Board -> Bool -> Word64
boardToZobrist90 =
  boardToZobrist whitePawnMasks90 kingMasks90 blackPawnMasks90

boardToZobrist180 :: Board -> Bool -> Word64
boardToZobrist180 =
  boardToZobrist whitePawnMasks180 kingMasks180 blackPawnMasks180

boardToZobrist270 :: Board -> Bool -> Word64
boardToZobrist270 =
  boardToZobrist whitePawnMasks270 kingMasks270 blackPawnMasks270

boardToRotatedZobrist :: Board -> Bool -> RotatedZobrist
boardToRotatedZobrist board isBlackTurn =
  RotatedZobrist
    { rotated0 = boardToZobrist0 board isBlackTurn
    , rotated90 = boardToZobrist90 board isBlackTurn
    , rotated180 = boardToZobrist180 board isBlackTurn
    , rotated270 = boardToZobrist270 board isBlackTurn
    }

-- TODO: specialize pragmas for the below
indexToCoord :: Integral i => i -> (i, i)
indexToCoord = swap . flip divMod 11

coordToIndex :: Integral i => (i, i) -> i
coordToIndex (x, y) = (y * 11) + x

centerCoord :: Integral i => (i, i) -> (i, i)
centerCoord (x, y) = (x - 5, y - 5)

uncenterCoord :: Integral i => (i, i) -> (i, i)
uncenterCoord (x, y) = (x + 5, y + 5)

rotate90 :: Integral i => (i, i) -> (i, i)
rotate90 (x, y) = (y, -x)

rotate180 :: Integral i => (i, i) -> (i, i)
rotate180 (x, y) = (-x, -y)

rotate270 :: Integral i => (i, i) -> (i, i)
rotate270 (x, y) = (-y, x)

withCenteredCoord :: Integral i => ((i, i) -> (i, i)) -> i -> i
withCenteredCoord f i =
  coordToIndex $ uncenterCoord $ f $ centerCoord $ indexToCoord i

rotateIndex90 :: Integral i => i -> i
rotateIndex90 = withCenteredCoord rotate90

rotateIndex180 :: Integral i => i -> i
rotateIndex180 = withCenteredCoord rotate180

rotateIndex270 :: Integral i => i -> i
rotateIndex270 = withCenteredCoord rotate270

rotateMasks :: V.Vector Word64 -> (Int -> Int) -> V.Vector Word64
rotateMasks ms f = V.map ((ms V.!) . f) $ V.fromList [0 .. 120]

whitePawnMasks90 :: V.Vector Word64
whitePawnMasks90 = rotateMasks whitePawnMasks rotateIndex90

whitePawnMasks180 :: V.Vector Word64
whitePawnMasks180 = rotateMasks whitePawnMasks rotateIndex180

whitePawnMasks270 :: V.Vector Word64
whitePawnMasks270 = rotateMasks whitePawnMasks rotateIndex270

kingMasks90 :: V.Vector Word64
kingMasks90 = rotateMasks kingMasks rotateIndex90

kingMasks180 :: V.Vector Word64
kingMasks180 = rotateMasks kingMasks rotateIndex180

kingMasks270 :: V.Vector Word64
kingMasks270 = rotateMasks kingMasks rotateIndex270

blackPawnMasks90 :: V.Vector Word64
blackPawnMasks90 = rotateMasks blackPawnMasks rotateIndex90

blackPawnMasks180 :: V.Vector Word64
blackPawnMasks180 = rotateMasks blackPawnMasks rotateIndex180

blackPawnMasks270 :: V.Vector Word64
blackPawnMasks270 = rotateMasks blackPawnMasks rotateIndex270

-- Oh shit I also need to mirror them, so this ends up holding 8
data RotatedZobrist = RotatedZobrist
  { rotated0 :: Word64
  , rotated90 :: Word64
  , rotated180 :: Word64
  , rotated270 :: Word64
  }

updateRotatedZobrist :: [(PieceType, Int8)] -> RotatedZobrist -> RotatedZobrist
updateRotatedZobrist pieces zobrist = foldl' (\acc (t, i) -> updateForPiece t i acc) turnToggled pieces
 where
  turnToggled =
    RotatedZobrist
      { rotated0 = xor zobrist.rotated0 blackTurnMask
      , rotated90 = xor zobrist.rotated90 blackTurnMask
      , rotated180 = xor zobrist.rotated180 blackTurnMask
      , rotated270 = xor zobrist.rotated270 blackTurnMask
      }
  updateForPiece :: PieceType -> Int8 -> RotatedZobrist -> RotatedZobrist
  updateForPiece t i z =
    case t of
      WhiteType ->
        RotatedZobrist
          { rotated0 = xor z.rotated0 (whitePawnMasks V.! fromIntegral i)
          , rotated90 = xor z.rotated90 (whitePawnMasks90 V.! fromIntegral i)
          , rotated180 = xor z.rotated180 (whitePawnMasks180 V.! fromIntegral i)
          , rotated270 = xor z.rotated270 (whitePawnMasks270 V.! fromIntegral i)
          }
      KingType ->
        RotatedZobrist
          { rotated0 = xor z.rotated0 (kingMasks V.! fromIntegral i)
          , rotated90 = xor z.rotated90 (kingMasks90 V.! fromIntegral i)
          , rotated180 = xor z.rotated180 (kingMasks180 V.! fromIntegral i)
          , rotated270 = xor z.rotated270 (kingMasks270 V.! fromIntegral i)
          }
      BlackType ->
        RotatedZobrist
          { rotated0 = xor z.rotated0 (blackPawnMasks V.! fromIntegral i)
          , rotated90 = xor z.rotated90 (blackPawnMasks90 V.! fromIntegral i)
          , rotated180 = xor z.rotated180 (blackPawnMasks180 V.! fromIntegral i)
          , rotated270 = xor z.rotated270 (blackPawnMasks270 V.! fromIntegral i)
          }

maxZobrist :: RotatedZobrist -> Word64
maxZobrist z = max z.rotated0 $ max z.rotated90 $ max z.rotated180 z.rotated270

--------------------------------------------------------------------------------
-- Rotate board

rotateLayer :: (Int8 -> Int8) -> Word128 -> Word128
rotateLayer f input =
  foldl'
    ( \acc i ->
        if testBit input (fromIntegral i)
          then setBit acc (fromIntegral $ f i)
          else acc
    )
    0
    [0 .. 120]

rotateLayer90 :: Word128 -> Word128
rotateLayer90 = rotateLayer rotateIndex90

rotateLayer180 :: Word128 -> Word128
rotateLayer180 = rotateLayer rotateIndex180

rotateLayer270 :: Word128 -> Word128
rotateLayer270 = rotateLayer rotateIndex270

rotateBoard :: (Word128 -> Word128) -> Board -> Board
rotateBoard f b =
  Board
    { whitePawns = f b.whitePawns
    , king = f b.king
    , blackPawns = f b.blackPawns
    }

rotateBoard90 :: Board -> Board
rotateBoard90 = rotateBoard rotateLayer90

rotateBoard180 :: Board -> Board
rotateBoard180 = rotateBoard rotateLayer180

rotateBoard270 :: Board -> Board
rotateBoard270 = rotateBoard rotateLayer270

allVariations :: Board -> [Board]
allVariations b = [b, rotateBoard90 b, rotateBoard180 b, rotateBoard270 b]

--------------------------------------------------------------------------------
-- debug print

showAllBoards :: BV.Vector Board -> IO ()
showAllBoards bs =
  BV.forM_ bs $ \b -> do
    putStrLn $ showBoard b
    putStrLn ""

-- use zobrist hashing for transposition table

-- haskell compact library may be good for reducing gc overhead on long-lived
-- lookup tables

--------------------------------------------------------------------------------
-- C interop

foreign import ccall "bump" c_bump :: CULong -> CULong -> IO CULong

foreign import ccall "pieceMoveCount" c_northMoveCount :: CULong -> CULong -> CInt -> IO CInt

foreign import ccall "teamMoveCount" c_teamMoveCount :: CULong -> CULong -> CULong -> CULong -> IO CInt

bump :: Word64 -> Int
-- {-# NOINLINE hLength #-}
bump b = unsafePerformIO $ do
  result <- c_bump (fromIntegral b) (fromIntegral b)
  return (fromIntegral result)

cPieceMoveCount :: Word128 -> Int -> Int
cPieceMoveCount occ pos = unsafePerformIO $ do
  result <-
    c_northMoveCount
      (fromIntegral occ.word128Hi64)
      (fromIntegral occ.word128Lo64)
      (fromIntegral pos)
  return (fromIntegral result)

cTeamMoveCount :: Word128 -> Word128 -> Int
cTeamMoveCount team occ = unsafePerformIO $ do
  result <-
    c_teamMoveCount
      (fromIntegral team.word128Hi64)
      (fromIntegral team.word128Lo64)
      (fromIntegral occ.word128Hi64)
      (fromIntegral occ.word128Lo64)
  return (fromIntegral result)

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

-- Generic free function; likely must be used with castFunPtr
foreign import ccall unsafe "stdlib.h &free" c_free_ptr :: FinalizerPtr CInt

foreign import ccall "test_ffi_move" c_test_ffi_move :: IO (Ptr Move)

testFfiMove :: Move
{-# NOINLINE testFfiMove #-}
testFfiMove = unsafePerformIO $ do
  fptr <- c_test_ffi_move >>= newForeignPtr (castFunPtr c_free_ptr)
  withForeignPtr fptr $ \ptr -> do
    peek ptr

testFfiMove' :: Move
{-# NOINLINE testFfiMove' #-}
testFfiMove' =
  unsafePerformIO $
    c_test_ffi_move
      >>= newForeignPtr (castFunPtr c_free_ptr)
      >>= flip withForeignPtr peek

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
