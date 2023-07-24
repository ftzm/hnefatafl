{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}

module Board.Move where

import Board.Board (Board (..), showBoard)
import Board.Constant (corners, pawnIllegalDestinations)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Foldable (maximumBy)
import Data.List (groupBy, maximum, minimum, minimumBy)
import Data.Map qualified as M
import Data.Vector qualified as BV
import Data.Vector.Unboxed qualified as V
import Data.WideWord (Word128)
import GHC.Conc (par)
import Text.RawString.QQ (r)

--------------------------------------------------------------------------------

data Team = White | Black
  deriving (Show, Generic, NFData, Eq)

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

blackMoveCount :: Board -> Int
blackMoveCount Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.sum $
        V.map (pieceMoveCount occ) $
          V.filter (testBit blackPawns . fromIntegral) $
            V.fromList [0 .. 120]

whiteMoves :: Board -> V.Vector (Int8, Int8)
whiteMoves Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.concatMap (\i -> V.map (i,) (pieceMoves occ i)) $
        V.filter (testBit whitePawns . fromIntegral) $
          V.fromList [0 .. 120]

whiteMoveCount :: Board -> Int
whiteMoveCount Board{whitePawns, king, blackPawns} =
  let occ = pawnIllegalDestinations .|. whitePawns .|. king .|. blackPawns
   in V.sum $
        V.map (pieceMoveCount occ) $
          V.filter (testBit whitePawns . fromIntegral) $
            V.fromList [0 .. 120]

kingMoves :: Board -> V.Vector (Int8, Int8)
kingMoves Board{whitePawns, king, blackPawns} =
  V.map (fromIntegral $ popCount king,) $
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

score :: Team -> Board -> Int
score t b =
  let
    (whiteMod, blackMod) = case t of
      White -> (id, negate)
      Black -> (negate, id)
    whitePoints =
      sum
        [ whitePieceCount b * 100
        , whiteMoveCount b
        ]
    blackPoints =
      sum
        [ blackPieceCount b * 100
        , blackMoveCount b
        ]
   in
    if
        | whiteVictory b -> whiteMod 1000000
        | blackVictory b -> blackMod 1000000
        | otherwise -> (whiteMod whitePoints + blackMod blackPoints)

--------------------------------------------------------------------------------
-- AI

data Tree a = Node a [Tree a]

data SearchState

type Depth = Int

type Move = (Int8, Int8)

type Score = Int

type Visited = Int

minimax :: Board -> Team -> Depth -> (Move, Board, Score, Visited)
minimax b t d =
  let
    startBoards = case t of
      Black -> map (\m -> (m, applyMoveBlack b m)) $ V.toList $ blackMoves b
      White ->
        map (\m -> (m, applyMoveKing b $ snd m)) (V.toList $ kingMoves b)
          ++ map (\m -> (m, applyMoveWhitePawn b m)) (V.toList $ whiteMoves b)
    judge = score t
    go :: Team -> Depth -> Board -> (Score, Visited)
    go _ 0 b' = (judge b', 1)
    go t' d' b' =
      let order = if t' == t then V.maximumBy else V.minimumBy
          nextBoards = case t' of
            White -> nextBoardsBlack b'
            Black -> nextBoardsWhite b'
      in order (comparing fst) $ V.map (go (opp t') (d'-1)) nextBoards
   in
    maximumBy (comparing (\(_, _, s, _) -> s)) $
      map
        ( \(m, b) ->
            let (score', visited) = go t d b
             in (m, b, score', visited)
        )
        startBoards

minimaxAlphaBeta :: Board -> Team -> Depth -> (Int8, Int8)
minimaxAlphaBeta = undefined

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
-- debug print

showAllBoards :: BV.Vector Board -> IO ()
showAllBoards bs =
  BV.forM_ bs $ \b -> do
    putStrLn $ showBoard b
    putStrLn ""

-- use zobrist hashing for transposition table

-- haskell compact library may be good for reducing gc overhead on long-lived
-- lookup tables
