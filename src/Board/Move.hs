{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
import Data.WideWord (Word128 (..))
import Foreign.C.Types (CInt (..), CULong (..))
import GHC.Conc (par)
import GHC.IO (unsafePerformIO)
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

nextMoveBoardsBlack :: Board -> V.Vector (Move, Board)
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

nextMoveBoardsWhite :: Board -> V.Vector (Move, Board)
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
    kingChanges :: V.Vector (Move, Board)
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
        [ whitePieceCount b * 100
        , whiteMoveCount' b
        ]
    blackPoints =
      sum
        [ blackPieceCount b * 100
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

type Move = (Int8, Int8)

type Score = Int

type Tally = Int

type Visited = Int

type Alpha = Int

type Beta = Int

data Result = Result {move :: Move, board :: Board, score :: Score, tally :: Visited}

minimax :: Board -> Team -> Depth -> Result
minimax b t d =
  let
    startBoards = case t of
      Black -> map (\m -> (m, applyMoveBlack b m)) $ V.toList $ blackMoves b
      White ->
        map (\m -> (m, applyMoveKing b $ snd m)) (V.toList $ kingMoves b)
          ++ map (\m -> (m, applyMoveWhitePawn b m)) (V.toList $ whiteMoves b)
    judge = scoreBoard t
    go :: Team -> Depth -> Board -> (Score, Visited)
    go _ 0 b' = (judge b', 1)
    go t' d' b' =
      let order = if t' == t then V.maximumBy else V.minimumBy
          nextBoards = case t' of
            White -> nextBoardsBlack b'
            Black -> nextBoardsWhite b'
          base = V.map (go (opp t') (d' - 1)) nextBoards
          best = order (comparing fst) base
          visited = V.sum $ V.map snd base
       in (snd best, visited)
    base =
      map
        ( \(m, b) ->
            let (score', visited) = go t d b
             in Result m b score' visited
        )
        startBoards
   in
    (maximumBy (comparing score) base){tally = sum $ map tally base}

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
maxBy f x y = if f x > f y then x else y

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = if f x < f y then x else y

alphaBeta :: Move -> Board -> Team -> Team -> Depth -> Tally -> Alpha -> Beta -> Result
alphaBeta move board maximizer _ 0 tally _ _ =
  Result move board (scoreBoard maximizer board) (tally + 1)
alphaBeta _ board maximizer current depth tally alpha beta =
  let
    nextBoards :: [(Move, Board)] = case current of
      White -> V.toList $ nextMoveBoardsWhite board
      Black -> V.toList $ nextMoveBoardsBlack board

    -- Maximize
    maximize :: [(Move, Board)] -> Tally -> Alpha -> Beta -> Result
    maximize [] _ _ _ = error "this should probably be nonempty"
    maximize [(move', board')] tally' alpha' beta' =
      alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
    maximize ((move', board') : ms) tally' alpha' beta' =
      let
        result = alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
        newAlpha = max alpha' result.score
        ~remainder = maximize ms result.tally newAlpha beta'
       in
        if result.score > beta' then result else maxBy (.score) (result {tally = remainder.tally}) remainder

    -- Minimize
    minimize :: [(Move, Board)] -> Tally -> Alpha -> Beta -> Result
    minimize [] _ _ _ = error "this should probably be nonempty"
    minimize [(move', board')] tally' alpha' beta' =
      alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
    minimize ((move', board') : ms) tally' alpha' beta' =
      let
        result = alphaBeta move' board' maximizer (opp current) (depth - 1) tally' alpha' beta'
        newBeta = min beta' result.score
        ~remainder = minimize ms result.tally alpha' newBeta
        selected = if result.score < alpha' then result else minBy (.score) (result {tally = remainder.tally}) remainder
       in
        selected
   in
    if maximizer == current
      then maximize nextBoards tally alpha beta
      else minimize nextBoards tally alpha beta

negamaxAB :: Move -> Board -> Team -> Team -> Depth -> Tally -> Alpha -> Beta -> Result
negamaxAB move board _ current 0 tally _ _ =
  Result move board (scoreBoard current board) (tally + 1)
negamaxAB _ board maximizer current depth tally alpha beta =
  let
    nextBoards :: [(Move, Board)] = case current of
      White -> V.toList $ nextMoveBoardsWhite board
      Black -> V.toList $ nextMoveBoardsBlack board

    go :: [(Move, Board)] -> Tally -> Alpha -> Beta -> Result
    go [] _ _ _ = error "this should probably be nonempty"
    go [(move', board')] tally' alpha' beta' =
      let orig = negamaxAB move' board' maximizer (opp current) (depth - 1) tally' (-beta') (-alpha')
      in orig {score = negate orig.score}
    go ((move', board') : ms) tally' alpha' beta' =
      let
        origResult = negamaxAB move' board' maximizer (opp current) (depth - 1) tally' (-beta') (-alpha')
        result = origResult {score = negate origResult.score}
        newAlpha = max alpha' result.score
        ~remainder = go ms result.tally newAlpha beta'
       in
        if newAlpha >= beta' then result else maxBy (.score) (result {tally = remainder.tally}) remainder
  in go nextBoards tally alpha beta

negamaxABF :: Move -> Board -> Team -> Team -> Depth -> Tally -> Alpha -> Beta -> Result
negamaxABF move board _ current 0 tally _ _ =
  Result move board (scoreBoard current board) (tally + 1)
negamaxABF _ board maximizer current depth tally alpha beta =
  let
    nextBoards :: [(Move, Board)] = case current of
      White -> V.toList $ nextMoveBoardsWhite board
      Black -> V.toList $ nextMoveBoardsBlack board

    go :: [(Move, Board)] -> Tally -> Alpha -> Beta -> Result
    go [] _ _ _ = error "this should probably be nonempty"
    go [(move', board')] tally' alpha' beta' =
      let orig = negamaxAB move' board' maximizer (opp current) (depth - 1) tally' (-beta') (-alpha')
      in orig {score = negate orig.score}
    go ((move', board') : ms) tally' alpha' beta' =
      let
        origResult = negamaxAB move' board' maximizer (opp current) (depth - 1) tally' (-beta') (-alpha')
        result = origResult {score = negate origResult.score}
        newAlpha = max alpha' result.score
        ~remainder = go ms result.tally newAlpha beta'
       in
        if newAlpha >= beta' then result else maxBy (.score) (result {tally = remainder.tally}) remainder
  in go nextBoards tally alpha beta

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
