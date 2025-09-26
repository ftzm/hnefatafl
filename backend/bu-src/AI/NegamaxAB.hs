module AI.NegamaxAB where

import Data.List qualified as L
import Board.Board (Board, Team (..), opp)
import Board.Move (nextMoveBoardsWhite', nextMoveBoardsBlack')
import AI.Assessment (scoreBoard)

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

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = if f x >= f y then x else y

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = if f x <= f y then x else y

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
