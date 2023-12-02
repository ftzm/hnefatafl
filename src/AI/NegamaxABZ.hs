{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

module AI.NegamaxABZ where

import AI.Assessment (scoreBoard)
import Board.Board (Board, Team (..), opp, showBoard)
import Board.Move (
  nextMoveBoardsBlack',
  nextMoveBoardsBlackZ,
  nextMoveBoardsBlackZ',
  nextMoveBoardsWhite',
  nextMoveBoardsWhiteZ,
  nextMoveBoardsWhiteZ',
 )
import Board.Zobrist (MultiZobrist (..), Zobrist (..), boardToMultiZobrist, boardToSingleZobrist, selectZobrist, unZobrist)
import Data.Data (Data)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Vector.Hashtables as VH
import Data.Vector.Storable.Mutable qualified as VM
import Data.Vector.Unboxed qualified as V
import Foreign.Storable (Storable (..))
import Foreign.Storable.Record as Store
import System.IO.Unsafe (unsafePerformIO)

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = if f x >= f y then x else y

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = if f x <= f y then x else y

type Depth = Int

type Score = Int

type Tally = Int

type Visited = Int

type Alpha = Int

type Beta = Int

data ZobristStats = ZobristStats
  { misses :: Integer
  , unrotatedHits :: Integer
  , rotatedHits :: Integer
  , hitDepth :: Map Int Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

data ZobristResult = ZobristResult
  { move :: (Int8, Int8)
  , board :: Board
  , score :: Score
  , tally :: Visited
  , zobrist :: MultiZobrist
  }
  deriving stock (Data, Generic, Eq)
  deriving anyclass (NFData)

data ZobristResult' = ZobristResult'
  { move :: (Int8, Int8)
  , board :: Board
  , score :: Score
  , tally :: Visited
  , zobrist :: Zobrist
  }
  deriving stock (Data, Generic, Eq)
  deriving anyclass (NFData)

negamaxABZ ::
  (Int8, Int8) ->
  Board ->
  Team ->
  Depth ->
  Tally ->
  Alpha ->
  Beta ->
  MultiZobrist ->
  IORef ZobristStats ->
  IORef VectorHashTable ->
  IO ZobristResult
negamaxABZ move board current depth tally alpha beta z zsr htr =
  let
    inner :: Int -> Int -> IO ZobristResult
    inner innerAlpha innerBeta
      | depth == 0 =
          pure $
            ZobristResult
              move
              board
              (scoreBoard current board)
              (tally + 1)
              z
      | otherwise =
          let
            nextBoards =
              if depth > 1
                then case current of
                  White ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsWhiteZ board z
                  Black ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsBlackZ board z
                else case current of
                  White ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsWhite' board
                  Black ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsBlack' board

            initial =
              L.head nextBoards & \(move', board', zobrist') -> do
                result <-
                  negamaxABZ
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
                pure $
                  ZobristResult
                    move'
                    board'
                    (negate result.score)
                    result.tally
                    zobrist'

            go ::
              ZobristResult ->
              [((Int8, Int8), Board, MultiZobrist)] ->
              Alpha ->
              Beta ->
              IO ZobristResult
            go prev [] _ _ = pure prev
            go !prev ((!move', !board', !zobrist') : ms) !alpha' !beta' = do
              let newAlpha = max prev.score alpha'
              if newAlpha >= beta'
                then pure prev
                else do
                  deepResult <-
                    negamaxABZ
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
                  let result =
                        ZobristResult move' board' (negate deepResult.score) deepResult.tally zobrist'
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
        selectedHash = selectZobrist z
        isRotatedHash = selectedHash /= z.rotated0
      ht <- readIORef htr
      VH.lookup ht selectedHash >>= \case
        Just
          ( TranspositionEntry
              { entryDepth
              , entryScore
              , entryBound
              , entryBoard
              , entryBestOrigin
              , entryBestDest
              }
            ) | entryDepth >= depth ->
            do
              -- putStrLn "--------------------------------------------"
              -- putStrLn $ showBoard board
              -- putStrLn ""
              -- putStrLn $ showBoard cachedBoard
              modifyIORef
                zsr
                ( \zs' ->
                    if isRotatedHash
                      then
                        zs'
                          { rotatedHits = zs'.rotatedHits + 1
                          , hitDepth = M.alter (Just . maybe 1 (+ 1)) depth zs'.hitDepth
                          }
                      else
                        zs'
                          { unrotatedHits = zs'.unrotatedHits + 1
                          , hitDepth = M.alter (Just . maybe 1 (+ 1)) depth zs'.hitDepth
                          }
                )
              pure $ case entryBound of
                -- exact
                2 -> (Just entryScore, alpha, beta)
                -- lowerbound
                1 ->
                  let alphaOverride = max alpha entryScore
                   in if alphaOverride >= beta
                        then (Just entryScore, alphaOverride, beta)
                        else (Nothing, alphaOverride, beta)
                -- upperbound
                _ ->
                  let betaOverride = min beta entryScore
                   in if alpha >= betaOverride
                        then (Just entryScore, alpha, betaOverride)
                        else (Nothing, alpha, betaOverride)
        _ -> do
          modifyIORef zsr (\zs' -> zs'{misses = zs'.misses + 1})
          pure (Nothing, alpha, beta)
   in
    do
      (ttScore, ttAlpha, ttBeta) <-
        if depth > 0 then getTransposition else pure (Nothing, alpha, beta)
      case ttScore of
        Just s -> pure $ ZobristResult move board s tally z
        Nothing -> do
          result <- inner ttAlpha ttBeta

          when (depth > 0) $
            let flag =
                  if
                      | result.score <= alpha -> 3
                      | result.score >= beta -> 1
                      | otherwise -> 2
             in do
                  ht <- readIORef htr
                  VH.insert
                    ht
                    (selectZobrist z)
                    ( TranspositionEntry
                        depth
                        result.score
                        flag
                        board
                        (fst result.move)
                        (snd result.move)
                    )

          pure result

data TranspositionEntry = TranspositionEntry
  { entryDepth :: Int
  , entryScore :: Int
  , entryBound :: Int
  , entryBoard :: Board
  , entryBestOrigin :: Int8
  , entryBestDest :: Int8
  }

store :: Store.Dictionary TranspositionEntry
store =
  Store.run
    ( TranspositionEntry
        <$> (Store.element entryDepth)
        <*> (Store.element entryScore)
        <*> (Store.element entryBound)
        <*> (Store.element entryBoard)
        <*> (Store.element entryBestOrigin)
        <*> (Store.element entryBestDest)
    )

instance Storable TranspositionEntry where
  sizeOf = Store.sizeOf store
  alignment = Store.alignment store
  peek = Store.peek store
  poke = Store.poke store

type VectorHashTable =
  VH.Dictionary
    (PrimState IO)
    VM.MVector
    Word64
    VM.MVector
    TranspositionEntry

-- ( Int -- depth
-- , Int -- score
-- , Int -- 1 = lowerbound | 2 = exact | 3 = upperbound
-- , Board -- board
-- , (Int8, Int8) -- best move
-- )

runSearch :: Board -> Team -> IO (ZobristResult, ZobristStats)
runSearch board team = do
  ht <- VH.initialize 10000 :: IO VectorHashTable
  htr <- newIORef ht
  zsr <- newIORef $ ZobristStats 0 0 0 mempty
  let startZobrist = boardToMultiZobrist board True
  result <-
    negamaxABZ
      (0, 0)
      board
      team
      2
      0
      (minBound + 10)
      (maxBound - 10)
      startZobrist
      zsr
      htr
  statResults <- readIORef zsr
  pure (result, statResults)

nextBoardNABZ :: Board -> Team -> Board
nextBoardNABZ board team = unsafePerformIO $ do
  result <- fst <$> runSearch board team
  let nextBoards = case team of
        Black -> nextMoveBoardsBlack' board
        White -> nextMoveBoardsWhite' board
  pure $ fromJust $ L.lookup result.move nextBoards

--------------------------------------------------------------------------------


negamaxABZ' ::
  (Int8, Int8) ->
  Board ->
  Team ->
  Depth ->
  Tally ->
  Alpha ->
  Beta ->
  Zobrist ->
  IORef ZobristStats ->
  IORef VectorHashTable ->
  IO ZobristResult'
negamaxABZ' move board current depth tally alpha beta z zsr htr =
  let
    inner :: Int -> Int -> IO ZobristResult'
    inner innerAlpha innerBeta
      | depth == 0 =
          pure $
            ZobristResult'
              move
              board
              (scoreBoard current board)
              (tally + 1)
              z
      | otherwise =
          let
            nextBoards =
              if depth > 1
                then case current of
                  White ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsWhiteZ' board z
                  Black ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsBlackZ' board z
                else case current of
                  White ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsWhite' board
                  Black ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsBlack' board

            initial =
              L.head nextBoards & \(move', board', zobrist') -> do
                result <-
                  negamaxABZ'
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
                pure $
                  ZobristResult'
                    move'
                    board'
                    (negate result.score)
                    result.tally
                    zobrist'

            go ::
              ZobristResult' ->
              [((Int8, Int8), Board, Zobrist)] ->
              Alpha ->
              Beta ->
              IO ZobristResult'
            go prev [] _ _ = pure prev
            go !prev ((!move', !board', !zobrist') : ms) !alpha' !beta' = do
              let newAlpha = max prev.score alpha'
              if newAlpha >= beta'
                then pure prev
                else do
                  deepResult <-
                    negamaxABZ'
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
                  let result =
                        ZobristResult' move' board' (negate deepResult.score) deepResult.tally zobrist'
                  let best = maxBy (.score) (prev{tally = result.tally} :: ZobristResult') result
                  go best ms newAlpha beta'
           in
            do
              i' <- initial
              result <- go i' (L.tail nextBoards) innerAlpha innerBeta
              pure $ ZobristResult' result.move result.board result.score result.tally z

    getTransposition :: IO (Maybe Int, Int, Int)
    getTransposition = do
      ht <- readIORef htr
      VH.lookup ht (unZobrist z) >>= \case
        Just
          ( TranspositionEntry
              { entryDepth
              , entryScore
              , entryBound
              , entryBoard
              , entryBestOrigin
              , entryBestDest
              }
            ) | entryDepth >= depth ->
            do
              -- putStrLn "--------------------------------------------"
              -- putStrLn $ showBoard board
              -- putStrLn ""
              -- putStrLn $ showBoard cachedBoard
              modifyIORef
                zsr
                ( \zs' ->
                        zs'
                          { unrotatedHits = zs'.unrotatedHits + 1
                          , hitDepth = M.alter (Just . maybe 1 (+ 1)) depth zs'.hitDepth
                          }
                )
              pure $ case entryBound of
                -- exact
                2 -> (Just entryScore, alpha, beta)
                -- lowerbound
                1 ->
                  let alphaOverride = max alpha entryScore
                   in if alphaOverride >= beta
                        then (Just entryScore, alphaOverride, beta)
                        else (Nothing, alphaOverride, beta)
                -- upperbound
                _ ->
                  let betaOverride = min beta entryScore
                   in if alpha >= betaOverride
                        then (Just entryScore, alpha, betaOverride)
                        else (Nothing, alpha, betaOverride)
        _ -> do
          modifyIORef zsr (\zs' -> zs'{misses = zs'.misses + 1})
          pure (Nothing, alpha, beta)
   in
    do
      (ttScore, ttAlpha, ttBeta) <-
        if depth > 0 then getTransposition else pure (Nothing, alpha, beta)
      case ttScore of
        Just s -> pure $ ZobristResult' move board s tally z
        Nothing -> do
          result <- inner ttAlpha ttBeta

          when (depth > 0) $
            let flag =
                  if
                      | result.score <= alpha -> 3
                      | result.score >= beta -> 1
                      | otherwise -> 2
             in do
                  ht <- readIORef htr
                  VH.insert
                    ht
                    (unZobrist z)
                    ( TranspositionEntry
                        depth
                        result.score
                        flag
                        board
                        (fst result.move)
                        (snd result.move)
                    )

          pure result

runSearch' :: Board -> Team -> IO (ZobristResult', ZobristStats)
runSearch' board team = do
  ht <- VH.initialize 10000 :: IO VectorHashTable
  htr <- newIORef ht
  zsr <- newIORef $ ZobristStats 0 0 0 mempty
  let startZobrist = boardToSingleZobrist board True
  result <-
    negamaxABZ'
      (0, 0)
      board
      team
      4
      0
      (minBound + 10)
      (maxBound - 10)
      startZobrist
      zsr
      htr
  statResults <- readIORef zsr
  pure (result, statResults)

--------------------------------------------------------------------------------


negamaxABZ'' ::
  (Int8, Int8) ->
  Board ->
  Team ->
  Depth ->
  Tally ->
  Alpha ->
  Beta ->
  Zobrist ->
  IO ZobristResult'
negamaxABZ'' move board current depth tally alpha beta z =
  let
    inner :: Int -> Int -> IO ZobristResult'
    inner innerAlpha innerBeta
      | depth == 0 =
          pure $
            ZobristResult'
              move
              board
              (scoreBoard current board)
              (tally + 1)
              z
      | otherwise =
          let
            nextBoards =
              --if depth > 1
              if False
                then case current of
                  White ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsWhiteZ' board z
                  Black ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsBlackZ' board z
                else case current of
                  White ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsWhite' board
                  Black ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsBlack' board

            initial =
              L.head nextBoards & \(move', board', zobrist') -> do
                result <-
                  negamaxABZ''
                    move'
                    board'
                    (opp current)
                    (depth - 1)
                    tally
                    (negate innerBeta)
                    (negate innerAlpha)
                    zobrist'
                pure $
                  ZobristResult'
                    move'
                    board'
                    (negate result.score)
                    result.tally
                    zobrist'

            go ::
              ZobristResult' ->
              [((Int8, Int8), Board, Zobrist)] ->
              Alpha ->
              Beta ->
              IO ZobristResult'
            go prev [] _ _ = pure prev
            go !prev ((!move', !board', !zobrist') : ms) !alpha' !beta' = do
              let newAlpha = max prev.score alpha'
              if newAlpha >= beta'
                then pure prev
                else do
                  deepResult <-
                    negamaxABZ''
                      move'
                      board'
                      (opp current)
                      (depth - 1)
                      prev.tally
                      (negate innerBeta)
                      (negate newAlpha)
                      zobrist'
                  let result =
                        ZobristResult' move' board' (negate deepResult.score) deepResult.tally zobrist'
                  let best = maxBy (.score) (prev{tally = result.tally} :: ZobristResult') result
                  go best ms newAlpha beta'
           in
            do
              i' <- initial
              result <- go i' (L.tail nextBoards) innerAlpha innerBeta
              pure $ ZobristResult' result.move result.board result.score result.tally z

   in
    do
      (ttScore, ttAlpha, ttBeta) <-
        pure (Nothing, alpha, beta)
      case ttScore of
        Just s -> pure $ ZobristResult' move board s tally z
        Nothing -> do
          result <- inner ttAlpha ttBeta
          pure result

runSearch'' :: Board -> Team -> IO (ZobristResult', ZobristStats)
runSearch'' board team = do
  ht <- VH.initialize 10000 :: IO VectorHashTable
  htr <- newIORef ht
  zsr <- newIORef $ ZobristStats 0 0 0 mempty
  let startZobrist = boardToSingleZobrist board True
  result <-
    negamaxABZ''
      (0, 0)
      board
      team
      4
      0
      (minBound + 10)
      (maxBound - 10)
      startZobrist
  statResults <- readIORef zsr
  pure (result, statResults)

--------------------------------------------------------------------------------

negamaxABZ''' ::
  (Int8, Int8) ->
  Board ->
  Team ->
  Depth ->
  Tally ->
  Alpha ->
  Beta ->
  MultiZobrist ->
  IORef ZobristStats ->
  IORef VectorHashTable ->
  IO ZobristResult
negamaxABZ''' move board current depth tally alpha beta z zsr htr =
  let
    inner :: Int -> Int -> IO ZobristResult
    inner innerAlpha innerBeta
      | depth == 0 =
          pure $
            ZobristResult
              move
              board
              (scoreBoard current board)
              (tally + 1)
              z
      | otherwise =
          let
            nextBoards =
              if depth > 1
                then case current of
                  White ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsWhiteZ board z
                  Black ->
                    map (\(m, b, z') -> (m, b, z')) $
                      nextMoveBoardsBlackZ board z
                else case current of
                  White ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsWhite' board
                  Black ->
                    map (\(m, b) -> (m, b, z)) $
                      nextMoveBoardsBlack' board

            initial =
              L.head nextBoards & \(move', board', zobrist') -> do
                result <-
                  negamaxABZ'''
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
                pure $
                  ZobristResult
                    move'
                    board'
                    (negate result.score)
                    result.tally
                    zobrist'

            go ::
              ZobristResult ->
              [((Int8, Int8), Board, MultiZobrist)] ->
              Alpha ->
              Beta ->
              IO ZobristResult
            go prev [] _ _ = pure prev
            go !prev ((!move', !board', !zobrist') : ms) !alpha' !beta' = do
              let newAlpha = max prev.score alpha'
              if newAlpha >= beta'
                then pure prev
                else do
                  deepResult <-
                    negamaxABZ'''
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
                  let result =
                        ZobristResult move' board' (negate deepResult.score) deepResult.tally zobrist'
                  let best = maxBy (.score) (prev{tally = result.tally} :: ZobristResult) result
                  go best ms newAlpha beta'
           in
            do
              i' <- initial
              result <- go i' (L.tail nextBoards) innerAlpha innerBeta
              pure $ ZobristResult result.move result.board result.score result.tally z
              ---

    getTransposition :: IO (Maybe Int, Int, Int)
    getTransposition = do
      let
        selectedHash = selectZobrist z
        isRotatedHash = selectedHash /= z.rotated0
      ht <- readIORef htr
      VH.lookup ht selectedHash >>= \case
        Just
          ( TranspositionEntry
              { entryDepth
              , entryScore
              , entryBound
              , entryBoard
              , entryBestOrigin
              , entryBestDest
              }
            ) | entryDepth >= depth ->
            do
              -- putStrLn "--------------------------------------------"
              -- putStrLn $ showBoard board
              -- putStrLn ""
              -- putStrLn $ showBoard cachedBoard
              modifyIORef
                zsr
                ( \zs' ->
                    if isRotatedHash
                      then
                        zs'
                          { rotatedHits = zs'.rotatedHits + 1
                          , hitDepth = M.alter (Just . maybe 1 (+ 1)) depth zs'.hitDepth
                          }
                      else
                        zs'
                          { unrotatedHits = zs'.unrotatedHits + 1
                          , hitDepth = M.alter (Just . maybe 1 (+ 1)) depth zs'.hitDepth
                          }
                )
              pure $ case entryBound of
                -- exact
                2 -> (Just entryScore, alpha, beta)
                -- lowerbound
                1 ->
                  let alphaOverride = max alpha entryScore
                   in if alphaOverride >= beta
                        then (Just entryScore, alphaOverride, beta)
                        else (Nothing, alphaOverride, beta)
                -- upperbound
                _ ->
                  let betaOverride = min beta entryScore
                   in if alpha >= betaOverride
                        then (Just entryScore, alpha, betaOverride)
                        else (Nothing, alpha, betaOverride)
        _ -> do
          modifyIORef zsr (\zs' -> zs'{misses = zs'.misses + 1})
          pure (Nothing, alpha, beta)
   in
    do
      (ttScore, ttAlpha, ttBeta) <-
        if depth > 0 then getTransposition else pure (Nothing, alpha, beta)
      case ttScore of
        Just s -> pure $ ZobristResult move board s tally z
        Nothing -> do
          result <- inner ttAlpha ttBeta

          when (depth > 0) $
            let flag =
                  if
                      | result.score <= alpha -> 3
                      | result.score >= beta -> 1
                      | otherwise -> 2
             in do
                  ht <- readIORef htr
                  VH.insert
                    ht
                    (selectZobrist z)
                    ( TranspositionEntry
                        depth
                        result.score
                        flag
                        board
                        (fst result.move)
                        (snd result.move)
                    )

          pure result

runSearch''' :: Board -> Team -> IO (ZobristResult, ZobristStats)
runSearch''' board team = do
  ht <- VH.initialize 10000 :: IO VectorHashTable
  htr <- newIORef ht
  zsr <- newIORef $ ZobristStats 0 0 0 mempty
  let startZobrist = boardToMultiZobrist board True
  result <-
    negamaxABZ'''
      (0, 0)
      board
      team
      4
      0
      (minBound + 10)
      (maxBound - 10)
      startZobrist
      zsr
      htr
  statResults <- readIORef zsr
  pure (result, statResults)
