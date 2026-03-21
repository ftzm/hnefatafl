{-# LANGUAGE BlockArguments #-}

module TestUtil where

import Hnefatafl.Bindings (EngineGameStatus, applyMoveSequence)
import Hnefatafl.Core.Data (MoveResult)
import Hnefatafl.Serialization (parseMoveList)

-- | Real game move results with actual board states
realMoveResults :: NonEmpty MoveResult
realMoveResults = fst realGameData

-- | Final game status from the real game
realGameFinalStatus :: EngineGameStatus
realGameFinalStatus = snd realGameData

-- | Real game data including both move results and final status
realGameData :: (NonEmpty MoveResult, EngineGameStatus)
realGameData =
  let moveString =
        "d11-d9 h6-h3 k7-i7 f8-c8 a4-c4 f4-i4 g1-g2 e5-c5 d1-d3 g5-j5 k4-j4xj5 f5-j5 h11-h4xi4 c5-h5xh4 i7-i5xj5 g7-g9 g2-g5xh5 f7-k7 i5-i9 f6-f8 j6-h6xg6 f8-j8 i9-j9 j8-i8 k6-i6 i8-i11 j9-j11 i11-i10 j11-j10 g9-k9xk8 g11-i11 k9-k10xj10 j4-j10 k10-k9 h6-h10 k9-k10xj10 d9-k9xk10 i10-k10 h10-j10 k10-k11"
      parsedMoves = case parseMoveList moveString of
        Left err -> error $ "Failed to parse real game moves: " <> err
        Right moves -> moves
   in applyMoveSequence parsedMoves