{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.Search.Test (
  runSearchTest,
  TestSearchConfig (..),
) where

import Data.Map.Strict qualified as Map
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Hnefatafl.Bindings (
  EngineGameStatus (..),
  SearchTrustedResult (..),
  applyMoveSequence,
 )
import Hnefatafl.Core.Data (Move (..), MoveResult (..))
import Hnefatafl.Effect.Search (Search (..))
import Hnefatafl.Serialization (parseMoveList)

-- | Configuration for the test search interpreter
newtype TestSearchConfig = TestSearchConfig {gameNotation :: Text}
  deriving (Show, Eq)

-- | Internal state for the test interpreter
data TestSearchState = TestSearchState
  { hashToNextMove :: Map Word64 (MoveResult, EngineGameStatus)
  -- ^ Maps zobrist hash of current position to the next move result and its game status
  , firstMove :: (MoveResult, EngineGameStatus)
  -- ^ The first move in the game sequence with its game status
  }

-- | Check if all elements of a list are unique
elemsUnique :: Ord a => [a] -> Bool
elemsUnique xs = length xs == length (ordNub xs)

-- | Create test search state from parsed moves
mkTestSearchState :: NonEmpty Move -> Either Text TestSearchState
mkTestSearchState moves =
  let (moveResults, finalStatus) = applyMoveSequence moves
      firstMoveResult = head moveResults
      allMoveResultsList = toList moveResults
      hashesUnique = elemsUnique $ map (.zobristHash) allMoveResultsList
      gameStatuses = replicate (length allMoveResultsList - 1) EngineOngoing ++ [finalStatus]
      moveResultsWithStatus = zip allMoveResultsList gameStatuses
      hashMap =
        Map.fromList $
          zip
            (map (zobristHash . fst) moveResultsWithStatus)
            (drop 1 moveResultsWithStatus)
   in if hashesUnique
        then Right $ TestSearchState hashMap (firstMoveResult, EngineOngoing)
        else Left "Zobrist hashes must be unique"

-- | Run the test search interpreter
-- The interpreter uses a predetermined game sequence to provide deterministic responses
runSearchTest ::
  Error Text :> es =>
  TestSearchConfig ->
  Eff (Search : es) a ->
  Eff es a
runSearchTest config =
  case parseMoveList config.gameNotation of
    Left err -> \_ -> throwError $ "Failed to parse move notation: " <> err
    Right moves ->
      case mkTestSearchState moves of
        Left err -> \_ -> throwError $ "Failed to create test search state: " <> err
        Right testState -> interpret $ \_ -> \case
          SearchTrusted _board _blackToMove hashes _timeout -> do
            case hashes of
              -- Empty hash list means we're at the start - return the first move
              [] -> pure $ uncurry moveResultToSearchResult testState.firstMove
              -- Look up the current position's hash to find the next move
              (currentHash : _) -> do
                case Map.lookup currentHash testState.hashToNextMove of
                  Nothing -> throwError $ ("No next move found for hash: " :: Text) <> show currentHash
                  Just (nextMoveResult, gameStatus) -> pure $ moveResultToSearchResult nextMoveResult gameStatus

-- | Convert a MoveResult to SearchTrustedResult with game status
moveResultToSearchResult ::
  MoveResult -> EngineGameStatus -> SearchTrustedResult
moveResultToSearchResult moveResult gameStatus =
  SearchTrustedResult
    { searchMove = moveResult.move
    , updatedBoard = moveResult.board
    , updatedZobristHash = moveResult.zobristHash
    , gameStatus = gameStatus
    }
