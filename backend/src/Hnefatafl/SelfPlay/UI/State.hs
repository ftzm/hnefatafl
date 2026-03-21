module Hnefatafl.SelfPlay.UI.State (
  ScoreState (..),
  UIState (..),
  UIEvent (..),
  mkInitialUIState,
  mkInitialScoreState,
  updateScoreState,
) where

import Data.List (partition)
import Data.Map.Strict (delete, insert, lookup)
import Hnefatafl.Core.Data (MoveResult (..))
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameKey,
  GameResult (..),
  GameSetup (..),
  Player (..),
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  StateUpdatePayload (..),
 )
import Optics ((%~))
import Prelude

data ScoreState = ScoreState
  { moveDifferenceSum :: Int
  -- ^ running sum of (old_moves - new_moves) for tied pairs
  , tiedPairCount :: Int
  -- ^ count of pairs where each engine won once
  , pending :: Map Int [(Bool, GameResult)]
  -- ^ keyed by game id, collecting (newAsBlack, result) pairs until all 6 plays complete
  , newPairWins :: Int
  -- ^ count of pairs where new engine won both games
  , oldPairWins :: Int
  -- ^ count of pairs where old engine won both games
  }
  deriving (Show, Eq, Generic)

-- | Internal state for the Brick application
data UIState = UIState
  { totalGames :: Int
  , completedGames :: Int
  , recentEvents :: [StateUpdate]
  , ongoingGames :: Map GameKey MoveResult
  , processingComplete :: Bool
  , scoreState :: ScoreState
  }
  deriving (Show, Eq, Generic)

-- | Events that can be sent to the Brick application
data UIEvent
  = SelfPlayEvent StateUpdate
  | Tick
  deriving (Show, Eq)

-- | Create initial UI state from ProcessingStateSnapshot
mkInitialUIState :: ProcessingStateSnapshot -> UIState
mkInitialUIState snapshot =
  UIState
    { totalGames = length snapshot.unprocessedGames + length snapshot.completedGames
    , completedGames = length snapshot.completedGames
    , recentEvents = []
    , ongoingGames = mempty
    , processingComplete = False
    , scoreState = mkInitialScoreState snapshot.completedGames
    }


-- | Aggregate results for one side (e.g., all plays where newAsBlack=True).
-- Returns (newEngineWon, avgWinningMoves).
-- Crashes if given an even or zero number of games.
aggregateSide :: Bool -> [GameResult] -> (Bool, Int)
aggregateSide newAsBlack results =
  let len = length results
   in if len == 0 || even len
        then error $ "aggregateSide: expected odd number of games, got " <> show len
        else
          let blackWins = filter ((== Black) . (.winner)) results
              whiteWins = filter ((== White) . (.winner)) results
              blackWonMajority = length blackWins > length whiteWins
              winningGames = if blackWonMajority then blackWins else whiteWins
              avgMoves = sum (map (.moves) winningGames) `div` length winningGames
              newWon = newAsBlack == blackWonMajority
           in (newWon, avgMoves)

-- | Process all plays for a game ID (6 total: 3 per side) and update the score state.
processAllPlays :: [(Bool, GameResult)] -> ScoreState -> ScoreState
processAllPlays plays scoreState =
  let (side1Plays, side2Plays) = partition fst plays
      (newWonSide1, side1Moves) = aggregateSide True (map snd side1Plays)
      (newWonSide2, side2Moves) = aggregateSide False (map snd side2Plays)
   in case (newWonSide1, newWonSide2) of
        (True, True) -> scoreState & #newPairWins %~ (+ 1)
        (False, False) -> scoreState & #oldPairWins %~ (+ 1)
        _ ->
          -- tie: one side won by each engine
          let (newMoves, oldMoves) =
                if newWonSide1 then (side1Moves, side2Moves) else (side2Moves, side1Moves)
           in scoreState
                & #moveDifferenceSum %~ (+ (oldMoves - newMoves))
                & #tiedPairCount %~ (+ 1)

updateScoreState :: StateUpdate -> ScoreState -> ScoreState
updateScoreState stateUpdate scoreState =
  case stateUpdate of
    StateUpdate _key (GameCompleted setup _moves gameResult) ->
      let gameId = setup.id
          entry = (setup.newAsBlack, gameResult)
       in case lookup gameId scoreState.pending of
            Nothing ->
              scoreState & #pending %~ insert gameId [entry]
            Just existing ->
              let updated = entry : existing
               in if length updated == 6
                    then processAllPlays updated (scoreState & #pending %~ delete gameId)
                    else scoreState & #pending %~ insert gameId updated
    _ -> scoreState

-- | Build initial ScoreState from completed games
mkInitialScoreState :: [CompletedGame] -> ScoreState
mkInitialScoreState completedGames =
  foldl' processCompletedGame initialScoreState completedGames
 where
  initialScoreState =
    ScoreState
      { moveDifferenceSum = 0
      , tiedPairCount = 0
      , pending = mempty
      , newPairWins = 0
      , oldPairWins = 0
      }

  processCompletedGame scoreState (CompletedGame setup _moves gameResult) =
    let gameId = setup.id
        entry = (setup.newAsBlack, gameResult)
     in case lookup gameId scoreState.pending of
          Nothing ->
            scoreState & #pending %~ insert gameId [entry]
          Just existing ->
            let updated = entry : existing
             in if length updated == 6
                  then processAllPlays updated (scoreState & #pending %~ delete gameId)
                  else scoreState & #pending %~ insert gameId updated
