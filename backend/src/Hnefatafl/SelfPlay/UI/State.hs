module Hnefatafl.SelfPlay.UI.State (
  ScoreState (..),
  UIState (..),
  UIEvent (..),
  mkInitialUIState,
  mkInitialScoreState,
  updateScoreState,
) where

import Data.Map.Strict (delete, insert, lookup)
import Hnefatafl.Core.Data (MoveResult (..))
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameKey,
  GameResult (..),
  GameSetup (..),
  Outcome (..),
  Player (..),
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  StateUpdatePayload (..),
 )
import Optics ((%~))
import Prelude

-- | We buffer game results by position ID and only count them once all 6 games
-- for a position are complete. This prevents ordering artifacts: if faster-finishing
-- games (e.g., wins from the favored side) complete before slower ones (losses or
-- draws from the unfavored side), we'd see temporary score swings that don't
-- reflect actual engine improvement.
data ScoreState = ScoreState
  { wins :: Int
  -- ^ games won by new engine
  , draws :: Int
  -- ^ drawn games
  , losses :: Int
  -- ^ games lost by new engine
  , pending :: Map Int [GameOutcome]
  -- ^ positionId -> outcomes for completed games, counted when all 6 arrive
  }
  deriving (Show, Eq, Generic)

-- | Outcome of a single game from the new engine's perspective
data GameOutcome = NewWin | NewDraw | NewLoss
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


-- | Determine outcome from new engine's perspective
toGameOutcome :: Bool -> Outcome -> GameOutcome
toGameOutcome newAsBlack = \case
  WonBy Black -> if newAsBlack then NewWin else NewLoss
  WonBy White -> if newAsBlack then NewLoss else NewWin
  Draw -> NewDraw

-- | Count outcomes and add to score
countOutcomes :: [GameOutcome] -> ScoreState -> ScoreState
countOutcomes outcomes s =
  s
    { wins = s.wins + length (filter (== NewWin) outcomes)
    , draws = s.draws + length (filter (== NewDraw) outcomes)
    , losses = s.losses + length (filter (== NewLoss) outcomes)
    }

updateScoreState :: StateUpdate -> ScoreState -> ScoreState
updateScoreState stateUpdate scoreState =
  case stateUpdate of
    StateUpdate _ (GameCompleted setup _ gameResult) ->
      let positionId = setup.id
          outcome = toGameOutcome setup.newAsBlack gameResult.outcome
       in case lookup positionId scoreState.pending of
            Nothing ->
              scoreState & #pending %~ insert positionId [outcome]
            Just existing ->
              let updated = outcome : existing
               in if length updated == 6
                    then
                      countOutcomes updated $
                        scoreState & #pending %~ delete positionId
                    else scoreState & #pending %~ insert positionId updated
    _ -> scoreState

-- | Build initial ScoreState from completed games
mkInitialScoreState :: [CompletedGame] -> ScoreState
mkInitialScoreState completedGames =
  foldl' processCompletedGame initialScoreState completedGames
 where
  initialScoreState =
    ScoreState
      { wins = 0
      , draws = 0
      , losses = 0
      , pending = mempty
      }

  processCompletedGame scoreState (CompletedGame setup _ gameResult) =
    let positionId = setup.id
        outcome = toGameOutcome setup.newAsBlack gameResult.outcome
     in case lookup positionId scoreState.pending of
          Nothing ->
            scoreState & #pending %~ insert positionId [outcome]
          Just existing ->
            let updated = outcome : existing
             in if length updated == 6
                  then
                    countOutcomes updated $
                      scoreState & #pending %~ delete positionId
                  else scoreState & #pending %~ insert positionId updated
