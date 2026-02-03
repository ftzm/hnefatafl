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
  , unpaired :: Map Int GameResult
  -- ^ keyed by game id, value is the game result (partner has opposite newAsBlack)
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

-- | Helper: did the "new" engine win this game?
newEngineWon :: Bool -> Player -> Bool
newEngineWon newAsBlack winner = newAsBlack == (winner == Black)

-- | Process a matched pair and update the score state
processPair :: GameSetup -> GameResult -> GameResult -> ScoreState -> ScoreState
processPair setup gameResult partnerResult scoreState =
  let newWonCurrent = newEngineWon setup.newAsBlack gameResult.winner
      newWonPartner = newEngineWon (not setup.newAsBlack) partnerResult.winner
   in case (newWonCurrent, newWonPartner) of
        (True, True) ->
          scoreState & #newPairWins %~ (+ 1)
        (False, False) ->
          scoreState & #oldPairWins %~ (+ 1)
        (True, False) ->
          let moveDiff = partnerResult.moves - gameResult.moves
           in scoreState
                & #moveDifferenceSum %~ (+ moveDiff)
                & #tiedPairCount %~ (+ 1)
        (False, True) ->
          let moveDiff = gameResult.moves - partnerResult.moves
           in scoreState
                & #moveDifferenceSum %~ (+ moveDiff)
                & #tiedPairCount %~ (+ 1)

updateScoreState :: StateUpdate -> ScoreState -> ScoreState
updateScoreState stateUpdate scoreState =
  case stateUpdate of
    StateUpdate _key (GameCompleted setup _moves gameResult) ->
      let gameId = setup.id
       in case lookup gameId scoreState.unpaired of
            Nothing ->
              scoreState & #unpaired %~ insert gameId gameResult
            Just partnerResult ->
              let scoreState' = scoreState & #unpaired %~ delete gameId
               in processPair setup gameResult partnerResult scoreState'
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
      , unpaired = mempty
      , newPairWins = 0
      , oldPairWins = 0
      }

  processCompletedGame scoreState (CompletedGame setup _moves gameResult) =
    let gameId = setup.id
     in case lookup gameId scoreState.unpaired of
          Nothing ->
            scoreState & #unpaired %~ insert gameId gameResult
          Just partnerResult ->
            let scoreState' = scoreState & #unpaired %~ delete gameId
             in processPair setup gameResult partnerResult scoreState'
