module Hnefatafl.Game.Hotseat (
  State (..),
  Event (..),
  Command (..),
  TransitionResult (..),
  transition,
  reconstruct,
) where

import Hnefatafl.Core.Data (PlayerColor (..))
import Hnefatafl.Game.Common (
  AppliedMove (..),
  Outcome (..),
  PendingAction,
  PersistenceCommand (..),
  TransitionError (..),
  currentTurn,
  opponent,
  undoMoves,
 )
import Prelude hiding (State)

data State
  = AwaitingMove PlayerColor [AppliedMove]
  | Finished Outcome
  deriving (Show, Eq)

data Event
  = MakeMove PlayerColor AppliedMove (Maybe Outcome)
  | Undo PlayerColor
  | Resign PlayerColor
  | AgreeDraw
  | Timeout PlayerColor
  deriving (Show, Eq)

data Command = Persist PersistenceCommand
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , commands :: [Command]
  }
  deriving (Show, Eq)

transition :: State -> Event -> Either TransitionError TransitionResult
transition (Finished _) = const $ Left GameAlreadyFinished
transition (AwaitingMove turn moves) = \case
  MakeMove color applied maybeOutcome
    | color /= turn -> Left NotYourTurn
    | Just outcome <- maybeOutcome ->
        Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ PersistMove applied
            , Persist $ PersistOutcome outcome
            ]
    | otherwise ->
        Right $
          TransitionResult
            (AwaitingMove (opponent turn) (moves <> [applied]))
            [Persist $ PersistMove applied]
  Undo _color ->
    case undoMoves 1 moves of
      Nothing -> Left NoMovesToUndo
      Just moves' ->
        Right $
          TransitionResult
            (AwaitingMove (opponent turn) moves')
            [Persist $ DeleteMoves 1]
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (Finished outcome)
            [Persist $ PersistOutcome outcome]
  AgreeDraw ->
    Right $
      TransitionResult
        (Finished Draw)
        [Persist $ PersistOutcome Draw]
  Timeout color ->
    let outcome = TimedOut color
     in Right $
          TransitionResult
            (Finished outcome)
            [Persist $ PersistOutcome outcome]

-- | Reconstruct state from persisted data
reconstruct :: [AppliedMove] -> Maybe Outcome -> Maybe PendingAction -> State
reconstruct _ (Just outcome) _ = Finished outcome
reconstruct moves Nothing _ = AwaitingMove (currentTurn moves) moves
