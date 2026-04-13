module Hnefatafl.Game.Hotseat (
  Phase (..),
  State (..),
  Event (..),
  TransitionResult (..),
  transition,
  reconstruct,
) where

import Chronos (Time)
import Hnefatafl.Bindings (nextGameStateWithMovesTrusted)
import Hnefatafl.Core.Data (
  ExternBoard,
  Move (..),
  MoveWithCaptures (..),
  Outcome (..),
  PlayerColor (..),
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  DomainEvent (..),
  TransitionError (..),
  currentBoard,
  currentTurn,
  mkAppliedMove,
  opponent,
  outcomeFromEngine,
  undoMoves,
  validMovesForPosition,
  zobristHashes,
 )
import Prelude hiding (State)

data Phase
  = Awaiting PlayerColor [MoveWithCaptures]
  | Finished Outcome
  deriving (Show, Eq)

data State = State ExternBoard [AppliedMove] Phase
  deriving (Show, Eq)

data Event
  = MakeMove Move Time
  | Undo
  | Resign PlayerColor
  | AgreeDraw
  | Timeout PlayerColor
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , events :: [DomainEvent]
  }
  deriving (Show, Eq)

-- | Construct an Awaiting state, computing valid moves from the position.
mkAwaiting :: ExternBoard -> [AppliedMove] -> State
mkAwaiting board moves =
  State board moves (Awaiting (currentTurn moves) (validMovesForPosition moves))

transition :: State -> Event -> Either TransitionError TransitionResult
transition (State _ _ (Finished _)) = const $ Left GameAlreadyFinished
transition (State board moves (Awaiting turn validMoves)) = \case
  MakeMove move time
    | move `notElem` map (.move) validMoves -> Left InvalidMove
    | otherwise -> do
        let hashes = zobristHashes moves
        (moveResult, engineStatus, nextValidMoves) <-
          first (const EngineError) $
            nextGameStateWithMovesTrusted board (turn == Black) move hashes
        let applied = mkAppliedMove moveResult time
            moves' = moves <> [applied]
        Right $ case outcomeFromEngine engineStatus of
          Just outcome ->
            TransitionResult
              (State applied.boardAfter moves' (Finished outcome))
              [MovePlayed applied, GameEnded outcome]
          Nothing ->
            TransitionResult
              (State applied.boardAfter moves' (Awaiting (opponent turn) nextValidMoves))
              [MovePlayed applied]
  Undo ->
    case undoMoves 1 moves of
      Nothing -> Left NoMovesToUndo
      Just moves' ->
        Right $
          TransitionResult
            (mkAwaiting (currentBoard moves') moves')
            [MovesUndone 1]
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            [GameEnded outcome]
  AgreeDraw ->
    Right $
      TransitionResult
        (State board moves (Finished Draw))
        [GameEnded Draw]
  Timeout color ->
    let outcome = TimedOut color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            [GameEnded outcome]

-- | Reconstruct state from persisted data
reconstruct :: ExternBoard -> [AppliedMove] -> Maybe Outcome -> State
reconstruct board moves = \case
  (Just outcome) -> State board moves (Finished outcome)
  Nothing -> mkAwaiting board moves
