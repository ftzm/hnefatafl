{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hnefatafl.Game.AI (
  Phase (..),
  State (..),
  Event (..),
  TransitionResult (..),
  pending,
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
  PendingAction (..),
  PendingActionType (..),
  TransitionError (..),
  cancelPending,
  clearPending,
  currentBoard,
  currentTurn,
  mkAppliedMove,
  opponent,
  outcomeFromEngine,
  respondToOffer,
  undoMoves,
  validMovesForPosition,
  zobristHashes,
 )
import Optics (AffineTraversal', (.~), (?~), (%), gafield)
import Prelude hiding (State, state)

data Phase
  = PlayerTurn
      { validMoves :: [MoveWithCaptures]
      , pending :: Maybe PendingAction
      }
  | EngineThinking {pending :: Maybe PendingAction}
  | Finished {outcome :: Outcome}
  deriving (Show, Eq, Generic)

data State = State
  { board :: ExternBoard
  , moves :: [AppliedMove]
  , phase :: Phase
  }
  deriving (Show, Eq, Generic)

pending :: AffineTraversal' Phase (Maybe PendingAction)
pending = gafield @"pending"

-- NOTE: Draw/undo offers involving the engine are not yet fully implemented.
-- Currently, pending actions from one side are silently cancelled when the
-- other side moves (via cancelPending). The engine never initiates or responds
-- to draw/undo offers through the event flow. The types and transitions are
-- in place for when this is implemented.
data Event
  = MakeMove Move Time
  | EngineMove AppliedMove (Maybe Outcome)
  | Undo
  | Resign PlayerColor
  | OfferDraw PlayerColor
  | AcceptDraw PlayerColor
  | DeclineDraw PlayerColor
  | Timeout
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , events :: [DomainEvent]
  }
  deriving (Show, Eq)

-- | Construct a PlayerTurn state, computing valid moves from the position.
mkPlayerTurn :: ExternBoard -> [AppliedMove] -> Maybe PendingAction -> State
mkPlayerTurn board moves pa =
  State board moves (PlayerTurn (validMovesForPosition moves) pa)

-- | First argument is the human player's color.
transition ::
  PlayerColor -> State -> Event -> Either TransitionError TransitionResult
transition _ (State _ _ (Finished _)) = const $ Left GameAlreadyFinished
transition humanColor s@(State board moves (PlayerTurn validMoves pend)) = \case
  MakeMove move time
    | move `notElem` map (.move) validMoves -> Left InvalidMove
    | otherwise -> do
        let engineColor = opponent humanColor
            hashes = zobristHashes moves
        (moveResult, engineStatus, _nextValidMoves) <-
          first (const EngineError) $
            nextGameStateWithMovesTrusted board (currentTurn moves == Black) move hashes
        let applied = mkAppliedMove moveResult time
            moves' = moves <> [applied]
            (pending', cancelEvts) = cancelPending engineColor pend
        Right $ case outcomeFromEngine engineStatus of
          Just outcome ->
            TransitionResult
              (State applied.boardAfter moves' (Finished outcome))
              (MovePlayed applied : cancelEvts <> [GameEnded outcome])
          Nothing ->
            TransitionResult
              (State applied.boardAfter moves' (EngineThinking pending'))
              (MovePlayed applied : cancelEvts)
  EngineMove{} -> Left NotYourTurn
  Undo ->
    case undoMoves 2 moves of
      Nothing -> Left NoMovesToUndo
      Just moves' ->
        Right $
          TransitionResult
            (mkPlayerTurn (currentBoard moves') moves' Nothing)
            (clearPending pend <> [MovesUndone 2])
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pend <> [GameEnded outcome])
  OfferDraw color
    | isJust pend -> Left ActionAlreadyPending
    | otherwise ->
        Right $
          TransitionResult
            (s & #phase % pending ?~ PendingAction DrawOffer color)
            [DrawOffered color]
  AcceptDraw color ->
    respondToOffer color pend $
      TransitionResult
        (State board moves (Finished Draw))
        [OfferCancelled, GameEnded Draw]
  DeclineDraw color ->
    respondToOffer color pend $
      TransitionResult
        (s & #phase % pending .~ Nothing)
        [DrawDeclined]
  Timeout ->
    let outcome = TimedOut humanColor
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pend <> [GameEnded outcome])
transition humanColor s@(State board moves (EngineThinking pend)) = \case
  EngineMove applied maybeOutcome ->
    let moves' = moves <> [applied]
        (pending', cancelEvts) = cancelPending humanColor pend
     in Right $ case maybeOutcome of
          Just outcome ->
            TransitionResult
              (State applied.boardAfter moves' (Finished outcome))
              (MovePlayed applied : cancelEvts <> [GameEnded outcome])
          Nothing ->
            TransitionResult
              (mkPlayerTurn applied.boardAfter moves' pending')
              (MovePlayed applied : cancelEvts)
  MakeMove{} -> Left NotYourTurn
  Undo ->
    case undoMoves 1 moves of
      Nothing -> Left NoMovesToUndo
      Just moves' ->
        Right $
          TransitionResult
            (mkPlayerTurn (currentBoard moves') moves' Nothing)
            (clearPending pend <> [MovesUndone 1])
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pend <> [GameEnded outcome])
  OfferDraw color
    | isJust pend -> Left ActionAlreadyPending
    | otherwise ->
        Right $
          TransitionResult
            (s & #phase % pending ?~ PendingAction DrawOffer color)
            [DrawOffered color]
  AcceptDraw color ->
    respondToOffer color pend $
      TransitionResult
        (State board moves (Finished Draw))
        [OfferCancelled, GameEnded Draw]
  DeclineDraw color ->
    respondToOffer color pend $
      TransitionResult
        (s & #phase % pending .~ Nothing)
        [DrawDeclined]
  Timeout ->
    let outcome = TimedOut humanColor
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pend <> [GameEnded outcome])

-- | Reconstruct state from persisted data.
-- If it's the engine's turn, returns EngineThinking — the effectful layer
-- is responsible for re-triggering the search.
reconstruct ::
  PlayerColor ->
  ExternBoard ->
  [AppliedMove] ->
  Maybe Outcome ->
  Maybe PendingAction ->
  State
reconstruct _ board moves (Just outcome) _ = State board moves (Finished outcome)
reconstruct humanColor board moves Nothing pa =
  let turn = currentTurn moves
   in if turn == humanColor
        then mkPlayerTurn board moves pa
        else State board moves (EngineThinking pa)
