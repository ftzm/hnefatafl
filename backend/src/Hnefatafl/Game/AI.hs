module Hnefatafl.Game.AI (
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
import Prelude hiding (State)

data Phase
  = PlayerTurn [MoveWithCaptures] (Maybe PendingAction)
  | EngineThinking (Maybe PendingAction)
  | Finished Outcome
  deriving (Show, Eq)

data State = State ExternBoard [AppliedMove] Phase
  deriving (Show, Eq)

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
mkPlayerTurn board moves pending =
  State board moves (PlayerTurn (validMovesForPosition moves) pending)

-- | First argument is the human player's color.
transition ::
  PlayerColor -> State -> Event -> Either TransitionError TransitionResult
transition _ (State _ _ (Finished _)) = const $ Left GameAlreadyFinished
transition humanColor (State board moves (PlayerTurn validMoves pending)) = \case
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
            (pending', cancelEvts) = cancelPending engineColor pending
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
            (clearPending pending <> [MovesUndone 2])
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pending <> [GameEnded outcome])
  OfferDraw color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        Right $
          TransitionResult
            (State board moves (PlayerTurn validMoves (Just (PendingAction DrawOffer color))))
            [DrawOffered color]
  AcceptDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Finished Draw))
        [OfferCancelled, GameEnded Draw]
  DeclineDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (PlayerTurn validMoves Nothing))
        [DrawDeclined]
  Timeout ->
    let outcome = TimedOut humanColor
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pending <> [GameEnded outcome])
transition humanColor (State board moves (EngineThinking pending)) = \case
  EngineMove applied maybeOutcome ->
    let moves' = moves <> [applied]
        (pending', cancelEvts) = cancelPending humanColor pending
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
            (clearPending pending <> [MovesUndone 1])
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pending <> [GameEnded outcome])
  OfferDraw color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        Right $
          TransitionResult
            (State board moves (EngineThinking (Just (PendingAction DrawOffer color))))
            [DrawOffered color]
  AcceptDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Finished Draw))
        [OfferCancelled, GameEnded Draw]
  DeclineDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (EngineThinking Nothing))
        [DrawDeclined]
  Timeout ->
    let outcome = TimedOut humanColor
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pending <> [GameEnded outcome])

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
reconstruct humanColor board moves Nothing pending =
  let turn = currentTurn moves
   in if turn == humanColor
        then mkPlayerTurn board moves pending
        else State board moves (EngineThinking pending)
