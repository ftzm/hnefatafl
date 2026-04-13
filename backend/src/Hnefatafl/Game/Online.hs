module Hnefatafl.Game.Online (
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
  = Active PlayerColor [MoveWithCaptures] (Maybe PendingAction)
  | Finished Outcome
  deriving (Show, Eq)

data State = State ExternBoard [AppliedMove] Phase
  deriving (Show, Eq)

data Event
  = MakeMove PlayerColor Move Time
  | Resign PlayerColor
  | Timeout PlayerColor
  | OfferDraw PlayerColor
  | AcceptDraw PlayerColor
  | DeclineDraw PlayerColor
  | RequestUndo PlayerColor
  | AcceptUndo PlayerColor
  | DeclineUndo PlayerColor
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , events :: [DomainEvent]
  }
  deriving (Show, Eq)

-- | Construct an Active state, computing valid moves from the position.
mkActive :: ExternBoard -> [AppliedMove] -> Maybe PendingAction -> State
mkActive board moves pending =
  State
    board
    moves
    (Active (currentTurn moves) (validMovesForPosition moves) pending)

transition :: State -> Event -> Either TransitionError TransitionResult
transition (State _ _ (Finished _)) = const $ Left GameAlreadyFinished
transition (State board moves (Active turn validMoves pending)) = \case
  MakeMove color move time
    | color /= turn -> Left NotYourTurn
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
              (MovePlayed applied : clearPending pending <> [GameEnded outcome])
          Nothing ->
            let (pending', cancelEvts) = cancelPending (opponent color) pending
             in TransitionResult
                  (State applied.boardAfter moves' (Active (opponent turn) nextValidMoves pending'))
                  (MovePlayed applied : cancelEvts)
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pending <> [GameEnded outcome])
  Timeout color ->
    let outcome = TimedOut color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            (clearPending pending <> [GameEnded outcome])
  OfferDraw color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        Right $
          TransitionResult
            ( State
                board
                moves
                (Active turn validMoves (Just (PendingAction DrawOffer color)))
            )
            [DrawOffered color]
  AcceptDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Finished Draw))
        [OfferCancelled, GameEnded Draw]
  DeclineDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Active turn validMoves Nothing))
        [DrawDeclined]
  RequestUndo color
    | isJust pending -> Left ActionAlreadyPending
    | not (any (\am -> am.side == color) moves) -> Left NoMovesToUndo
    | otherwise ->
        Right $
          TransitionResult
            ( State
                board
                moves
                (Active turn validMoves (Just (PendingAction UndoRequest color)))
            )
            [UndoRequested color]
  AcceptUndo color -> case pending of
    Just pa
      | pa.actionType == UndoRequest && pa.offeredBy /= color ->
          let undoCount = if turn == pa.offeredBy then 2 else 1
           in case undoMoves undoCount moves of
                Nothing -> Left NoMovesToUndo
                Just moves' ->
                  let board' = currentBoard moves'
                   in Right $
                        TransitionResult
                          (mkActive board' moves' Nothing)
                          [OfferCancelled, MovesUndone undoCount]
    Just pa | pa.offeredBy == color -> Left CannotRespondToOwnOffer
    _ -> Left NoPendingOffer
  DeclineUndo color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Active turn validMoves Nothing))
        [UndoDeclined]

-- | Reconstruct state from persisted data
reconstruct ::
  ExternBoard -> [AppliedMove] -> Maybe Outcome -> Maybe PendingAction -> State
reconstruct board moves (Just outcome) _ = State board moves (Finished outcome)
reconstruct board moves Nothing pending = mkActive board moves pending
