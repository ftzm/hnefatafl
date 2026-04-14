{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hnefatafl.Game.Online (
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
import Optics (AffineTraversal', gafield, (%), (.~), (?~))
import Prelude hiding (State, state)

data Phase
  = Active
      { turn :: PlayerColor
      , validMoves :: [MoveWithCaptures]
      , pending :: Maybe PendingAction
      }
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
mkActive board moves pa =
  State
    board
    moves
    (Active (currentTurn moves) (validMovesForPosition moves) pa)

transition :: State -> Event -> Either TransitionError TransitionResult
transition (State _ _ (Finished _)) = const $ Left GameAlreadyFinished
transition s@(State board moves (Active turn validMoves pend)) = \case
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
              (MovePlayed applied : clearPending pend <> [GameEnded outcome])
          Nothing ->
            let (pending', cancelEvts) = cancelPending (opponent color) pend
             in TransitionResult
                  (State applied.boardAfter moves' (Active (opponent turn) nextValidMoves pending'))
                  (MovePlayed applied : cancelEvts)
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (s & #phase .~ Finished outcome)
            (clearPending pend <> [GameEnded outcome])
  Timeout color ->
    let outcome = TimedOut color
     in Right $
          TransitionResult
            (s & #phase .~ Finished outcome)
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
        (s & #phase .~ Finished Draw)
        [OfferCancelled, GameEnded Draw]
  DeclineDraw color ->
    respondToOffer color pend $
      TransitionResult
        (s & #phase % pending .~ Nothing)
        [DrawDeclined]
  RequestUndo color
    | isJust pend -> Left ActionAlreadyPending
    | not (any (\am -> am.side == color) moves) -> Left NoMovesToUndo
    | otherwise ->
        Right $
          TransitionResult
            (s & #phase % pending ?~ PendingAction UndoRequest color)
            [UndoRequested color]
  AcceptUndo color -> case pend of
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
    respondToOffer color pend $
      TransitionResult
        (s & #phase % pending .~ Nothing)
        [UndoDeclined]

-- | Reconstruct state from persisted data
reconstruct ::
  ExternBoard -> [AppliedMove] -> Maybe Outcome -> Maybe PendingAction -> State
reconstruct board moves (Just outcome) _ = State board moves (Finished outcome)
reconstruct board moves Nothing pa = mkActive board moves pa
