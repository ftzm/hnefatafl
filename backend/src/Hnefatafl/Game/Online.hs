{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hnefatafl.Game.Online (
  Phase (..),
  State (..),
  Event (..),
  TransitionResult (..),
  pending,
  remainingFor,
  updateClock,
  transition,
  reconstruct,
) where

import Chronos (Time)
import Hnefatafl.Bindings (nextGameStateWithMovesTrusted)
import Hnefatafl.Core.Data (
  ClockState (..),
  ExternBoard,
  Move (..),
  MoveWithCaptures (..),
  Outcome (..),
  PlayerColor (..),
  RemainingTime,
  TimeControl (..),
  addIncrement,
  deduct,
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
import Optics (AffineTraversal', Lens', gafield, traverseOf, (%), (.~), (?~))
import Torsor (difference)
import Prelude hiding (State, state)

data Phase
  = Active
      { turn :: PlayerColor
      , validMoves :: [MoveWithCaptures]
      , pending :: Maybe PendingAction
      , clock :: Maybe (TimeControl, ClockState)
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
  | AcceptUndo PlayerColor Time
  | DeclineUndo PlayerColor
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , events :: [DomainEvent]
  }
  deriving (Show, Eq)

-- | Construct an Active state, computing valid moves from the position.
mkActive ::
  ExternBoard ->
  [AppliedMove] ->
  Maybe PendingAction ->
  Maybe (TimeControl, ClockState) ->
  State
mkActive board moves pa clk =
  State
    board
    moves
    (Active (currentTurn moves) (validMovesForPosition moves) pa clk)

-- | End the game, clearing any pending actions.
mkFinished :: State -> Maybe PendingAction -> Outcome -> TransitionResult
mkFinished s pend outcome =
  TransitionResult
    (s & #phase .~ Finished outcome)
    (clearPending pend <> [GameEnded outcome])

-- | Lens into the remaining time for the given player color.
remainingFor :: PlayerColor -> Lens' ClockState RemainingTime
remainingFor White = #whiteRemaining
remainingFor Black = #blackRemaining

-- | Compute new clock state after a move. Deducts elapsed thinking
-- time from the mover's remaining time and adds the Fischer increment.
-- Returns Nothing if the mover's time has expired.
updateClock ::
  TimeControl ->
  ClockState ->
  PlayerColor ->
  Time ->
  Maybe ClockState
updateClock tc cs mover moveTime =
  traverseOf (remainingFor mover) update cs
    <&> (#turnStartedAt .~ moveTime)
 where
  elapsed = max mempty (difference moveTime cs.turnStartedAt)
  update r = addIncrement tc.increment <$> deduct elapsed r

transition :: State -> Event -> Either TransitionError TransitionResult
transition (State _ _ (Finished _)) = const $ Left GameAlreadyFinished
transition s@(State board moves (Active turn validMoves pend clk)) = \case
  MakeMove color move time
    | color /= turn -> Left NotYourTurn
    | move `notElem` map (.move) validMoves -> Left InvalidMove
    | otherwise -> case clk of
        Just (tc, cs) -> case updateClock tc cs color time of
          Nothing -> Right $ mkFinished s pend (TimedOut color)
          Just cs' -> makeMove (Just (tc, cs'))
        Nothing -> makeMove Nothing
   where
    makeMove clk' = do
      (moveResult, engineStatus, nextValidMoves) <-
        first (const EngineError) $
          nextGameStateWithMovesTrusted
            board
            (turn == Black)
            move
            (zobristHashes moves)

      let applied = mkAppliedMove moveResult time
          moves' = moves <> [applied]
          clockEvt = ClockUpdated . snd <$> clk'
          (pending', cancelEvts) = cancelPending (opponent color) pend

      Right $ case outcomeFromEngine engineStatus of
        Just outcome ->
          TransitionResult
            (State applied.boardAfter moves' (Finished outcome))
            ( MovePlayed applied
                : maybeToList clockEvt
                  <> clearPending pend
                  <> [GameEnded outcome]
            )
        Nothing ->
          TransitionResult
            ( State
                applied.boardAfter
                moves'
                (Active (opponent turn) nextValidMoves pending' clk')
            )
            (MovePlayed applied : maybeToList clockEvt <> cancelEvts)
  Resign color ->
    Right $ mkFinished s pend (ResignedBy color)
  -- A stale TimeoutFired can arrive if the timer commits its
  -- enqueue between the start of processing a move (which switches
  -- turn) and the subsequent Async.cancel in manageTimer. Guard
  -- against this by rejecting timeouts for the non-active player.
  Timeout color
    | color == turn -> Right $ mkFinished s pend (TimedOut color)
    | otherwise -> Left NotYourTurn
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
  AcceptUndo color undoTime -> case pend of
    Just pa
      | pa.actionType == UndoRequest && pa.offeredBy /= color ->
          let undoCount = if turn == pa.offeredBy then 2 else 1
              clk' = clk <&> second (\cs -> cs{turnStartedAt = undoTime})
           in case undoMoves undoCount moves of
                Nothing -> Left NoMovesToUndo
                Just moves' ->
                  let board' = currentBoard moves'
                   in Right $
                        TransitionResult
                          (mkActive board' moves' Nothing clk')
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
  ExternBoard ->
  [AppliedMove] ->
  Maybe Outcome ->
  Maybe PendingAction ->
  Maybe (TimeControl, ClockState) ->
  State
reconstruct board moves (Just outcome) _ _ =
  State board moves (Finished outcome)
reconstruct board moves Nothing pa clk =
  mkActive board moves pa clk
