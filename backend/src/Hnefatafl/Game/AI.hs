module Hnefatafl.Game.AI (
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
  PendingAction (..),
  PendingActionType (..),
  PersistenceCommand (..),
  TransitionError (..),
  cancelPending,
  clearPending,
  currentTurn,
  opponent,
  respondToOffer,
  undoMoves,
 )
import Prelude hiding (State)

data State
  = PlayerTurn [AppliedMove] (Maybe PendingAction)
  | EngineThinking [AppliedMove] (Maybe PendingAction)
  | Finished Outcome
  deriving (Show, Eq)

data Event
  = MakeMove AppliedMove (Maybe Outcome)
  | EngineMove AppliedMove (Maybe Outcome)
  | Undo
  | Resign PlayerColor
  | OfferDraw PlayerColor
  | AcceptDraw PlayerColor
  | DeclineDraw PlayerColor
  | Timeout
  deriving (Show, Eq)

data Command
  = Persist PersistenceCommand
  | TriggerEngineSearch [AppliedMove]
  | CancelEngineSearch
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , commands :: [Command]
  }
  deriving (Show, Eq)

-- | First argument is the human player's color.
transition ::
  PlayerColor -> State -> Event -> Either TransitionError TransitionResult
transition _ (Finished _) = const $ Left GameAlreadyFinished
transition humanColor (PlayerTurn moves pending) = \case
  MakeMove applied maybeOutcome ->
    let engineColor = opponent humanColor
        moves' = moves <> [applied]
        (pending', cancelCmd) = cancelPending engineColor pending
     in Right $ case maybeOutcome of
          Just outcome ->
            TransitionResult
              (Finished outcome)
              [ Persist $ PersistMove applied
              , Persist cancelCmd
              , Persist $ PersistOutcome outcome
              ]
          Nothing ->
            TransitionResult
              (EngineThinking moves' pending')
              [ Persist $ PersistMove applied
              , Persist cancelCmd
              , TriggerEngineSearch moves'
              ]
  EngineMove{} -> Left NotYourTurn
  Undo ->
    case undoMoves 2 moves of
      Nothing -> Left NoMovesToUndo
      Just moves' ->
        Right $
          TransitionResult
            (PlayerTurn moves' Nothing)
            [ Persist $ DeleteMoves 2
            , Persist $ clearPending pending
            ]
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (Finished outcome)
            [ Persist (clearPending pending)
            , Persist $ PersistOutcome outcome
            ]
  OfferDraw color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        let pa = PendingAction DrawOffer color
         in Right $
              TransitionResult
                (PlayerTurn moves (Just pa))
                [Persist $ PersistPendingAction pa]
  AcceptDraw color ->
    respondToOffer color pending $
      TransitionResult
        (Finished Draw)
        [Persist ClearPendingAction, Persist $ PersistOutcome Draw]
  DeclineDraw color ->
    respondToOffer color pending $
      TransitionResult
        (PlayerTurn moves Nothing)
        [Persist ClearPendingAction]
  Timeout ->
    let outcome = TimedOut humanColor
     in Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ clearPending pending
            , Persist $ PersistOutcome outcome
            ]
transition humanColor (EngineThinking moves pending) = \case
  EngineMove applied maybeOutcome ->
    let moves' = moves <> [applied]
        (pending', cancelCmd) = cancelPending humanColor pending
     in Right $ case maybeOutcome of
          Just outcome ->
            TransitionResult
              (Finished outcome)
              [ Persist $ PersistMove applied
              , Persist cancelCmd
              , Persist $ PersistOutcome outcome
              ]
          Nothing ->
            TransitionResult
              (PlayerTurn moves' pending')
              [ Persist $ PersistMove applied
              , Persist cancelCmd
              ]
  MakeMove{} -> Left NotYourTurn
  Undo ->
    case undoMoves 1 moves of
      Nothing -> Left NoMovesToUndo
      Just moves' ->
        Right $
          TransitionResult
            (PlayerTurn moves' Nothing)
            [ CancelEngineSearch
            , Persist $ DeleteMoves 1
            , Persist $ clearPending pending
            ]
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ clearPending pending
            , Persist $ PersistOutcome outcome
            ]
  OfferDraw color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        let pa = PendingAction DrawOffer color
         in Right $
              TransitionResult
                (EngineThinking moves (Just pa))
                [Persist $ PersistPendingAction pa]
  AcceptDraw color ->
    respondToOffer color pending $
      TransitionResult
        (Finished Draw)
        [Persist ClearPendingAction, Persist $ PersistOutcome Draw]
  DeclineDraw color ->
    respondToOffer color pending $
      TransitionResult
        (EngineThinking moves Nothing)
        [Persist ClearPendingAction]
  Timeout ->
    let outcome = TimedOut humanColor
     in Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ clearPending pending
            , Persist $ PersistOutcome outcome
            ]

-- | Reconstruct state from persisted data.
-- If it's the engine's turn, returns EngineThinking — the effectful layer
-- is responsible for re-triggering the search.
reconstruct ::
  PlayerColor -> [AppliedMove] -> Maybe Outcome -> Maybe PendingAction -> State
reconstruct _ _ (Just outcome) _ = Finished outcome
reconstruct humanColor moves Nothing pending =
  let turn = currentTurn moves
   in if turn == humanColor
        then PlayerTurn moves pending
        else EngineThinking moves pending
