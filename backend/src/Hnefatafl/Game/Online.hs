module Hnefatafl.Game.Online (
  ActiveGame (..),
  State (..),
  Event (..),
  Command (..),
  Notification (..),
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

data ActiveGame = ActiveGame
  { turn :: PlayerColor
  , moves :: [AppliedMove]
  , pendingAction :: Maybe PendingAction
  }
  deriving (Show, Eq)

data State
  = Active ActiveGame
  | Finished Outcome
  deriving (Show, Eq)

data Event
  = MakeMove PlayerColor AppliedMove (Maybe Outcome)
  | Resign PlayerColor
  | Timeout PlayerColor
  | OfferDraw PlayerColor
  | AcceptDraw PlayerColor
  | DeclineDraw PlayerColor
  | RequestUndo PlayerColor
  | AcceptUndo PlayerColor
  | DeclineUndo PlayerColor
  deriving (Show, Eq)

data Notification
  = OpponentMoved AppliedMove
  | OpponentResigned PlayerColor
  | OpponentTimedOut PlayerColor
  | DrawOffered PlayerColor
  | DrawAccepted
  | DrawDeclined
  | UndoRequested PlayerColor
  | UndoAccepted [AppliedMove]
  | UndoDeclined
  deriving (Show, Eq)

data Command
  = Persist PersistenceCommand
  | NotifyOpponent Notification
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , commands :: [Command]
  }
  deriving (Show, Eq)

transition :: State -> Event -> Either TransitionError TransitionResult
transition (Finished _) = const $ Left GameAlreadyFinished
transition (Active g) = \case
  MakeMove color applied maybeOutcome
    | color /= g.turn -> Left NotYourTurn
    | Just outcome <- maybeOutcome ->
        Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ PersistMove applied
            , Persist $ clearPending g.pendingAction
            , Persist $ PersistOutcome outcome
            , NotifyOpponent $ OpponentMoved applied
            ]
    | otherwise ->
        let (pending', cancelCmd) = cancelPending (opponent color) g.pendingAction
            g' =
              g
                { turn = opponent g.turn
                , moves = g.moves <> [applied]
                , pendingAction = pending'
                }
         in Right $
              TransitionResult
                (Active g')
                [ Persist $ PersistMove applied
                , Persist cancelCmd
                , NotifyOpponent $ OpponentMoved applied
                ]
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ clearPending g.pendingAction
            , Persist $ PersistOutcome outcome
            , NotifyOpponent $ OpponentResigned color
            ]
  Timeout color ->
    let outcome = TimedOut color
     in Right $
          TransitionResult
            (Finished outcome)
            [ Persist $ clearPending g.pendingAction
            , Persist $ PersistOutcome outcome
            , NotifyOpponent $ OpponentTimedOut color
            ]
  OfferDraw color
    | isJust g.pendingAction -> Left ActionAlreadyPending
    | otherwise ->
        let pa = PendingAction DrawOffer color
         in Right $
              TransitionResult
                (Active g{pendingAction = Just pa})
                [ Persist $ PersistPendingAction pa
                , NotifyOpponent $ DrawOffered color
                ]
  AcceptDraw color ->
    respondToOffer color g.pendingAction $
      TransitionResult
        (Finished Draw)
        [ Persist ClearPendingAction
        , Persist $ PersistOutcome Draw
        , NotifyOpponent DrawAccepted
        ]
  DeclineDraw color ->
    respondToOffer color g.pendingAction $
      TransitionResult
        (Active g{pendingAction = Nothing})
        [ Persist ClearPendingAction
        , NotifyOpponent DrawDeclined
        ]
  RequestUndo color
    | isJust g.pendingAction -> Left ActionAlreadyPending
    | otherwise ->
        let pa = PendingAction UndoRequest color
         in Right $
              TransitionResult
                (Active g{pendingAction = Just pa})
                [ Persist $ PersistPendingAction pa
                , NotifyOpponent $ UndoRequested color
                ]
  AcceptUndo color -> case g.pendingAction of
    Just pa
      | pa.actionType == UndoRequest && pa.offeredBy /= color ->
          -- If it's the requester's turn, opponent has responded since,
          -- so undo 2 moves. Otherwise undo 1.
          let undoCount = if g.turn == pa.offeredBy then 2 else 1
           in case undoMoves undoCount g.moves of
                Nothing -> Left NoMovesToUndo
                Just moves' ->
                  let g' = ActiveGame{turn = pa.offeredBy, moves = moves', pendingAction = Nothing}
                   in Right $
                        TransitionResult
                          (Active g')
                          [ Persist ClearPendingAction
                          , Persist $ DeleteMoves undoCount
                          , NotifyOpponent $ UndoAccepted moves'
                          ]
    Just pa | pa.offeredBy == color -> Left CannotRespondToOwnOffer
    _ -> Left NoPendingOffer
  DeclineUndo color ->
    respondToOffer color g.pendingAction $
      TransitionResult
        (Active g{pendingAction = Nothing})
        [ Persist ClearPendingAction
        , NotifyOpponent UndoDeclined
        ]

-- | Reconstruct state from persisted data
reconstruct :: [AppliedMove] -> Maybe Outcome -> Maybe PendingAction -> State
reconstruct _ (Just outcome) _ = Finished outcome
reconstruct moves Nothing pending =
  Active
    ActiveGame
      { turn = currentTurn moves
      , moves = moves
      , pendingAction = pending
      }
