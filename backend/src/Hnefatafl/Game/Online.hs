module Hnefatafl.Game.Online (
  Phase (..),
  State (..),
  Event (..),
  Command (..),
  Notification (..),
  ActorNotification (..),
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
  PlayerColor (..),
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  Outcome (..),
  PendingAction (..),
  PendingActionType (..),
  PersistenceCommand (..),
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

data ActorNotification
  = GameEnded Outcome
  | UndoApplied
  deriving (Show, Eq)

data Command
  = Persist PersistenceCommand
  | NotifyOpponent Notification
  | NotifyActor ActorNotification
  deriving (Show, Eq)

data TransitionResult = TransitionResult
  { newState :: State
  , commands :: [Command]
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
    | otherwise ->
        let hashes = zobristHashes moves
            (moveResult, engineStatus, nextValidMoves) =
              fromRight (error "valid move rejected by engine") $
                nextGameStateWithMovesTrusted board (turn == Black) move hashes
            applied = mkAppliedMove moveResult time
            moves' = moves <> [applied]
         in case outcomeFromEngine engineStatus of
              Just outcome ->
                Right $
                  TransitionResult
                    (State applied.boardAfter moves' (Finished outcome))
                    [ Persist $ PersistMove applied
                    , Persist $ clearPending pending
                    , Persist $ PersistOutcome outcome
                    , NotifyOpponent $ OpponentMoved applied
                    , NotifyActor $ GameEnded outcome
                    ]
              Nothing ->
                let (pending', cancelCmd) = cancelPending (opponent color) pending
                 in Right $
                      TransitionResult
                        (State applied.boardAfter moves' (Active (opponent turn) nextValidMoves pending'))
                        [ Persist $ PersistMove applied
                        , Persist cancelCmd
                        , NotifyOpponent $ OpponentMoved applied
                        ]
  Resign color ->
    let outcome = ResignedBy color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            [ Persist $ clearPending pending
            , Persist $ PersistOutcome outcome
            , NotifyOpponent $ OpponentResigned color
            ]
  Timeout color ->
    let outcome = TimedOut color
     in Right $
          TransitionResult
            (State board moves (Finished outcome))
            [ Persist $ clearPending pending
            , Persist $ PersistOutcome outcome
            , NotifyOpponent $ OpponentTimedOut color
            ]
  OfferDraw color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        let pa = PendingAction DrawOffer color
         in Right $
              TransitionResult
                (State board moves (Active turn validMoves (Just pa)))
                [ Persist $ PersistPendingAction pa
                , NotifyOpponent $ DrawOffered color
                ]
  AcceptDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Finished Draw))
        [ Persist ClearPendingAction
        , Persist $ PersistOutcome Draw
        , NotifyOpponent DrawAccepted
        ]
  DeclineDraw color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Active turn validMoves Nothing))
        [ Persist ClearPendingAction
        , NotifyOpponent DrawDeclined
        ]
  RequestUndo color
    | isJust pending -> Left ActionAlreadyPending
    | otherwise ->
        let pa = PendingAction UndoRequest color
         in Right $
              TransitionResult
                (State board moves (Active turn validMoves (Just pa)))
                [ Persist $ PersistPendingAction pa
                , NotifyOpponent $ UndoRequested color
                ]
  AcceptUndo color -> case pending of
    Just pa
      | pa.actionType == UndoRequest && pa.offeredBy /= color ->
          -- If it's the requester's turn, opponent has responded since,
          -- so undo 2 moves. Otherwise undo 1.
          let undoCount = if turn == pa.offeredBy then 2 else 1
           in case undoMoves undoCount moves of
                Nothing -> Left NoMovesToUndo
                Just moves' ->
                  let board' = currentBoard moves'
                   in Right $
                        TransitionResult
                          (mkActive board' moves' Nothing)
                          [ Persist ClearPendingAction
                          , Persist $ DeleteMoves undoCount
                          , NotifyOpponent $ UndoAccepted moves'
                          , NotifyActor UndoApplied
                          ]
    Just pa | pa.offeredBy == color -> Left CannotRespondToOwnOffer
    _ -> Left NoPendingOffer
  DeclineUndo color ->
    respondToOffer color pending $
      TransitionResult
        (State board moves (Active turn validMoves Nothing))
        [ Persist ClearPendingAction
        , NotifyOpponent UndoDeclined
        ]

-- | Reconstruct state from persisted data
reconstruct ::
  ExternBoard -> [AppliedMove] -> Maybe Outcome -> Maybe PendingAction -> State
reconstruct board moves (Just outcome) _ = State board moves (Finished outcome)
reconstruct board moves Nothing pending = mkActive board moves pending
