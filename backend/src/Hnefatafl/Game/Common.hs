{-# LANGUAGE PatternSynonyms #-}

module Hnefatafl.Game.Common (
  -- * Pending actions
  PendingActionType (..),
  PendingAction (..),

  -- * Game record
  AppliedMove (..),

  -- * Errors
  TransitionError (..),

  -- * Domain events
  DomainEvent (..),

  -- * Helpers (re-exports)
  opponent,
  winner,
  currentBoard,
  currentTurn,
  zobristHashes,
  validMovesForPosition,
  mkAppliedMove,
  cancelPending,
  clearPending,
  respondToOffer,
  outcomeFromEngine,
  toGameMove,
  undoMoves,
) where

import Chronos (Time)
import Control.Exception (throw)
import Hnefatafl.Bindings (
  EngineGameStatus (..),
  nextGameStateWithMovesTrusted,
  startBlackMoves,
  startBoard,
 )
import Hnefatafl.Core.Data (
  BlackWinCondition (..),
  ExternBoard (..),
  GameMove (..),
  Layer (..),
  Move (..),
  MoveResult (..),
  MoveWithCaptures (..),
  Outcome (..),
  PlayerColor (..),
  WhiteWinCondition (..),
  opponent,
 )
import Hnefatafl.Exception (GameInvariantException (..))
import Hnefatafl.Util (pattern Snoc)

data PendingActionType = DrawOffer | UndoRequest
  deriving (Show, Eq)

data PendingAction = PendingAction
  { actionType :: PendingActionType
  , offeredBy :: PlayerColor
  }
  deriving (Show, Eq)

data AppliedMove = AppliedMove
  { move :: Move
  , side :: PlayerColor
  , captures :: Layer
  , boardAfter :: ExternBoard
  , zobristHash :: Word64
  , timestamp :: Time
  }
  deriving (Show, Eq)

data TransitionError
  = NotYourTurn
  | GameAlreadyFinished
  | InvalidMove
  | NoPendingOffer
  | CannotRespondToOwnOffer
  | ActionAlreadyPending
  | NoMovesToUndo
  | EngineError
  deriving (Show, Eq)

-- | Facts about what happened during a transition. Used for
-- persistence, notifications, metrics, and other cross-cutting
-- concerns.
data DomainEvent
  = MovePlayed AppliedMove
  | GameEnded Outcome
  | MovesUndone Int
  | DrawOffered PlayerColor
  | DrawDeclined
  | UndoRequested PlayerColor
  | UndoDeclined
  | OfferCancelled
  deriving (Show, Eq)

winner :: Outcome -> Maybe PlayerColor
winner (BlackWins _) = Just Black
winner (WhiteWins _) = Just White
winner (ResignedBy c) = Just (opponent c)
winner (TimedOut c) = Just (opponent c)
winner _ = Nothing

currentBoard :: [AppliedMove] -> ExternBoard
currentBoard moves = maybe startBoard (.boardAfter) (viaNonEmpty last moves)

currentTurn :: [AppliedMove] -> PlayerColor
currentTurn moves = maybe Black (opponent . (.side)) (viaNonEmpty last moves)

zobristHashes :: [AppliedMove] -> [Word64]
zobristHashes = map (.zobristHash)

-- | Drop the last N moves, returning the remaining history.
-- Returns Nothing if there aren't enough moves to undo.
undoMoves :: Int -> [AppliedMove] -> Maybe [AppliedMove]
undoMoves n moves
  | length moves < n = Nothing
  | otherwise = Just $ take (length moves - n) moves

-- | Conditionally cancel a pending action if it was offered by the given color.
-- Returns the new pending state and any domain events.
cancelPending ::
  PlayerColor ->
  Maybe PendingAction ->
  (Maybe PendingAction, [DomainEvent])
cancelPending _ Nothing = (Nothing, [])
cancelPending byColor (Just pa)
  | pa.offeredBy == byColor = (Nothing, [OfferCancelled])
  | otherwise = (Just pa, [])

-- | Unconditionally clear a pending action, returning any domain events.
clearPending :: Maybe PendingAction -> [DomainEvent]
clearPending Nothing = []
clearPending (Just _) = [OfferCancelled]

-- | Validate that a player can respond to a pending offer.
respondToOffer ::
  PlayerColor ->
  Maybe PendingAction ->
  a ->
  Either TransitionError a
respondToOffer color pending onSuccess = case pending of
  Nothing -> Left NoPendingOffer
  Just pa
    | pa.offeredBy == color -> Left CannotRespondToOwnOffer
    | otherwise -> Right onSuccess

-- | Convert an engine game status to an Outcome.
-- Returns Nothing if the game is still ongoing.
outcomeFromEngine :: EngineGameStatus -> Maybe Outcome
outcomeFromEngine = \case
  EngineOngoing -> Nothing
  EngineKingCaptured -> Just $ BlackWins KingCaptured
  EngineWhiteSurrounded -> Just $ BlackWins WhiteSurrounded
  EngineNoWhiteMoves -> Just $ BlackWins NoWhiteMoves
  EngineKingEscaped -> Just $ WhiteWins KingEscaped
  EngineExitFort -> Just $ WhiteWins ExitFort
  EngineNoBlackMoves -> Just $ WhiteWins NoBlackMoves
  EngineDrawOffered -> Just Draw
  EngineWhiteResigned -> Just $ ResignedBy White
  EngineBlackResigned -> Just $ ResignedBy Black

toGameMove :: AppliedMove -> GameMove
toGameMove am =
  GameMove
    { playerColor = am.side
    , move = am.move
    , boardStateAfter = am.boardAfter
    , captures = am.captures
    , timestamp = am.timestamp
    }

-- | Compute valid moves for a position from the move history.
validMovesForPosition :: [AppliedMove] -> [MoveWithCaptures]
validMovesForPosition moves = case nonEmpty moves of
  Nothing -> toList startBlackMoves
  Just (Snoc prevMs lastM) ->
    let board = currentBoard prevMs
        hashes = zobristHashes prevMs
     in case nextGameStateWithMovesTrusted board (lastM.side == Black) lastM.move hashes of
          Right (_, _, validMoves) -> validMoves
          Left err ->
            throw $
              EngineReplayFailed
                { context = "validMovesForPosition"
                , detail = show err
                }

mkAppliedMove :: MoveResult -> Time -> AppliedMove
mkAppliedMove m t =
  AppliedMove
    { move = m.move
    , side = if m.wasBlackTurn then Black else White
    , captures = m.captures
    , boardAfter = m.board
    , zobristHash = m.zobristHash
    , timestamp = t
    }
