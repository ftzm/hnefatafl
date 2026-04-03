{-# LANGUAGE PatternSynonyms #-}

module Hnefatafl.Game.Common (
  -- * Outcome types
  BlackWinCondition (..),
  WhiteWinCondition (..),
  Outcome (..),

  -- * Pending actions
  PendingActionType (..),
  PendingAction (..),

  -- * Game record
  AppliedMove (..),

  -- * Errors
  TransitionError (..),

  -- * Commands
  PersistenceCommand (..),

  -- * Helpers
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
  outcomeToGameStatus,
  gameStatusToOutcome,
  toGameMove,
  undoMoves,
) where

import Chronos (Time)
import Hnefatafl.Bindings (
  EngineGameStatus (..),
  nextGameStateWithMovesTrusted,
  startBlackMoves,
  startBoard,
 )
import Hnefatafl.Core.Data (
  ExternBoard (..),
  GameMove (..),
  GameStatus,
  Layer (..),
  Move (..),
  MoveResult (..),
  MoveWithCaptures (..),
  PlayerColor (..),
 )
import Hnefatafl.Core.Data qualified as Core
import Hnefatafl.Util (pattern Snoc)

-- | Conditions under which Black wins
data BlackWinCondition = KingCaptured | WhiteSurrounded | NoWhiteMoves
  deriving (Show, Eq)

-- | Conditions under which White wins
data WhiteWinCondition = KingEscaped | ExitFort | NoBlackMoves
  deriving (Show, Eq)

data Outcome
  = BlackWins BlackWinCondition
  | WhiteWins WhiteWinCondition
  | ResignedBy PlayerColor
  | TimedOut PlayerColor
  | Draw
  | Abandoned
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data PersistenceCommand
  = PersistMove AppliedMove
  | DeleteMoves Int
  | PersistOutcome Outcome
  | PersistPendingAction PendingAction
  | ClearPendingAction
  | -- a convenience to support conditional persistence actions
    -- without wrapping everything in Maybe
    NoOp
  deriving (Show, Eq)

opponent :: PlayerColor -> PlayerColor
opponent White = Black
opponent Black = White

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
-- Returns the new pending state and any persistence command needed.
cancelPending ::
  PlayerColor ->
  Maybe PendingAction ->
  (Maybe PendingAction, PersistenceCommand)
cancelPending _ Nothing = (Nothing, NoOp)
cancelPending byColor (Just pa)
  | pa.offeredBy == byColor = (Nothing, ClearPendingAction)
  | otherwise = (Just pa, NoOp)

-- | Unconditionally clear a pending action, returning any persistence command needed.
clearPending :: Maybe PendingAction -> PersistenceCommand
clearPending Nothing = NoOp
clearPending (Just _) = ClearPendingAction

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

outcomeToGameStatus :: Outcome -> GameStatus
outcomeToGameStatus = \case
  BlackWins KingCaptured -> Core.BlackWonKingCaptured
  BlackWins WhiteSurrounded -> Core.BlackWonWhiteSurrounded
  BlackWins NoWhiteMoves -> Core.BlackWonNoWhiteMoves
  WhiteWins KingEscaped -> Core.WhiteWonKingEscaped
  WhiteWins ExitFort -> Core.WhiteWonExitFort
  WhiteWins NoBlackMoves -> Core.WhiteWonNoBlackMoves
  ResignedBy Black -> Core.BlackWonResignation
  ResignedBy White -> Core.WhiteWonResignation
  TimedOut Black -> Core.BlackWonTimeout
  TimedOut White -> Core.WhiteWonTimeout
  Draw -> Core.Draw
  Abandoned -> Core.Abandoned

gameStatusToOutcome :: GameStatus -> Maybe Outcome
gameStatusToOutcome = \case
  Core.Ongoing -> Nothing
  Core.BlackWonKingCaptured -> Just (BlackWins KingCaptured)
  Core.BlackWonWhiteSurrounded -> Just (BlackWins WhiteSurrounded)
  Core.BlackWonNoWhiteMoves -> Just (BlackWins NoWhiteMoves)
  Core.BlackWonResignation -> Just (ResignedBy White)
  Core.BlackWonTimeout -> Just (TimedOut White)
  Core.WhiteWonKingEscaped -> Just (WhiteWins KingEscaped)
  Core.WhiteWonExitFort -> Just (WhiteWins ExitFort)
  Core.WhiteWonNoBlackMoves -> Just (WhiteWins NoBlackMoves)
  Core.WhiteWonResignation -> Just (ResignedBy Black)
  Core.WhiteWonTimeout -> Just (TimedOut Black)
  Core.Draw -> Just Draw
  Core.Abandoned -> Just Abandoned

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
          Left _ -> []

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
