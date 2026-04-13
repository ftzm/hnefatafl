module Hnefatafl.Game.TestUtil where

import Chronos (Time (..))
import Hnefatafl.Bindings (startBoard)
import Hnefatafl.Core.Data (
  BlackWinCondition (..),
  Layer (..),
  Move (..),
  Outcome (..),
  PlayerColor (..),
  WhiteWinCondition (..),
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  DomainEvent (..),
  PendingAction (..),
  PendingActionType (..),
 )
import Test.QuickCheck (Arbitrary (..), elements, oneof)

dummyMove :: PlayerColor -> AppliedMove
dummyMove color =
  AppliedMove
    { move = Move 0 1
    , side = color
    , captures = Layer 0 0
    , boardAfter = startBoard
    , zobristHash = 0
    , timestamp = Time 0
    }

-- | Build a move history of N alternating moves starting with Black
mkMoves :: Int -> [AppliedMove]
mkMoves n =
  take n $
    zipWith
      (\i _ -> dummyMove (if even (i :: Int) then Black else White))
      [0 ..]
      [1 .. n]

instance Arbitrary PlayerColor where
  arbitrary = elements [Black, White]

instance Arbitrary AppliedMove where
  arbitrary = dummyMove <$> arbitrary

instance Arbitrary PendingActionType where
  arbitrary = elements [DrawOffer, UndoRequest]

instance Arbitrary PendingAction where
  arbitrary = PendingAction <$> arbitrary <*> arbitrary

instance Arbitrary Outcome where
  arbitrary =
    oneof
      [ pure $ BlackWins KingCaptured
      , pure $ WhiteWins KingEscaped
      , ResignedBy <$> arbitrary
      , TimedOut <$> arbitrary
      , pure Draw
      , pure Abandoned
      ]

-- | In-memory persistence store, mirrors DB tables
data PersistenceStore = PersistenceStore
  { storedMoves :: [AppliedMove]
  , storedOutcome :: Maybe Outcome
  , storedPendingAction :: Maybe PendingAction
  }
  deriving (Show, Eq)

emptyStore :: PersistenceStore
emptyStore = PersistenceStore [] Nothing Nothing

-- | Execute a DomainEvent against the in-memory store
applyEvent :: DomainEvent -> PersistenceStore -> PersistenceStore
applyEvent evt store = case evt of
  MovePlayed am -> store{storedMoves = store.storedMoves <> [am]}
  GameEnded o -> store{storedOutcome = Just o}
  MovesUndone n -> store{storedMoves = take (length store.storedMoves - n) store.storedMoves}
  DrawOffered color -> store{storedPendingAction = Just (PendingAction DrawOffer color)}
  UndoRequested color -> store{storedPendingAction = Just (PendingAction UndoRequest color)}
  DrawDeclined -> store{storedPendingAction = Nothing}
  UndoDeclined -> store{storedPendingAction = Nothing}
  OfferCancelled -> store{storedPendingAction = Nothing}

applyEvents :: [DomainEvent] -> PersistenceStore -> PersistenceStore
applyEvents evts store = foldl' (flip applyEvent) store evts
