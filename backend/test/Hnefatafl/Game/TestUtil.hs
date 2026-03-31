module Hnefatafl.Game.TestUtil where

import Chronos (Time (..))
import Hnefatafl.Bindings (startBoard)
import Hnefatafl.Core.Data (Layer (..), Move (..), PlayerColor (..))
import Hnefatafl.Game.Common (
  AppliedMove (..),
  BlackWinCondition (..),
  Outcome (..),
  PendingAction (..),
  PendingActionType (..),
  PersistenceCommand (..),
  WhiteWinCondition (..),
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

-- | Execute a PersistenceCommand against the in-memory store
executePersistence :: PersistenceCommand -> PersistenceStore -> PersistenceStore
executePersistence cmd store = case cmd of
  PersistMove am -> store{storedMoves = store.storedMoves <> [am]}
  DeleteMoves n -> store{storedMoves = take (length store.storedMoves - n) store.storedMoves}
  PersistOutcome o -> store{storedOutcome = Just o}
  PersistPendingAction pa -> store{storedPendingAction = Just pa}
  ClearPendingAction -> store{storedPendingAction = Nothing}
  NoOp -> store
