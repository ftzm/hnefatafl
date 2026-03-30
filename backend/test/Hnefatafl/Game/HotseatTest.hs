module Hnefatafl.Game.HotseatTest where

import Hnefatafl.Core.Data (PlayerColor (..))
import Hnefatafl.Game.Common (BlackWinCondition (..), Outcome (..))
import Hnefatafl.Game.Hotseat (
  Command (..),
  Event (..),
  State (..),
  TransitionResult (..),
  reconstruct,
  transition,
 )
import Hnefatafl.Game.TestUtil (
  PersistenceStore (..),
  dummyMove,
  emptyStore,
  executePersistence,
 )
import Test.QuickCheck (Gen, elements, frequency, (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (State)

executeCommands :: [Command] -> PersistenceStore -> PersistenceStore
executeCommands cmds store = foldl' (\s (Persist c) -> executePersistence c s) store cmds

fromStore :: PersistenceStore -> State
fromStore store = reconstruct store.storedMoves store.storedOutcome store.storedPendingAction

genEvent :: State -> Maybe (Gen Event)
genEvent (Finished _) = Nothing
genEvent (AwaitingMove turn moves) =
  Just $
    frequency $
      concat
        [
          [
            ( 5
            , MakeMove turn (dummyMove turn)
                <$> elements [Nothing, Nothing, Nothing, Just (BlackWins KingCaptured)]
            )
          ]
        , [(2, pure $ Undo turn) | not (null moves)]
        , [(1, pure $ Resign turn)]
        , [(1, pure AgreeDraw)]
        , [(1, pure $ Timeout turn)]
        ]

genSequence :: State -> Gen [(Event, TransitionResult)]
genSequence s = case genEvent s of
  Nothing -> pure []
  Just gen ->
    frequency
      [ (1, pure [])
      ,
        ( 9
        , do
            event <- gen
            case transition s event of
              Left _ -> pure []
              Right result -> do
                rest <- genSequence result.newState
                pure $ (event, result) : rest
        )
      ]

test_hotseatRoundTrip :: TestTree
test_hotseatRoundTrip =
  testProperty "Hotseat: reconstructed state equals incremental state" $
    QC.forAll (genSequence (AwaitingMove Black [])) $ \steps ->
      let s = case steps of
            [] -> AwaitingMove Black []
            _ -> (snd (fromMaybe (error "empty") (viaNonEmpty last steps))).newState
          store = foldl' (\acc (_, tr) -> executeCommands tr.commands acc) emptyStore steps
       in fromStore store === s
