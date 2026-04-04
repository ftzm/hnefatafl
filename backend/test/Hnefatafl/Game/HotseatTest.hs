module Hnefatafl.Game.HotseatTest where

import Chronos (Time (..))
import Hnefatafl.Bindings (startBlackMoves, startBoard)
import Hnefatafl.Core.Data (MoveWithCaptures (..), PlayerColor (..))
import Hnefatafl.Game.Common (currentBoard)
import Hnefatafl.Game.Hotseat (
  Command (..),
  Event (..),
  Phase (..),
  State (..),
  TransitionResult (..),
  reconstruct,
  transition,
 )
import Hnefatafl.Game.TestUtil (
  PersistenceStore (..),
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
fromStore store =
  reconstruct
    (currentBoard store.storedMoves)
    store.storedMoves
    store.storedOutcome

genEvent :: State -> Maybe (Gen Event)
genEvent (State _ _ (Finished _)) = Nothing
genEvent (State _ moves (Awaiting turn validMoves)) =
  Just $
    frequency $
      concat
        [
          [
            ( 5
            , MakeMove . (.move) <$> elements (toList validMoves) <*> pure (Time 0)
            )
          ]
        , [(2, pure Undo) | not (null moves)]
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
  testProperty "Hotseat: reconstructed state equals incremental state"
    $ QC.forAll
      (genSequence (State startBoard [] (Awaiting Black (toList startBlackMoves))))
    $ \steps ->
      let s = case steps of
            [] -> State startBoard [] (Awaiting Black (toList startBlackMoves))
            _ -> (snd (fromMaybe (error "empty") (viaNonEmpty last steps))).newState
          store = foldl' (\acc (_, tr) -> executeCommands tr.commands acc) emptyStore steps
       in fromStore store === s
