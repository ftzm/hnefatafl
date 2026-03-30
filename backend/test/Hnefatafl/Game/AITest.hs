module Hnefatafl.Game.AITest where

import Hnefatafl.Core.Data (PlayerColor (..))
import Hnefatafl.Game.AI (
  Command (..),
  Event (..),
  State (..),
  TransitionResult (..),
  reconstruct,
  transition,
 )
import Hnefatafl.Game.Common (
  BlackWinCondition (..),
  Outcome (..),
  PendingAction (..),
  PendingActionType (..),
  WhiteWinCondition (..),
 )
import Hnefatafl.Game.TestUtil (
  PersistenceStore (..),
  dummyMove,
  emptyStore,
  executePersistence,
 )
import Test.QuickCheck (Gen, elements, frequency, (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (State)

humanColor :: PlayerColor
humanColor = Black

engineColor :: PlayerColor
engineColor = White

executeCommands :: [Command] -> PersistenceStore -> PersistenceStore
executeCommands cmds store = foldl' go store cmds
 where
  go s (Persist c) = executePersistence c s
  go s _ = s

fromStore :: PersistenceStore -> State
fromStore store =
  reconstruct
    humanColor
    store.storedMoves
    store.storedOutcome
    store.storedPendingAction

genEvent :: State -> Maybe (Gen Event)
genEvent (Finished _) = Nothing
genEvent (PlayerTurn moves pending) =
  Just $
    frequency $
      concat
        [
          [
            ( 5
            , MakeMove (dummyMove humanColor)
                <$> elements [Nothing, Nothing, Nothing, Just (BlackWins KingCaptured)]
            )
          ]
        , [(2, pure Undo) | length moves >= 2]
        , [(1, pure $ Resign humanColor)]
        , [(1, pure $ OfferDraw humanColor) | isNothing pending]
        , [ (1, pure $ AcceptDraw humanColor)
          | Just pa <- [pending]
          , pa.offeredBy == engineColor
          ]
        , [ (1, pure $ DeclineDraw humanColor)
          | Just pa <- [pending]
          , pa.offeredBy == engineColor
          ]
        , [(1, pure Timeout)]
        ]
genEvent (EngineThinking moves pending) =
  Just $
    frequency $
      concat
        [
          [
            ( 5
            , EngineMove (dummyMove engineColor)
                <$> elements [Nothing, Nothing, Nothing, Just (WhiteWins KingEscaped)]
            )
          ]
        , [(2, pure Undo) | not (null moves)]
        , [(1, pure $ Resign engineColor)]
        , [(1, pure $ OfferDraw engineColor) | isNothing pending]
        , [ (1, pure $ AcceptDraw engineColor)
          | Just pa <- [pending]
          , pa.offeredBy == humanColor
          ]
        , [ (1, pure $ DeclineDraw engineColor)
          | Just pa <- [pending]
          , pa.offeredBy == humanColor
          ]
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
            case transition humanColor s event of
              Left _ -> pure []
              Right result -> do
                rest <- genSequence result.newState
                pure $ (event, result) : rest
        )
      ]

test_aiRoundTrip :: TestTree
test_aiRoundTrip =
  testProperty "AI: reconstructed state equals incremental state" $
    QC.forAll (genSequence (PlayerTurn [] Nothing)) $ \steps ->
      let s = case steps of
            [] -> PlayerTurn [] Nothing
            _ -> (snd (fromMaybe (error "empty") (viaNonEmpty last steps))).newState
          store = foldl' (\acc (_, tr) -> executeCommands tr.commands acc) emptyStore steps
       in fromStore store === s

test_aiCancellation :: TestTree
test_aiCancellation =
  testGroup
    "AI implicit cancellation"
    [ testCase "Player move cancels engine's draw offer" $ do
        let pending = Just $ PendingAction DrawOffer engineColor
            applied = dummyMove humanColor
        case transition humanColor (PlayerTurn [] pending) (MakeMove applied Nothing) of
          Right tr -> case tr.newState of
            EngineThinking _ p -> p @?= Nothing
            other -> fail $ "Expected EngineThinking, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Player move preserves own draw offer" $ do
        let pending = Just $ PendingAction DrawOffer humanColor
            applied = dummyMove humanColor
        case transition humanColor (PlayerTurn [] pending) (MakeMove applied Nothing) of
          Right tr -> case tr.newState of
            EngineThinking _ p -> p @?= pending
            other -> fail $ "Expected EngineThinking, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Engine move cancels human's draw offer" $ do
        let pending = Just $ PendingAction DrawOffer humanColor
            applied = dummyMove engineColor
        case transition
          humanColor
          (EngineThinking [dummyMove humanColor] pending)
          (EngineMove applied Nothing) of
          Right tr -> case tr.newState of
            PlayerTurn _ p -> p @?= Nothing
            other -> fail $ "Expected PlayerTurn, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Engine move preserves own draw offer" $ do
        let pending = Just $ PendingAction DrawOffer engineColor
            applied = dummyMove engineColor
        case transition
          humanColor
          (EngineThinking [dummyMove humanColor] pending)
          (EngineMove applied Nothing) of
          Right tr -> case tr.newState of
            PlayerTurn _ p -> p @?= pending
            other -> fail $ "Expected PlayerTurn, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    ]
