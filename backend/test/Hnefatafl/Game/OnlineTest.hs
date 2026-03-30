module Hnefatafl.Game.OnlineTest where

import Hnefatafl.Core.Data (PlayerColor (..))
import Hnefatafl.Game.Common (
  AppliedMove (..),
  BlackWinCondition (..),
  Outcome (..),
  PendingAction (..),
  PendingActionType (..),
  opponent,
 )
import Hnefatafl.Game.Online (
  ActiveGame (..),
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
  mkMoves,
 )
import Test.QuickCheck (Gen, elements, frequency, (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (State)

executeCommands :: [Command] -> PersistenceStore -> PersistenceStore
executeCommands cmds store = foldl' go store cmds
 where
  go s (Persist c) = executePersistence c s
  go s _ = s

fromStore :: PersistenceStore -> State
fromStore store = reconstruct store.storedMoves store.storedOutcome store.storedPendingAction

mkActive :: PlayerColor -> [AppliedMove] -> Maybe PendingAction -> State
mkActive turn moves pending = Active ActiveGame{turn, moves, pendingAction = pending}

genEvent :: State -> Maybe (Gen Event)
genEvent (Finished _) = Nothing
genEvent (Active g) =
  Just $
    frequency $
      concat
        [
          [
            ( 5
            , MakeMove g.turn (dummyMove g.turn)
                <$> elements [Nothing, Nothing, Nothing, Just (BlackWins KingCaptured)]
            )
          ]
        , [(1, Resign <$> elements [Black, White])]
        , [(1, Timeout <$> elements [Black, White])]
        , [(1, OfferDraw <$> elements [Black, White]) | isNothing g.pendingAction]
        , [ (1, pure $ AcceptDraw (opponent pa.offeredBy))
          | Just pa <- [g.pendingAction]
          , pa.actionType == DrawOffer
          ]
        , [ (1, pure $ DeclineDraw (opponent pa.offeredBy))
          | Just pa <- [g.pendingAction]
          , pa.actionType == DrawOffer
          ]
        , [(1, RequestUndo <$> elements [Black, White]) | isNothing g.pendingAction]
        , [ (1, pure $ AcceptUndo (opponent pa.offeredBy))
          | Just pa <- [g.pendingAction]
          , pa.actionType == UndoRequest
          , hasEnoughMoves g pa
          ]
        , [ (1, pure $ DeclineUndo (opponent pa.offeredBy))
          | Just pa <- [g.pendingAction]
          , pa.actionType == UndoRequest
          ]
        ]
 where
  hasEnoughMoves g' pa =
    let n = if g'.turn == pa.offeredBy then 2 else 1
     in length g'.moves >= n

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

test_onlineRoundTrip :: TestTree
test_onlineRoundTrip =
  testProperty "Online: reconstructed state equals incremental state" $
    QC.forAll (genSequence (mkActive Black [] Nothing)) $ \steps ->
      let s = case steps of
            [] -> mkActive Black [] Nothing
            _ -> (snd (fromMaybe (error "empty") (viaNonEmpty last steps))).newState
          store = foldl' (\acc (_, tr) -> executeCommands tr.commands acc) emptyStore steps
       in fromStore store === s

test_onlineCancellation :: TestTree
test_onlineCancellation =
  testGroup
    "Online implicit cancellation"
    [ testCase "Move cancels opponent's draw offer" $ do
        let st = mkActive Black [dummyMove White] (Just $ PendingAction DrawOffer White)
            applied = dummyMove Black
        case transition st (MakeMove Black applied Nothing) of
          Right tr -> case tr.newState of
            Active g -> g.pendingAction @?= Nothing
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Move preserves own draw offer" $ do
        let st = mkActive Black [] (Just $ PendingAction DrawOffer Black)
            applied = dummyMove Black
        case transition st (MakeMove Black applied Nothing) of
          Right tr -> case tr.newState of
            Active g -> g.pendingAction @?= Just (PendingAction DrawOffer Black)
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Move cancels opponent's undo request" $ do
        let st = mkActive Black [dummyMove White] (Just $ PendingAction UndoRequest White)
            applied = dummyMove Black
        case transition st (MakeMove Black applied Nothing) of
          Right tr -> case tr.newState of
            Active g -> g.pendingAction @?= Nothing
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Move preserves own undo request" $ do
        let st = mkActive Black [] (Just $ PendingAction UndoRequest Black)
            applied = dummyMove Black
        case transition st (MakeMove Black applied Nothing) of
          Right tr -> case tr.newState of
            Active g -> g.pendingAction @?= Just (PendingAction UndoRequest Black)
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    ]

test_onlineUndoCount :: TestTree
test_onlineUndoCount =
  testGroup
    "Online undo move count"
    [ testCase "Undo 1 move when requester just moved (opponent's turn)" $ do
        -- Black moved, it's White's turn, Black requested undo, White accepts
        let moves = [dummyMove Black]
            st = mkActive White moves (Just $ PendingAction UndoRequest Black)
        case transition st (AcceptUndo White) of
          Right tr -> case tr.newState of
            Active g -> do
              length g.moves @?= 0
              g.turn @?= Black
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Undo 2 moves when opponent responded (requester's turn)" $ do
        -- Black moved, White responded, it's Black's turn, Black requested undo, White accepts
        let moves = mkMoves 2
            st = mkActive Black moves (Just $ PendingAction UndoRequest Black)
        case transition st (AcceptUndo White) of
          Right tr -> case tr.newState of
            Active g -> do
              length g.moves @?= 0
              g.turn @?= Black
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    ]
