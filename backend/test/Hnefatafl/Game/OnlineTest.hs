module Hnefatafl.Game.OnlineTest where

import Chronos (Time (..))
import Hnefatafl.Bindings (startBlackMoves, startBoard)
import Hnefatafl.Core.Data (MoveWithCaptures (..), PlayerColor (..))
import Hnefatafl.Game.Common (
  AppliedMove (..),
  PendingAction (..),
  PendingActionType (..),
  currentBoard,
  opponent,
  validMovesForPosition,
 )
import Hnefatafl.Game.Online (
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (State, state)

executeCommands :: [Command] -> PersistenceStore -> PersistenceStore
executeCommands cmds store = foldl' go store cmds
 where
  go s (Persist c) = executePersistence c s
  go s _ = s

fromStore :: PersistenceStore -> State
fromStore store =
  reconstruct
    (currentBoard store.storedMoves)
    store.storedMoves
    store.storedOutcome
    store.storedPendingAction

mkActiveState :: PlayerColor -> [AppliedMove] -> Maybe PendingAction -> State
mkActiveState turn moves pending =
  let board = currentBoard moves
      validMoves = validMovesForPosition moves
   in State board moves (Active turn validMoves pending)

genEvent :: State -> Maybe (Gen Event)
genEvent (State _ _ (Finished _)) = Nothing
genEvent (State _ moves (Active turn validMoves pending)) =
  Just $
    frequency $
      concat
        [
          [ (5, MakeMove turn . (.move) <$> elements validMoves <*> pure (Time 0))
          ]
        , [(1, Resign <$> elements [Black, White])]
        , [(1, Timeout <$> elements [Black, White])]
        , [(1, OfferDraw <$> elements [Black, White]) | isNothing pending]
        , [ (1, pure $ AcceptDraw (opponent pa.offeredBy))
          | Just pa <- [pending]
          , pa.actionType == DrawOffer
          ]
        , [ (1, pure $ DeclineDraw (opponent pa.offeredBy))
          | Just pa <- [pending]
          , pa.actionType == DrawOffer
          ]
        , [(1, RequestUndo <$> elements [Black, White]) | isNothing pending]
        , [ (1, pure $ AcceptUndo (opponent pa.offeredBy))
          | Just pa <- [pending]
          , pa.actionType == UndoRequest
          , hasEnoughMoves
          ]
        , [ (1, pure $ DeclineUndo (opponent pa.offeredBy))
          | Just pa <- [pending]
          , pa.actionType == UndoRequest
          ]
        ]
 where
  hasEnoughMoves = case pending of
    Just pa ->
      let n = if turn == pa.offeredBy then 2 else 1
       in length moves >= n
    Nothing -> False

initialState :: State
initialState =
  State startBoard [] (Active Black (toList startBlackMoves) Nothing)

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
    QC.forAll (genSequence initialState) $ \steps ->
      let s = case steps of
            [] -> initialState
            _ -> (snd (fromMaybe (error "empty") (viaNonEmpty last steps))).newState
          store = foldl' (\acc (_, tr) -> executeCommands tr.commands acc) emptyStore steps
       in fromStore store === s

test_onlineCancellation :: TestTree
test_onlineCancellation =
  testGroup
    "Online implicit cancellation"
    [ testCase "Move cancels opponent's draw offer" $ do
        let st = mkActiveState Black [] (Just $ PendingAction DrawOffer White)
            move = (head startBlackMoves).move
        case transition st (MakeMove Black move (Time 0)) of
          Right tr -> case tr.newState of
            State _ _ (Active _ _ p) -> p @?= Nothing
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Move preserves own draw offer" $ do
        let st = mkActiveState Black [] (Just $ PendingAction DrawOffer Black)
            move = (head startBlackMoves).move
        case transition st (MakeMove Black move (Time 0)) of
          Right tr -> case tr.newState of
            State _ _ (Active _ _ p) -> p @?= Just (PendingAction DrawOffer Black)
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Move cancels opponent's undo request" $ do
        let st = mkActiveState Black [] (Just $ PendingAction UndoRequest White)
            move = (head startBlackMoves).move
        case transition st (MakeMove Black move (Time 0)) of
          Right tr -> case tr.newState of
            State _ _ (Active _ _ p) -> p @?= Nothing
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Move preserves own undo request" $ do
        let st = mkActiveState Black [] (Just $ PendingAction UndoRequest Black)
            move = (head startBlackMoves).move
        case transition st (MakeMove Black move (Time 0)) of
          Right tr -> case tr.newState of
            State _ _ (Active _ _ p) -> p @?= Just (PendingAction UndoRequest Black)
            other -> fail $ "Expected Active, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    ]

test_onlineUndoCount :: TestTree
test_onlineUndoCount =
  testGroup
    "Online undo move count"
    [ testCase "Undo 1 move when requester just moved (opponent's turn)" $ do
        -- Black moves, it's White's turn, Black requested undo, White accepts
        let move = (head startBlackMoves).move
        case transition initialState (MakeMove Black move (Time 0)) of
          Right tr -> do
            let State board1 moves1 _ = tr.newState
                stateWithUndo =
                  State
                    board1
                    moves1
                    ( Active
                        White
                        (validMovesForPosition moves1)
                        (Just $ PendingAction UndoRequest Black)
                    )
            case transition stateWithUndo (AcceptUndo White) of
              Right tr2 -> case tr2.newState of
                State _ moves2 (Active turn2 _ _) -> do
                  length moves2 @?= 0
                  turn2 @?= Black
                other -> fail $ "Expected Active, got " <> show other
              Left e -> fail $ "Expected Right, got Left " <> show e
          Left e -> fail $ "Setup move failed: " <> show e
    , testCase "Undo 2 moves when opponent responded (requester's turn)" $ do
        -- Black moves, White moves, it's Black's turn, Black requests undo, White accepts
        let blackMove = (head startBlackMoves).move
        case transition initialState (MakeMove Black blackMove (Time 0)) of
          Right tr1 -> case tr1.newState of
            State _ _ (Active _ whiteMoves _) -> do
              let whiteMove = case whiteMoves of
                    (mc : _) -> mc.move
                    [] -> error "no white moves"
              case transition tr1.newState (MakeMove White whiteMove (Time 0)) of
                Right tr2 -> do
                  let State board2 moves2 _ = tr2.newState
                      stateWithUndo =
                        State
                          board2
                          moves2
                          ( Active
                              Black
                              (validMovesForPosition moves2)
                              (Just $ PendingAction UndoRequest Black)
                          )
                  case transition stateWithUndo (AcceptUndo White) of
                    Right tr3 -> case tr3.newState of
                      State _ moves3 (Active turn3 _ _) -> do
                        length moves3 @?= 0
                        turn3 @?= Black
                      other -> fail $ "Expected Active, got " <> show other
                    Left e -> fail $ "Expected Right, got Left " <> show e
                Left e -> fail $ "White move failed: " <> show e
            other -> fail $ "Expected Active after black move, got " <> show other
          Left e -> fail $ "Black move failed: " <> show e
    ]
