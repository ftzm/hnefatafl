module Hnefatafl.Game.AITest where

import Chronos (Time (..))
import Hnefatafl.Bindings (
  nextGameStateWithMovesTrusted,
  startBlackMoves,
  startBoard,
 )
import Hnefatafl.Core.Data (
  ExternBoard,
  MoveWithCaptures (..),
  Outcome (..),
  PlayerColor (..),
 )
import Hnefatafl.Game.AI (
  Event (..),
  Phase (..),
  State (..),
  TransitionResult (..),
  reconstruct,
  transition,
 )
import Hnefatafl.Game.Common (
  AppliedMove (..),
  PendingAction (..),
  PendingActionType (..),
  currentBoard,
  mkAppliedMove,
  outcomeFromEngine,
  validMovesForPosition,
  zobristHashes,
 )
import Hnefatafl.Game.TestUtil (
  PersistenceStore (..),
  applyEvents,
  emptyStore,
 )
import Test.QuickCheck (Gen, elements, frequency, (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Prelude hiding (State, state)

humanColor :: PlayerColor
humanColor = Black

engineColor :: PlayerColor
engineColor = White

fromStore :: PersistenceStore -> State
fromStore store =
  reconstruct
    humanColor
    (currentBoard store.storedMoves)
    store.storedMoves
    store.storedOutcome
    store.storedPendingAction

-- | Compute a real engine move from the current position
realEngineMove :: ExternBoard -> [AppliedMove] -> (AppliedMove, Maybe Outcome)
realEngineMove board moves =
  case validMovesForPosition moves of
    (mc : _) ->
      let hashes = zobristHashes moves
          (mr, status, _) =
            fromRight (error "valid move rejected") $
              nextGameStateWithMovesTrusted board False mc.move hashes
          applied = mkAppliedMove mr (Time 0)
       in (applied, outcomeFromEngine status)
    [] -> error "no valid moves for engine"

genEvent :: State -> Maybe (Gen Event)
genEvent (State _ _ (Finished _)) = Nothing
genEvent (State _ moves (PlayerTurn validMoves pending)) =
  Just $
    frequency $
      concat
        [
          [ (5, MakeMove . (.move) <$> elements validMoves <*> pure (Time 0))
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
genEvent (State board moves (EngineThinking pending)) =
  let (applied, outcome) = realEngineMove board moves
   in Just $
        frequency $
          concat
            [
              [ (5, pure $ EngineMove applied outcome)
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

initialState :: State
initialState =
  State startBoard [] (PlayerTurn (toList startBlackMoves) Nothing)

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
    QC.forAll (genSequence initialState) $ \steps ->
      let s = case steps of
            [] -> initialState
            _ -> (snd (fromMaybe (error "empty") (viaNonEmpty last steps))).newState
          store = foldl' (\acc (_, tr) -> applyEvents tr.events acc) emptyStore steps
       in fromStore store === s

-- | Make the first black move from starting position, returning the resulting applied move
firstBlackApplied :: AppliedMove
firstBlackApplied =
  let move = (head startBlackMoves).move
      (mr, _, _) =
        fromRight (error "first move failed") $
          nextGameStateWithMovesTrusted startBoard True move []
   in mkAppliedMove mr (Time 0)

test_aiCancellation :: TestTree
test_aiCancellation =
  testGroup
    "AI implicit cancellation"
    [ testCase "Player move cancels engine's draw offer" $ do
        let pending = Just $ PendingAction DrawOffer engineColor
            st = State startBoard [] (PlayerTurn (toList startBlackMoves) pending)
            move = (head startBlackMoves).move
        case transition humanColor st (MakeMove move (Time 0)) of
          Right tr -> case tr.newState of
            State _ _ (EngineThinking p) -> p @?= Nothing
            other -> fail $ "Expected EngineThinking, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Player move preserves own draw offer" $ do
        let pending = Just $ PendingAction DrawOffer humanColor
            st = State startBoard [] (PlayerTurn (toList startBlackMoves) pending)
            move = (head startBlackMoves).move
        case transition humanColor st (MakeMove move (Time 0)) of
          Right tr -> case tr.newState of
            State _ _ (EngineThinking p) -> p @?= pending
            other -> fail $ "Expected EngineThinking, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Engine move cancels human's draw offer" $ do
        let applied = firstBlackApplied
            moves = [applied]
            board = applied.boardAfter
            pending = Just $ PendingAction DrawOffer humanColor
            st = State board moves (EngineThinking pending)
            (engineApplied, engineOutcome) = realEngineMove board moves
        case transition humanColor st (EngineMove engineApplied engineOutcome) of
          Right tr -> case tr.newState of
            State _ _ (PlayerTurn _ p) -> p @?= Nothing
            other -> fail $ "Expected PlayerTurn, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    , testCase "Engine move preserves own draw offer" $ do
        let applied = firstBlackApplied
            moves = [applied]
            board = applied.boardAfter
            pending = Just $ PendingAction DrawOffer engineColor
            st = State board moves (EngineThinking pending)
            (engineApplied, engineOutcome) = realEngineMove board moves
        case transition humanColor st (EngineMove engineApplied engineOutcome) of
          Right tr -> case tr.newState of
            State _ _ (PlayerTurn _ p) -> p @?= pending
            other -> fail $ "Expected PlayerTurn, got " <> show other
          Left e -> fail $ "Expected Right, got Left " <> show e
    ]
