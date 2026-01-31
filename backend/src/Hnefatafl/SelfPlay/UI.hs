{-# LANGUAGE DataKinds #-}

module Hnefatafl.SelfPlay.UI (
  UIState (..),
  UIEvent (..),
  ScoreState (..),
  mkInitialScoreState,
  updateScoreState,
  runSelfPlayUI,
) where

import Brick (
  App (..),
  BrickEvent (..),
  EventM,
  Widget (..),
  customMain,
  fill,
  hBox,
  hLimit,
  neverShowCursor,
  padBottom,
  render,
  str,
  txt,
  vBox,
  vLimit,
 )
import Brick.AttrMap (attrMap)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Types (Context (..), Size (..), getContext)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (Padding (..))
import Data.List.Split (chunksOf)
import Data.Map.Strict (delete, insert, lookup, toList)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async, cancel)
import Effectful.Concurrent.STM (TChan, atomically, readTChan)
import Graphics.Vty (Event (..), Key (..), defaultConfig)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Platform.Unix (mkVty)
import Hnefatafl.Board (formatMoveResult)
import Hnefatafl.Core.Data (MoveResult (..))
import Hnefatafl.SelfPlay (
  CompletedGame (..),
  GameKey,
  GameSetup (..),
  GameResult (..),
  Player (..),
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  StateUpdatePayload (..),
  gameMoveEventToMoveResult,
  mkGameName,
 )
import Optics ((%~))
import Optics.State.Operators ((%=))
import Text.Printf (printf)
import Prelude hiding (atomically, toList)

data ScoreState = ScoreState
  { moveDifferenceSum :: Int
  -- ^ running sum of (old_moves - new_moves) for tied pairs
  , tiedPairCount :: Int
  -- ^ count of pairs where each engine won once
  , unpaired :: Map Int GameResult
  -- ^ keyed by game id, value is the game result (partner has opposite newAsBlack)
  , newPairWins :: Int
  -- ^ count of pairs where new engine won both games
  , oldPairWins :: Int
  -- ^ count of pairs where old engine won both games
  }
  deriving (Show, Eq, Generic)

-- | Internal state for the Brick application
data UIState = UIState
  { totalGames :: Int
  , completedGames :: Int
  , recentEvents :: [StateUpdate]
  , ongoingGames :: Map GameKey MoveResult
  , processingComplete :: Bool
  , scoreState :: ScoreState
  }
  deriving (Show, Eq, Generic)

-- | Events that can be sent to the Brick application
data UIEvent
  = SelfPlayEvent StateUpdate
  | Tick
  deriving (Show, Eq)

-- | Create initial UI state from ProcessingStateSnapshot
mkInitialUIState :: ProcessingStateSnapshot -> UIState
mkInitialUIState snapshot =
  UIState
    { totalGames = length snapshot.unprocessedGames + length snapshot.completedGames
    , completedGames = length snapshot.completedGames
    , recentEvents = []
    , ongoingGames = mempty
    , processingComplete = False
    , scoreState = mkInitialScoreState snapshot.completedGames
    }

-- | Helper: did the "new" engine win this game?
newEngineWon :: Bool -> Player -> Bool
newEngineWon newAsBlack winner = newAsBlack == (winner == Black)

-- | Process a matched pair and update the score state
processPair :: GameSetup -> GameResult -> GameResult -> ScoreState -> ScoreState
processPair setup gameResult partnerResult scoreState =
  let newWonCurrent = newEngineWon setup.newAsBlack gameResult.winner
      newWonPartner = newEngineWon (not setup.newAsBlack) partnerResult.winner
   in case (newWonCurrent, newWonPartner) of
        (True, True) ->
          scoreState & #newPairWins %~ (+ 1)
        (False, False) ->
          scoreState & #oldPairWins %~ (+ 1)
        (True, False) ->
          let moveDiff = partnerResult.moves - gameResult.moves
           in scoreState
                & #moveDifferenceSum %~ (+ moveDiff)
                & #tiedPairCount %~ (+ 1)
        (False, True) ->
          let moveDiff = gameResult.moves - partnerResult.moves
           in scoreState
                & #moveDifferenceSum %~ (+ moveDiff)
                & #tiedPairCount %~ (+ 1)

updateScoreState :: StateUpdate -> ScoreState -> ScoreState
updateScoreState stateUpdate scoreState =
  case stateUpdate of
    StateUpdate _key (GameCompleted setup _moves gameResult) ->
      let gameId = setup.id
       in case lookup gameId scoreState.unpaired of
            Nothing ->
              scoreState & #unpaired %~ insert gameId gameResult
            Just partnerResult ->
              let scoreState' = scoreState & #unpaired %~ delete gameId
               in processPair setup gameResult partnerResult scoreState'
    _ -> scoreState

-- | Build initial ScoreState from completed games
mkInitialScoreState :: [CompletedGame] -> ScoreState
mkInitialScoreState completedGames =
  foldl' processCompletedGame initialScoreState completedGames
 where
  initialScoreState =
    ScoreState
      { moveDifferenceSum = 0
      , tiedPairCount = 0
      , unpaired = mempty
      , newPairWins = 0
      , oldPairWins = 0
      }

  processCompletedGame scoreState (CompletedGame setup _moves gameResult) =
    let gameId = setup.id
     in case lookup gameId scoreState.unpaired of
          Nothing ->
            scoreState & #unpaired %~ insert gameId gameResult
          Just partnerResult ->
            let scoreState' = scoreState & #unpaired %~ delete gameId
             in processPair setup gameResult partnerResult scoreState'

-- | Format the score display: "4W-0L-2T (+8, 2 unpaired)"
formatScore :: ScoreState -> Text
formatScore scoreState = baseScore <> unpairedSuffix
 where
  totalPairs = scoreState.newPairWins + scoreState.oldPairWins + scoreState.tiedPairCount
  unpairedCount = length scoreState.unpaired
  netPoints = (scoreState.newPairWins - scoreState.oldPairWins) * 2

  baseScore
    | totalPairs == 0 = "--"
    | otherwise =
        show scoreState.newPairWins
          <> "W-"
          <> show scoreState.oldPairWins
          <> "L-"
          <> show scoreState.tiedPairCount
          <> "T ("
          <> showSign netPoints
          <> ")"

  unpairedSuffix
    | unpairedCount == 0 = ""
    | otherwise = " (" <> show unpairedCount <> " unpaired)"

  showSign n
    | n > 0 = "+" <> show n
    | n == 0 = "0"
    | otherwise = show n

-- | Format move average: "+3.5 moves" or "--"
formatMoveAvg :: ScoreState -> Text
formatMoveAvg scoreState
  | scoreState.tiedPairCount == 0 = "--"
  | otherwise = showSign avgDiff <> " moves"
 where
  avgDiff =
    fromIntegral scoreState.moveDifferenceSum
      / fromIntegral scoreState.tiedPairCount ::
      Double
  showSign n
    | n > 0 = "+" <> toText (printf "%.1f" n :: String)
    | otherwise = toText (printf "%.1f" n :: String)

-- | Handle events and update UI state
handleEvent :: BrickEvent () UIEvent -> EventM () UIState ()
handleEvent = \case
  VtyEvent (EvKey (KChar 'q') []) -> halt
  VtyEvent (EvKey KEsc []) -> halt
  VtyEvent (EvKey _ _) -> do
    -- If processing is complete, any key exits
    s <- get
    when s.processingComplete halt
  AppEvent (SelfPlayEvent stateUpdate) -> do
    -- Add event to recent events (keep last 5)
    #recentEvents %= addEvent stateUpdate
    -- Update ongoing games and completed counter
    case stateUpdate of
      StateUpdate _key (GameClaimed _setup) -> pure () -- Game started, but no moves yet
      StateUpdate key (GameProgressed moveEvent) -> do
        let moveResult = gameMoveEventToMoveResult moveEvent
        #ongoingGames %= insert key moveResult
      StateUpdate key (GameCompleted _setup _moves _result) -> do
        #completedGames %= (+ 1)
        #ongoingGames %= delete key
        #scoreState %= updateScoreState stateUpdate
        -- Check if all games are completed
        s <- get
        when (s.completedGames >= s.totalGames) $
          #processingComplete %= const True
  _ -> pure ()
 where
  addEvent :: StateUpdate -> [StateUpdate] -> [StateUpdate]
  addEvent newEvent events = take 5 (newEvent : events)

-- | Render the UI
drawUI :: UIState -> [Widget ()]
drawUI appState = [ui]
 where
  ui =
    vBox
      [ statusWidget
      , hBorder
      , padBottom Max boardsWidget
      ]

  statusWidget =
    str $
      "Games: "
        <> show (length appState.ongoingGames)
        <> "/"
        <> show appState.completedGames
        <> "/"
        <> show appState.totalGames
        <> " | Pairwise score: "
        <> toString (formatScore appState.scoreState)
        <> " | Movecount Advantage: "
        <> toString (formatMoveAvg appState.scoreState)
        <> " | "
        <> if appState.processingComplete
          then "Processing complete! Press any key to exit"
          else "Press 'q' or 'Esc' to quit"

  boardsWidget = case toList appState.ongoingGames of
    [] -> center $ str "(No active games)"
    games -> renderBoardGrid (sortOn fst games)

  -- Board dimensions (just the board itself, padding added between)
  boardWidth = 40 -- board width
  boardHeight = 15 -- 1 title line + 14 board lines (13 board + 1 blank from formatMoveResult)

  -- | Render items in a grid with evenly distributed spacing
  paddedGrid ::
    Int -> -- item width
    Int -> -- item height
    [a] -> -- items to render
    (a -> Widget ()) -> -- render function for each item
    Widget ()
  paddedGrid itemWidth itemHeight items renderItem = Widget Fixed Fixed $ do
    ctx <- getContext
    let availWidth = ctx.availWidth
        availHeight = ctx.availHeight

        -- Calculate how many items fit per row
        calculateItemsPerRow :: Int -> Int
        calculateItemsPerRow n =
          let widthForItems = n * itemWidth
              minSpacing = n + 1 -- minimum 1 char spacing
              required = widthForItems + minSpacing
           in if required <= availWidth && n < length items
                then calculateItemsPerRow (n + 1)
                else n - 1

        itemsPerRow = max 1 (calculateItemsPerRow 1)

        -- Calculate horizontal spacing
        totalItemWidth = itemsPerRow * itemWidth
        remainingWidth = availWidth - totalItemWidth
        hSpacing = max 1 (remainingWidth `div` (itemsPerRow + 1))

        -- Calculate vertical layout
        rowsToShow = max 1 (availHeight `div` (itemHeight + 1))
        totalItemHeight = rowsToShow * itemHeight
        remainingHeight = availHeight - totalItemHeight
        vSpacing = max 1 (remainingHeight `div` (rowsToShow + 1))

        displayItems = take (itemsPerRow * rowsToShow) items
        groupedItems = chunksOf itemsPerRow displayItems

        hSpacer = hLimit hSpacing $ fill ' '
        vSpacer = vLimit vSpacing $ fill ' '

        renderRow rowItems =
          hBox $ hSpacer : intersperse hSpacer (map renderItem rowItems) ++ [hSpacer]

    render $ vBox $ vSpacer : intersperse vSpacer (map renderRow groupedItems) ++ [vSpacer]

  renderBoard :: (GameKey, MoveResult) -> Widget ()
  renderBoard (key, moveResult) =
    let name = mkGameName key
     in hLimit boardWidth $
          vLimit boardHeight $
            vBox
              [ txt name
              , txt $ formatMoveResult moveResult
              ]

  renderBoardGrid :: [(GameKey, MoveResult)] -> Widget ()
  renderBoardGrid games = paddedGrid boardWidth boardHeight games renderBoard

-- | Brick application configuration
app :: App UIState UIEvent ()
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const (attrMap defAttr [])
    }

-- | Event reader that reads from TChan and forwards to Brick
eventReader ::
  (IOE :> es, Concurrent :> es) =>
  TChan StateUpdate ->
  BChan UIEvent ->
  Eff es ()
eventReader selfPlayChan brickChan = forever $ do
  stateUpdate <- atomically $ readTChan selfPlayChan
  liftIO $ writeBChan brickChan (SelfPlayEvent stateUpdate)

-- | Run the Brick UI application
runSelfPlayUI ::
  (IOE :> es, Concurrent :> es) =>
  ProcessingStateSnapshot ->
  TChan StateUpdate ->
  Eff es ()
runSelfPlayUI snapshot selfPlayChan = do
  -- Create brick event channel
  brickChan <- liftIO $ newBChan 10

  -- Start event reader using async
  eventReaderAsync <- async $ eventReader selfPlayChan brickChan

  -- Create vty instance and run the brick application
  let buildVty = mkVty defaultConfig
  initialVty <- liftIO buildVty

  -- Run the brick application
  liftIO $
    void $
      customMain initialVty buildVty (Just brickChan) app (mkInitialUIState snapshot)

  -- Clean up the event reader when the UI exits
  cancel eventReaderAsync
