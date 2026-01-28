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
  GameDefinition (..),
  GameName (..),
  GameResult (..),
  Player (..),
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  gameMoveEventToMoveResult,
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
  , ongoingGames :: Map GameName MoveResult
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
processPair :: GameDefinition -> GameResult -> GameResult -> ScoreState -> ScoreState
processPair gameDef gameResult partnerResult scoreState =
  let newWonCurrent = newEngineWon gameDef.newAsBlack gameResult.winner
      newWonPartner = newEngineWon (not gameDef.newAsBlack) partnerResult.winner
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
    GameCompleted _gameName gameDef gameResult ->
      let gameId = gameDef.id
       in case lookup gameId scoreState.unpaired of
            Nothing ->
              scoreState & #unpaired %~ insert gameId gameResult
            Just partnerResult ->
              let scoreState' = scoreState & #unpaired %~ delete gameId
               in processPair gameDef gameResult partnerResult scoreState'
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

  processCompletedGame scoreState (CompletedGame gameDef gameResult) =
    let gameId = gameDef.id
     in case lookup gameId scoreState.unpaired of
          Nothing ->
            scoreState & #unpaired %~ insert gameId gameResult
          Just partnerResult ->
            let scoreState' = scoreState & #unpaired %~ delete gameId
             in processPair gameDef gameResult partnerResult scoreState'

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
      GameClaimed _ _ -> pure () -- Game started, but no moves yet
      GameProgressed gameName moveEvent -> do
        let moveResult = gameMoveEventToMoveResult moveEvent
        #ongoingGames %= insert gameName moveResult
      GameCompleted gameName _ _ -> do
        #completedGames %= (+ 1)
        #ongoingGames %= delete gameName
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

  renderBoardGrid :: [(GameName, MoveResult)] -> Widget ()
  renderBoardGrid games = Widget Fixed Fixed $ do
    -- Get available terminal space
    ctx <- getContext
    let termWidth = ctx.availWidth
        termHeight = ctx.availHeight
        availableHeight = termHeight - 2 -- Account for status line

        -- Calculate how many boards fit
        -- Start with 1 board per row and increase while they fit
        calculateBoardsPerRow :: Int -> Int
        calculateBoardsPerRow n =
          let widthForBoards = n * boardWidth
              minSpacing = n + 1  -- minimum 1 char spacing
              required = widthForBoards + minSpacing
           in if required <= termWidth && n < length games
                then calculateBoardsPerRow (n + 1)
                else n - 1

        boardsPerRow = max 1 (calculateBoardsPerRow 1)

        -- Calculate spacing
        totalBoardWidth = boardsPerRow * boardWidth
        remainingWidth = termWidth - totalBoardWidth
        hSpacing = max 1 (remainingWidth `div` (boardsPerRow + 1))

        -- Calculate vertical layout
        rowsToShow = max 1 (availableHeight `div` (boardHeight + 1))
        totalBoardHeight = rowsToShow * boardHeight
        remainingHeight = availableHeight - totalBoardHeight
        vSpacing = max 1 (remainingHeight `div` (rowsToShow + 1))

        displayGames = take (boardsPerRow * rowsToShow) games
        groupedGames = chunksOf boardsPerRow displayGames

        renderSingleBoard :: (GameName, MoveResult) -> Widget ()
        renderSingleBoard (GameName name, moveResult) =
          hLimit boardWidth $
            vLimit boardHeight $
              vBox
                [ txt name
                , txt $ formatMoveResult moveResult
                ]

        hSpacer = hLimit hSpacing $ fill ' '
        vSpacer = vLimit vSpacing $ fill ' '

        renderRow :: [(GameName, MoveResult)] -> Widget ()
        renderRow rowGames =
          hBox $ hSpacer : intersperse hSpacer (map renderSingleBoard rowGames) ++ [hSpacer]

    render $ vBox $ vSpacer : intersperse vSpacer (map renderRow groupedGames) ++ [vSpacer]

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
