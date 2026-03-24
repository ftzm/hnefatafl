{-# LANGUAGE DataKinds #-}

module Hnefatafl.SelfPlay.UI (
  UIState (..),
  UIEvent (..),
  ScoreState (..),
  mkInitialScoreState,
  updateScoreState,
  formatScore,
  runSelfPlayUI,
) where

import Brick (
  App (..),
  BrickEvent (..),
  EventM,
  Widget (..),
  customMain,
  neverShowCursor,
  padBottom,
  str,
  vBox,
 )
import Brick.AttrMap (attrMap)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (Padding (..))
import Data.Map.Strict (delete, insert, toList)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async, cancel)
import Effectful.Concurrent.STM (TChan, atomically, readTChan)
import Graphics.Vty (Event (..), Key (..), defaultConfig)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Platform.Unix (mkVty)
import Hnefatafl.SelfPlay (
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  StateUpdatePayload (..),
  gameMoveEventToMoveResult,
 )
import Hnefatafl.SelfPlay.UI.State (
  ScoreState (..),
  UIEvent (..),
  UIState (..),
  mkInitialScoreState,
  mkInitialUIState,
  updateScoreState,
 )
import Hnefatafl.SelfPlay.UI.Widgets (renderBoardGrid)
import Optics.State.Operators ((%=))
import Text.Printf (printf)
import Prelude hiding (atomically, toList)

-- | Format the score display: "+12.5% (25W-10D-25L)"
formatScore :: ScoreState -> Text
formatScore scoreState = baseScore <> pendingSuffix
 where
  totalGames = scoreState.wins + scoreState.draws + scoreState.losses
  pendingGames = sum $ map (length . snd) $ toList scoreState.pending

  baseScore
    | totalGames == 0 = "--"
    | otherwise =
        formatPct pctAbove50
          <> " ("
          <> show scoreState.wins
          <> "W-"
          <> show scoreState.draws
          <> "D-"
          <> show scoreState.losses
          <> "L)"

  newPoints :: Double
  newPoints = fromIntegral scoreState.wins + 0.5 * fromIntegral scoreState.draws

  pctAbove50 :: Double
  pctAbove50 = ((newPoints / fromIntegral totalGames) - 0.5) * 100

  formatPct p
    | p > 0 = "+" <> toText (printf "%.1f" p :: String) <> "%"
    | p == 0 = "0%"
    | otherwise = toText (printf "%.1f" p :: String) <> "%"

  pendingSuffix
    | pendingGames == 0 = ""
    | otherwise = " (" <> show pendingGames <> " pending)"

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
        <> " | Score: "
        <> toString (formatScore appState.scoreState)
        <> " | "
        <> if appState.processingComplete
          then "Processing complete! Press any key to exit"
          else "Press 'q' or 'Esc' to quit"

  boardsWidget = case toList appState.ongoingGames of
    [] -> center $ str "(No active games)"
    games -> renderBoardGrid (sortOn fst games)

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
