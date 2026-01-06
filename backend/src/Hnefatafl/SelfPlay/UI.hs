{-# LANGUAGE DataKinds #-}

module Hnefatafl.SelfPlay.UI (
  UIState (..),
  UIEvent (..),
  runSelfPlayUI,
) where

import Brick (
  App (..),
  BrickEvent (..),
  EventM,
  Widget,
  customMain,
  neverShowCursor,
  str,
  txt,
  vBox,
 )
import Brick.AttrMap (attrMap)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (halt)
import Brick.Widgets.Center (center)
import Data.Map.Strict (delete, insert, toList)
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
  GameName (..),
  ProcessingStateSnapshot (..),
  StateUpdate (..),
  gameMoveEventToMoveResult,
 )
import Hnefatafl.Serialization (moveToNotation)
import Optics.State.Operators ((%=))
import Prelude hiding (atomically, toList)

-- | Internal state for the Brick application
data UIState = UIState
  { totalGames :: Int
  , completedGames :: Int
  , recentEvents :: [StateUpdate]
  , ongoingGames :: Map GameName MoveResult
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
    }

-- | Handle events and update UI state
handleEvent :: BrickEvent () UIEvent -> EventM () UIState ()
handleEvent = \case
  VtyEvent (EvKey (KChar 'q') []) -> halt
  VtyEvent (EvKey KEsc []) -> halt
  AppEvent (SelfPlayEvent stateUpdate) -> do
    -- Add event to recent events (keep last 5)
    #recentEvents %= addEvent stateUpdate
    -- Update ongoing games and completed counter
    case stateUpdate of
      GameClaimed _ _ -> pure () -- Game started, but no moves yet
      GameProgressed gameName moveEvent -> do
        let moveResult = gameMoveEventToMoveResult moveEvent
        #ongoingGames %= insert gameName moveResult
      GameCompleted gameName _ -> do
        #completedGames %= (+ 1)
        #ongoingGames %= delete gameName
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
      , center boardWidget
      ]

  statusWidget =
    str $
      "Games: "
        <> show appState.completedGames
        <> "/"
        <> show appState.totalGames
        <> " "
        <> progressBar appState
        <> " | Press 'q' or 'Esc' to quit"

  boardWidget = case getCurrentGame appState.ongoingGames of
    Nothing ->
      vBox
        [ str "(No active games)"
        ]
    Just (gameName, moveResult) ->
      vBox
        [ txt $ "Game: " <> show gameName <> " | " <> moveToNotation moveResult.move
        , str ""
        , txt $ formatMoveResult moveResult
        ]

  getCurrentGame :: Map GameName MoveResult -> Maybe (GameName, MoveResult)
  getCurrentGame games = case sortOn fst (toList games) of
    [] -> Nothing
    (gameName, moveResult) : _ -> Just (gameName, moveResult)

  progressBar :: UIState -> String
  progressBar s =
    let completed = s.completedGames
        total = s.totalGames
        percentage = if total == 0 then 0 else (completed * 100) `div` total
        barLength = 20
        filledLength = (completed * barLength) `div` max 1 total
        filled = replicate filledLength '█'
        emptyChars = replicate (barLength - filledLength) '░'
     in "[" <> filled <> emptyChars <> "] " <> show percentage <> "%"

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
