{-# LANGUAGE QuasiQuotes #-}

module Html.AI where

import Board.Constant (startBoard)
import Data.Aeson.Text (encodeToLazyText)
import Data.Map qualified as M
import Data.UUID (UUID)
import Game.AI (GameState (..), initialGameState, toSeeMovesMap)
import Html.Board (renderBoard)
import Html.Wrapper (wrapper)
import Lucid (Html)
import Lucid.Html5
import Text.RawString.QQ

aiPage :: UUID -> GameState -> Html ()
aiPage hotseatId gs = wrapper "board" $ do
  script_ $
    "let socket = new WebSocket(\"ws://localhost:8080/aiws/"
      <> show hotseatId
      <> "\")"
      <> [r|

   socket.onmessage = function(payload) {
     console.log(`[message] Data received from server: ${payload.data}`);
     let event = JSON.parse(payload.data)
     console.log(event);
     switch (event.tag) {
       case "UpdateBoard":
         let boardWrapper = document.getElementById("board-wrapper")
         boardWrapper.innerHTML = event.contents.board
         boardCommands = event.contents.commands
         break;
       case "UpdateStatus":
         let status = document.getElementById("status")
         status.innerHTML = event.contents
         break;
       default:
         console.log("Unhandled event:" + event)
         break;
     }
   };
  |]
  h2_ [id_ "status"] "Black to move"
  div_ [id_ "board-wrapper"] $ do
    script_ $ "let boardCommands = " <> initialCommands gs
    renderBoard gs.board (M.keysSet gs.moves) mempty

initialCommands :: GameState -> Text
initialCommands gs = toStrict (encodeToLazyText $ toSeeMovesMap gs.moves)
