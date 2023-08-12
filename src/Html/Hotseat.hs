{-# LANGUAGE QuasiQuotes #-}

module Html.Hotseat where

import Board.Constant (startBoard)
import Data.Aeson.Text (encodeToLazyText)
import Data.Map qualified as M
import Data.UUID (UUID)
import Game.Hotseat (GameState (..), initialGameState, toSeeMovesMap)
import Html.Board (renderBoard)
import Html.Wrapper (wrapper)
import Lucid (Html)
import Lucid.Html5
import Text.RawString.QQ

hotseatPage :: UUID -> GameState -> Html ()
hotseatPage hotseatId gs = wrapper "board" $ do
  script_ $
    "let socket = new WebSocket(\"ws://localhost:8080/hotseatws/"
      <> show hotseatId
      <> "\")"
      <> [r|

   socket.onmessage = function(payload) {
     console.log(`[message] Data received from server: ${payload.data}`);
     let events = JSON.parse(payload.data)
     console.log(events);
     for (event of events) {
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
       }
     }
   };
  |]
  h2_ [id_ "status"] "Black to move"
  div_ [id_ "board-wrapper"] $ do
    script_ $ "let boardCommands = " <> initialCommands gs
    renderBoard gs.board (M.keysSet gs.moves) mempty

initialCommands :: GameState -> Text
initialCommands gs = toStrict (encodeToLazyText $ toSeeMovesMap gs.moves)
