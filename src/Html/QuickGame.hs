{-# LANGUAGE QuasiQuotes #-}

module Html.QuickGame where
import Lucid (Html)
import Board.Constant (startBoard)
import Html.Board (renderBoard)
import Html.Wrapper (wrapper)
import Lucid.Html5
import Text.RawString.QQ
import QuickGame (initialGameState, toSeeMovesMap, GameState(..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Map qualified as M

quickGamePage :: Html ()
quickGamePage = wrapper "board" $ do
  script_ [r|
   let socket = new WebSocket("ws://localhost:8080/quickgamews")

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
    script_ $ "let boardCommands = " <> initialCommands
    renderBoard initialGameState.board (M.keysSet initialGameState.moves) mempty

initialCommands :: Text
initialCommands = toStrict (encodeToLazyText $ toSeeMovesMap initialGameState.moves)
