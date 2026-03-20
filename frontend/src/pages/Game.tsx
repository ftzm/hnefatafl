import { Switch, Match, createEffect } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Board from "../components/Board";
import Chat from "../components/Chat";
import Controls from "../components/Controls";
import AiInfoPanel from "../components/AiInfoPanel";
import PlayerBar from "../components/PlayerBar";
import Panel from "../components/ui/Panel";
import { store, type GameMode } from "../state";

export default function Game() {
  const navigate = useNavigate();

  createEffect(() => {
    if (!store.game.mode) navigate("/", { replace: true });
  });

  const mode = (): GameMode => store.game.mode || "hotseat";

  const isBlackActive = () =>
    store.game.currentPlayer === "black" && store.game.historyCursor === 0 && !store.game.gameOver;
  const isWhiteActive = () =>
    store.game.currentPlayer === "white" && store.game.historyCursor === 0 && !store.game.gameOver;

  return (
    <div class="main-layout">
      <div class="board-section">
        <PlayerBar
          color="black"
          name={store.game.players?.black}
          capturedCount={store.game.capturedPieces.white}
          active={isBlackActive()}
        />
        <Board />
        <PlayerBar
          color="white"
          name={store.game.players?.white}
          capturedCount={store.game.capturedPieces.black}
          active={isWhiteActive()}
        />
      </div>
      <div class="right-column">
        <Panel>
          <Controls mode={mode()} />
        </Panel>
        <Switch>
          <Match when={mode() === "online"}>
            <Chat />
          </Match>
          <Match when={mode() === "ai"}>
            <AiInfoPanel />
          </Match>
        </Switch>
      </div>
    </div>
  );
}
