import { Show, For } from "solid-js";
import { useNavigate } from "@solidjs/router";
import {
  canViewPrev,
  canViewNext,
  handleViewStart,
  handleViewPreviousMove,
  handleViewNextMove,
  handleViewEnd,
  handleUndo,
  handleResign,
  handleOfferDraw,
  store,
} from "../state.js";
import MoveHistory from "./MoveHistory.jsx";
import Button from "./ui/Button.jsx";
import ButtonGroup from "./ui/ButtonGroup.jsx";
import Toolbar from "./ui/Toolbar.jsx";
import SkipBackIcon from "./ui/icons/SkipBackIcon.jsx";
import PrevIcon from "./ui/icons/PrevIcon.jsx";
import NextIcon from "./ui/icons/NextIcon.jsx";
import SkipForwardIcon from "./ui/icons/SkipForwardIcon.jsx";

const modeButtons = {
  hotseat: ["newGame", "undo"],
  ai: ["newGame", "undo", "resign"],
  online: ["newGame", "resign", "draw"],
};

export default function Controls(props) {
  const navigate = useNavigate();

  const gameActive = () => !store.game.gameOver;

  const buttonDefs = {
    newGame: { label: "New Game", onClick: () => navigate("/"), disabled: () => false },
    undo: { label: "Undo", onClick: handleUndo, disabled: () => !gameActive() || store.game.moveHistory.length === 0 },
    resign: { label: "Resign", onClick: handleResign, disabled: () => !gameActive() },
    draw: { label: "Draw", onClick: handleOfferDraw, disabled: () => !gameActive() },
  };

  const statusText = () => {
    if (store.game.gameOver) {
      if (store.game.gameOver.winner === "draw") return "Draw";
      const winner = store.game.gameOver.winner === "white" ? "White" : "Black";
      return `${winner} wins — ${store.game.gameOver.reason}`;
    }
    const player = store.game.currentPlayer === "black" ? "Black" : "White";
    return `${player} to move`;
  };

  const activeButtons = () => modeButtons[props.mode] || modeButtons.hotseat;

  return (
    <>
      <div class="game-status">
        {statusText()}
        <Show when={gameActive()}>
          <span class="thinking-dots">
            <span>.</span>
            <span>.</span>
            <span>.</span>
          </span>
        </Show>
      </div>
      <div class="move-section">
        <MoveHistory />
        <Toolbar aria-label="Move navigation">
          <Button disabled={!canViewPrev()} onClick={handleViewStart}>
            <SkipBackIcon />
          </Button>
          <Button disabled={!canViewPrev()} onClick={handleViewPreviousMove}>
            <PrevIcon />
          </Button>
          <Button disabled={!canViewNext()} onClick={handleViewNextMove}>
            <NextIcon />
          </Button>
          <Button disabled={!canViewNext()} onClick={handleViewEnd}>
            <SkipForwardIcon />
          </Button>
        </Toolbar>
      </div>
      <ButtonGroup>
        <For each={activeButtons()}>
          {(key) => {
            const def = buttonDefs[key];
            return (
              <Button disabled={def.disabled()} onClick={def.onClick}>
                {def.label}
              </Button>
            );
          }}
        </For>
      </ButtonGroup>
    </>
  );
}
