import { For } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { useGame, type GameMode } from "../game-context";
import GameStatus from "./GameStatus";
import MoveHistory from "./MoveHistory";
import Button from "./ui/Button";
import ButtonGroup from "./ui/ButtonGroup";
import Toolbar from "./ui/Toolbar";
import SkipBackIcon from "./ui/icons/SkipBackIcon";
import PrevIcon from "./ui/icons/PrevIcon";
import NextIcon from "./ui/icons/NextIcon";
import SkipForwardIcon from "./ui/icons/SkipForwardIcon";

interface ButtonDef {
  label: string;
  onClick: () => void;
  disabled: () => boolean;
}

interface ControlsProps {
  mode: GameMode;
  onResign?: () => void;
  onUndo?: () => void;
  onDraw?: () => void;
}

const modeButtons: Record<GameMode, string[]> = {
  hotseat: ["newGame", "undo"],
  ai: ["newGame", "undo", "resign"],
  online: ["newGame", "resign", "draw"],
};

export default function Controls(props: ControlsProps) {
  const navigate = useNavigate();
  const game = useGame();

  const gameActive = () => !game.store.game.gameOver;

  const buttonDefs: Record<string, ButtonDef> = {
    newGame: { label: "New Game", onClick: () => navigate("/"), disabled: () => false },
    undo: {
      label: "Undo",
      onClick: () => props.onUndo?.(),
      disabled: () => !gameActive() || game.store.game.moveHistory.length === 0,
    },
    resign: {
      label: "Resign",
      onClick: () => props.onResign?.(),
      disabled: () => !gameActive(),
    },
    draw: {
      label: "Draw",
      onClick: () => props.onDraw?.(),
      disabled: () => !gameActive(),
    },
  };

  const activeButtons = () => modeButtons[props.mode] || modeButtons.hotseat;

  return (
    <>
      <GameStatus />
      <div class="move-section">
        <MoveHistory />
        <Toolbar aria-label="Move navigation">
          <Button disabled={!game.canViewPrev()} onClick={game.viewStart}>
            <SkipBackIcon />
          </Button>
          <Button disabled={!game.canViewPrev()} onClick={game.viewPrev}>
            <PrevIcon />
          </Button>
          <Button disabled={!game.canViewNext()} onClick={game.viewNext}>
            <NextIcon />
          </Button>
          <Button disabled={!game.canViewNext()} onClick={game.viewEnd}>
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
