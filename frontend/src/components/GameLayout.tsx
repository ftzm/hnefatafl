import { Switch, Match, For, createSignal, type JSX } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Board from "./Board";
import Chat from "./Chat";
import Controls from "./Controls";
import AiInfoPanel from "./AiInfoPanel";
import PlayerBar from "./PlayerBar";
import GameStatus from "./GameStatus";
import MoveHistory from "./MoveHistory";
import Panel from "./ui/Panel";
import Button from "./ui/Button";
import Toolbar from "./ui/Toolbar";
import BottomSheet from "./ui/BottomSheet";
import ListIcon from "./ui/icons/ListIcon";
import ChatIcon from "./ui/icons/ChatIcon";
import HomeIcon from "./ui/icons/HomeIcon";
import UndoIcon from "./ui/icons/UndoIcon";
import FlagIcon from "./ui/icons/FlagIcon";
import BalanceIcon from "./ui/icons/BalanceIcon";
import SkipBackIcon from "./ui/icons/SkipBackIcon";
import PrevIcon from "./ui/icons/PrevIcon";
import NextIcon from "./ui/icons/NextIcon";
import SkipForwardIcon from "./ui/icons/SkipForwardIcon";
import { useGame, type GameMode } from "../game-context";
import type { Move } from "../board-logic";

interface ToolbarDef {
  icon: JSX.Element;
  label: string;
  onClick: () => void;
  disabled: () => boolean;
}

const modeButtons: Record<GameMode, string[]> = {
  hotseat: ["newGame", "undo"],
  ai: ["newGame", "undo", "resign"],
  online: ["newGame", "resign", "draw"],
};

interface GameLayoutProps {
  mode: GameMode;
  onMove: (move: Move) => void;
  onResign?: () => void;
  onUndo?: () => void;
  onDraw?: () => void;
}

export default function GameLayout(props: GameLayoutProps) {
  const navigate = useNavigate();
  const game = useGame();

  const [movesSheetOpen, setMovesSheetOpen] = createSignal(false);
  const [chatSheetOpen, setChatSheetOpen] = createSignal(false);

  const isBlackActive = () =>
    game.store.game.currentPlayer === "black" &&
    game.store.game.historyCursor === 0 &&
    !game.store.game.gameOver;
  const isWhiteActive = () =>
    game.store.game.currentPlayer === "white" &&
    game.store.game.historyCursor === 0 &&
    !game.store.game.gameOver;

  const gameActive = () => !game.store.game.gameOver;

  const toolbarDefs: Record<string, ToolbarDef> = {
    newGame: {
      icon: <HomeIcon />,
      label: "New",
      onClick: () => navigate("/"),
      disabled: () => false,
    },
    undo: {
      icon: <UndoIcon />,
      label: "Undo",
      onClick: () => props.onUndo?.(),
      disabled: () => !gameActive() || game.store.game.moveHistory.length === 0,
    },
    resign: {
      icon: <FlagIcon />,
      label: "Resign",
      onClick: () => props.onResign?.(),
      disabled: () => !gameActive(),
    },
    draw: {
      icon: <BalanceIcon />,
      label: "Draw",
      onClick: () => props.onDraw?.(),
      disabled: () => !gameActive(),
    },
  };

  const activeButtons = () => modeButtons[props.mode] || modeButtons.hotseat;
  const hasSecondPanel = () => props.mode === "online" || props.mode === "ai";

  return (
    <div class="main-layout">
      <div class="mobile-only mobile-status">
        <GameStatus />
      </div>

      <div class="board-section">
        <PlayerBar
          color="black"
          name={game.store.game.players?.black}
          capturedCount={game.store.game.capturedPieces.white}
          active={isBlackActive()}
        />
        <Board onMove={props.onMove} />
        <PlayerBar
          color="white"
          name={game.store.game.players?.white}
          capturedCount={game.store.game.capturedPieces.black}
          active={isWhiteActive()}
        />
      </div>

      <div class="right-column desktop-only">
        <Panel>
          <Controls
            mode={props.mode}
            onResign={props.onResign}
            onUndo={props.onUndo}
            onDraw={props.onDraw}
          />
        </Panel>
        <Switch>
          <Match when={props.mode === "online"}>
            <Chat />
          </Match>
          <Match when={props.mode === "ai"}>
            <AiInfoPanel />
          </Match>
        </Switch>
      </div>

      <nav class="mobile-only mobile-toolbar">
        <For each={activeButtons()}>
          {(key) => {
            const def = toolbarDefs[key];
            return (
              <button
                class="mobile-toolbar-item"
                disabled={def.disabled()}
                onClick={def.onClick}
              >
                {def.icon}
                <span>{def.label}</span>
              </button>
            );
          }}
        </For>
        <button
          class="mobile-toolbar-item"
          onClick={() => setMovesSheetOpen(true)}
        >
          <ListIcon />
          <span>Moves</span>
        </button>
        {hasSecondPanel() && (
          <button
            class="mobile-toolbar-item"
            onClick={() => setChatSheetOpen(true)}
          >
            <ChatIcon />
            <span>{props.mode === "online" ? "Chat" : "AI"}</span>
          </button>
        )}
      </nav>

      <BottomSheet
        open={movesSheetOpen()}
        onOpenChange={setMovesSheetOpen}
        title="Moves"
      >
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
      </BottomSheet>

      <BottomSheet
        open={chatSheetOpen()}
        onOpenChange={setChatSheetOpen}
        title={props.mode === "online" ? "Chat" : "AI Info"}
      >
        <Switch>
          <Match when={props.mode === "online"}>
            <Chat />
          </Match>
          <Match when={props.mode === "ai"}>
            <AiInfoPanel />
          </Match>
        </Switch>
      </BottomSheet>
    </div>
  );
}
