import { useNavigate } from "@solidjs/router";
import { createSignal, For, type JSX, Match, Show, Switch } from "solid-js";
import type { Move } from "../board-logic";
import { type GameMode, useGame } from "../game-context";
import { useToasts } from "../toast-context";
import AiInfoPanel from "./AiInfoPanel";
import Board from "./Board";
import Chat from "./Chat";
import GameStatus from "./GameStatus";
import MoveHistory from "./MoveHistory";
import BottomSheet from "./ui/BottomSheet";
import Button from "./ui/Button";
import BalanceIcon from "./ui/icons/BalanceIcon";
import ChatIcon from "./ui/icons/ChatIcon";
import FlagIcon from "./ui/icons/FlagIcon";
import HomeIcon from "./ui/icons/HomeIcon";
import ListIcon from "./ui/icons/ListIcon";
import NextIcon from "./ui/icons/NextIcon";
import PrevIcon from "./ui/icons/PrevIcon";
import SkipBackIcon from "./ui/icons/SkipBackIcon";
import SkipForwardIcon from "./ui/icons/SkipForwardIcon";
import UndoIcon from "./ui/icons/UndoIcon";
import Toolbar from "./ui/Toolbar";

interface ToolbarDef {
  icon: JSX.Element;
  label: string;
  onClick: () => void;
  disabled: () => boolean;
}

interface ActionDef {
  label: string;
  onClick: () => void;
  disabled: () => boolean;
}

const modeActions: Record<GameMode, string[]> = {
  hotseat: ["undo"],
  ai: ["undo", "resign"],
  online: ["resign", "draw"],
};

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
  connecting?: boolean;
}

export default function GameLayout(props: GameLayoutProps) {
  const navigate = useNavigate();
  const game = useGame();
  const { toasts, dismiss } = useToasts();

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

  const actionDefs: Record<string, ActionDef> = {
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

  const activeActions = () => modeActions[props.mode] || modeActions.hotseat;
  const activeButtons = () => modeButtons[props.mode] || modeButtons.hotseat;
  const hasSecondPanel = () => props.mode === "online" || props.mode === "ai";

  const blackName = () => game.store.game.players?.black || "Black";
  const whiteName = () => game.store.game.players?.white || "White";
  const moveCount = () => game.store.game.moveHistory.length;

  return (
    <div class="main-layout">
      <Show when={props.connecting}>
        <div class="connecting-overlay">
          <div class="connecting-overlay__content">
            <span class="connecting-overlay__spinner" />
            <span>Connecting...</span>
          </div>
        </div>
      </Show>

      <div class="toast-stack">
        <For each={toasts()}>
          {(toast) => (
            <div class="error-toast">
              <span class="error-toast__message">{toast.error.message}</span>
              <button
                type="button"
                class="error-toast__close"
                onClick={() => dismiss(toast.id)}
              >
                &times;
              </button>
            </div>
          )}
        </For>
      </div>

      {/* Left column — players, captures, actions (desktop) */}
      <div class="col-left desktop-only">
        <div
          class={`ply black${isBlackActive() ? " active" : ""}${!isBlackActive() ? " opp" : ""}`}
        >
          <span class="nm">{blackName()}</span>
          <span class="hr" />
          <span class="ck">7:28</span>
        </div>
        <div class="caps-row top w">
          <For
            each={Array.from({ length: game.capturedPieces().white })}
          >
            {() => <span class="d" />}
          </For>
        </div>
        <div class="ply-gap" />
        <div class="caps-row bot b">
          <For
            each={Array.from({ length: game.capturedPieces().black })}
          >
            {() => <span class="d" />}
          </For>
        </div>
        <div
          class={`ply white${isWhiteActive() ? " active" : ""}${!isWhiteActive() ? " opp" : ""}`}
        >
          <span class="nm">{whiteName()}</span>
          <span class="hr" />
          <span class="ck">7:28</span>
        </div>
        <div class="side-acts">
          <For each={activeActions()}>
            {(key) => {
              const def = actionDefs[key];
              return (
                <a
                  classList={{ disabled: def.disabled() }}
                  onClick={() => !def.disabled() && def.onClick()}
                >
                  {def.label}
                </a>
              );
            }}
          </For>
        </div>
      </div>

      {/* Center — board */}
      <div class="board-col">
        <Board onMove={props.onMove} />
      </div>

      {/* Right column — moves (desktop) */}
      <div class="col-right desktop-only">
        <GameStatus />
        <div class="mv-head">
          <span>Moves</span>
          <span class="ct">{moveCount()}</span>
        </div>
        <MoveHistory />
        <div class="mv-nav">
          <a
            classList={{ disabled: !game.canViewPrev() }}
            onClick={() => game.canViewPrev() && game.viewStart()}
          >
            &laquo;
          </a>
          <a
            classList={{ disabled: !game.canViewPrev() }}
            onClick={() => game.canViewPrev() && game.viewPrev()}
          >
            &lsaquo;
          </a>
          <a
            classList={{ disabled: !game.canViewNext() }}
            onClick={() => game.canViewNext() && game.viewNext()}
          >
            &rsaquo;
          </a>
          <a
            classList={{ disabled: !game.canViewNext() }}
            onClick={() => game.canViewNext() && game.viewEnd()}
          >
            &raquo;
          </a>
        </div>
      </div>

      {/* Mobile status */}
      <div class="mobile-only mobile-status">
        <GameStatus />
      </div>

      {/* Mobile toolbar */}
      <nav class="mobile-only mobile-toolbar">
        <For each={activeButtons()}>
          {(key) => {
            const def = toolbarDefs[key];
            return (
              <button
                type="button"
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
          type="button"
          class="mobile-toolbar-item"
          onClick={() => setMovesSheetOpen(true)}
        >
          <ListIcon />
          <span>Moves</span>
        </button>
        {hasSecondPanel() && (
          <button
            type="button"
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
