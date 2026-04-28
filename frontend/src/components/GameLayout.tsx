import { useNavigate } from "@solidjs/router";
import { createSignal, For, type JSX, Match, Show, Switch } from "solid-js";
import type { Move } from "../board-logic";
import { type GameMode, useGame } from "../game-context";
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

      {/* Left column — players, captures, actions (desktop) */}
      <div class="sidebar-left desktop-only">
        <div
          class={`player black${isBlackActive() ? " active" : ""}${!isBlackActive() ? " inactive" : ""}`}
        >
          <span class="player-name">{blackName()}</span>
          <span class="player-rule" />
          <span class="player-clock">7:28</span>
        </div>
        <div class="captures top white">
          <For each={Array.from({ length: game.capturedPieces().white })}>
            {() => <span class="pip" />}
          </For>
        </div>
        <div class="player-gap" />
        <div class="captures bot black">
          <For each={Array.from({ length: game.capturedPieces().black })}>
            {() => <span class="pip" />}
          </For>
        </div>
        <div
          class={`player white${isWhiteActive() ? " active" : ""}${!isWhiteActive() ? " inactive" : ""}`}
        >
          <span class="player-name">{whiteName()}</span>
          <span class="player-rule" />
          <span class="player-clock">7:28</span>
        </div>
        <div class="game-actions">
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
      <div class="sidebar-right desktop-only">
        <div class="moves-header">
          <span>Moves</span>
          <span class="count">{moveCount()}</span>
        </div>
        <div class="moves-scroll">
          <MoveHistory />
        </div>
        <GameStatus />
        <div class="moves-nav">
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
