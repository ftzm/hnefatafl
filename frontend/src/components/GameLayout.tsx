import { useNavigate } from "@solidjs/router";
import { createSignal, For, type JSX, Match, Show, Switch } from "solid-js";
import type { Move, PlayerColor } from "../board-logic";
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

export function formatClockMs(ms: number): string {
  const totalSeconds = Math.max(0, Math.floor(ms / 1000));
  const minutes = Math.floor(totalSeconds / 60);
  const seconds = totalSeconds % 60;
  return `${minutes}:${seconds.toString().padStart(2, "0")}`;
}

// Whether the Undo button has a move this player is allowed to take
// back. Online play only lets a player undo their own move, and the
// server rejects a request otherwise: Black moves first so needs at
// least one move played, White needs two. Hotseat / AI has no player
// color, so any played move is undoable.
export function hasUndoableMove(
  playerColor: PlayerColor | null,
  historyLength: number,
): boolean {
  if (playerColor === "white") return historyLength >= 2;
  return historyLength >= 1;
}

interface ActionDef {
  label: string;
  icon: JSX.Element;
  onClick: () => void;
  disabled: () => boolean;
}

type PlayerState = "active" | "idle" | "ended";

const modeActions: Record<GameMode, string[]> = {
  hotseat: ["newGame", "undo"],
  ai: ["newGame", "undo", "resign"],
  online: ["newGame", "undo", "resign", "draw"],
};

interface GameLayoutProps {
  mode: GameMode;
  onMove: (move: Move) => void;
  onResign?: () => void;
  onUndo?: () => void;
  onDraw?: () => void;
  connecting?: boolean;
  /** Online: a draw offer has been sent and is awaiting opponent response. */
  outgoingDrawPending?: boolean;
  /** Online: an undo request has been sent and is awaiting opponent response. */
  outgoingUndoPending?: boolean;
  /**
   * Online: an offer from the opponent (draw or undo) is currently awaiting
   * our response. The server allows only one pending action per game from
   * either side, so we must disable our own offer buttons until this slot is
   * free. Sending while it is set yields `action_already_pending`.
   */
  incomingOfferPending?: boolean;
  /**
   * Slot rendered under the top player line on desktop and below the mobile
   * status line on mobile. Used by the online controller for the offer banner.
   */
  banner?: JSX.Element;
}

export default function GameLayout(props: GameLayoutProps) {
  const navigate = useNavigate();
  const game = useGame();

  const [movesSheetOpen, setMovesSheetOpen] = createSignal(false);
  const [chatSheetOpen, setChatSheetOpen] = createSignal(false);

  const playerState = (color: "black" | "white"): PlayerState => {
    if (game.store.game.gameOver) return "ended";
    if (
      game.store.game.currentPlayer === color &&
      game.store.game.historyCursor === 0
    ) {
      return "active";
    }
    return "idle";
  };

  const gameActive = () => !game.store.game.gameOver;

  // The server allows only one pending offer per game (from either side).
  // While *anything* is pending — our outgoing offer, or an incoming one
  // from the opponent — sending another results in `action_already_pending`.
  // Gate both offer-style buttons on the union of these states.
  const anyOfferPending = () =>
    !!props.outgoingDrawPending ||
    !!props.outgoingUndoPending ||
    !!props.incomingOfferPending;

  // Single source of truth for in-game actions. Drives both the desktop
  // sidebar (label only) and the mobile toolbar (icon + label).
  const actions: Record<string, ActionDef> = {
    newGame: {
      label: "New",
      icon: <HomeIcon />,
      onClick: () => navigate("/"),
      disabled: () => false,
    },
    undo: {
      label: "Undo",
      icon: <UndoIcon />,
      onClick: () => props.onUndo?.(),
      disabled: () => {
        if (!gameActive() || anyOfferPending()) return true;
        return !hasUndoableMove(
          game.store.game.playerColor,
          game.store.game.moveHistory.length,
        );
      },
    },
    resign: {
      label: "Resign",
      icon: <FlagIcon />,
      onClick: () => props.onResign?.(),
      disabled: () => !gameActive(),
    },
    draw: {
      label: "Draw",
      icon: <BalanceIcon />,
      onClick: () => props.onDraw?.(),
      disabled: () => !gameActive() || anyOfferPending(),
    },
  };

  const activeActions = () => modeActions[props.mode] || modeActions.hotseat;
  const hasSecondPanel = () => props.mode === "online" || props.mode === "ai";

  const blackName = () => game.store.game.players?.black || "Black";
  const whiteName = () => game.store.game.players?.white || "White";
  const moveCount = () => game.store.game.moveHistory.length;

  // Top of the sidebar is the opponent; bottom is the local player. In
  // hotseat (no playerColor) we keep black on top — black moves first in
  // Hnefatafl so it's the conventional reading order. Captures swap with
  // the players: each row shows the pieces that player has captured (the
  // opposite color's pips), so the count and pip class come from the
  // *other* side's color.
  type Side = "black" | "white";
  const opposite = (c: Side): Side => (c === "black" ? "white" : "black");
  const topColor = (): Side =>
    game.store.game.playerColor === "black" ? "white" : "black";
  const bottomColor = (): Side => opposite(topColor());
  const playerName = (c: Side) => (c === "black" ? blackName() : whiteName());
  const clockForSide = (c: Side) => {
    const clock = game.store.game.clock;
    if (!clock) return null;
    return c === "white" ? clock.whiteMs : clock.blackMs;
  };

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

      {/* Left column — players, captures, actions (desktop). Top row is
          the opponent (or black in hotseat), bottom row is the local
          player. */}
      <div class="sidebar-left desktop-only">
        <div
          class={`player ${topColor()}`}
          data-state={playerState(topColor())}
        >
          <span class="player-name">{playerName(topColor())}</span>
          <span class="player-rule" />
          <Show when={clockForSide(topColor()) != null}>
            <span class="player-clock">
              {formatClockMs(clockForSide(topColor())!)}
            </span>
          </Show>
        </div>
        <div class={`captures top ${bottomColor()}`}>
          <For
            each={Array.from({ length: game.capturedPieces()[bottomColor()] })}
          >
            {() => <span class="pip" />}
          </For>
        </div>
        {props.banner}
        <div class="player-gap" />
        <div class={`captures bot ${topColor()}`}>
          <For each={Array.from({ length: game.capturedPieces()[topColor()] })}>
            {() => <span class="pip" />}
          </For>
        </div>
        <div
          class={`player ${bottomColor()}`}
          data-state={playerState(bottomColor())}
        >
          <span class="player-name">{playerName(bottomColor())}</span>
          <span class="player-rule" />
          <Show when={clockForSide(bottomColor()) != null}>
            <span class="player-clock">
              {formatClockMs(clockForSide(bottomColor())!)}
            </span>
          </Show>
        </div>
        <div class="game-actions">
          <For each={activeActions()}>
            {(key) => {
              const def = actions[key];
              return (
                <button
                  type="button"
                  disabled={def.disabled()}
                  onClick={def.onClick}
                >
                  {def.label}
                </button>
              );
            }}
          </For>
        </div>
      </div>

      {/* Mobile player header — black left, white right */}
      <div class="mobile-only mobile-player-header">
        <div class="mobile-player-side">
          <div class="captures white">
            <For
              each={Array.from({
                length: game.capturedPieces().white,
              })}
            >
              {() => <span class="pip" />}
            </For>
          </div>
          <div class="mobile-player-info">
            <span class={`mobile-player-name ${playerState("black")}`}>
              {blackName()}
            </span>
            <Show when={clockForSide("black") != null}>
              <span class={`mobile-player-clock ${playerState("black")}`}>
                {formatClockMs(clockForSide("black")!)}
              </span>
            </Show>
          </div>
        </div>
        <span class="mobile-vs">vs</span>
        <div class="mobile-player-side">
          <div class="mobile-player-info">
            <span class={`mobile-player-name ${playerState("white")}`}>
              {whiteName()}
            </span>
            <Show when={clockForSide("white") != null}>
              <span class={`mobile-player-clock ${playerState("white")}`}>
                {formatClockMs(clockForSide("white")!)}
              </span>
            </Show>
          </div>
          <div class="captures black">
            <For
              each={Array.from({
                length: game.capturedPieces().black,
              })}
            >
              {() => <span class="pip" />}
            </For>
          </div>
        </div>
      </div>

      {/* Mobile status + notifications */}
      <div class="mobile-only mobile-status">
        <GameStatus />
        {props.banner}
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
          <button
            type="button"
            disabled={!game.canViewPrev()}
            onClick={game.viewStart}
            aria-label="Jump to first move"
          >
            &laquo;
          </button>
          <button
            type="button"
            disabled={!game.canViewPrev()}
            onClick={game.viewPrev}
            aria-label="Previous move"
          >
            &lsaquo;
          </button>
          <button
            type="button"
            disabled={!game.canViewNext()}
            onClick={game.viewNext}
            aria-label="Next move"
          >
            &rsaquo;
          </button>
          <button
            type="button"
            disabled={!game.canViewNext()}
            onClick={game.viewEnd}
            aria-label="Jump to last move"
          >
            &raquo;
          </button>
        </div>
      </div>

      {/* Mobile toolbar */}
      <nav class="mobile-only mobile-toolbar">
        <For each={activeActions()}>
          {(key) => {
            const def = actions[key];
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
