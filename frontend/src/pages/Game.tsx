import { Switch, Match, For, createEffect, createSignal, type JSX } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Board from "../components/Board";
import Chat from "../components/Chat";
import Controls from "../components/Controls";
import AiInfoPanel from "../components/AiInfoPanel";
import PlayerBar from "../components/PlayerBar";
import GameStatus from "../components/GameStatus";
import MoveHistory from "../components/MoveHistory";
import Panel from "../components/ui/Panel";
import Button from "../components/ui/Button";
import Toolbar from "../components/ui/Toolbar";
import BottomSheet from "../components/ui/BottomSheet";
import ListIcon from "../components/ui/icons/ListIcon";
import ChatIcon from "../components/ui/icons/ChatIcon";
import HomeIcon from "../components/ui/icons/HomeIcon";
import UndoIcon from "../components/ui/icons/UndoIcon";
import FlagIcon from "../components/ui/icons/FlagIcon";
import HandshakeIcon from "../components/ui/icons/HandshakeIcon";
import SkipBackIcon from "../components/ui/icons/SkipBackIcon";
import PrevIcon from "../components/ui/icons/PrevIcon";
import NextIcon from "../components/ui/icons/NextIcon";
import SkipForwardIcon from "../components/ui/icons/SkipForwardIcon";
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
  type GameMode,
} from "../state";

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

export default function Game() {
  const navigate = useNavigate();

  const [movesSheetOpen, setMovesSheetOpen] = createSignal(false);
  const [chatSheetOpen, setChatSheetOpen] = createSignal(false);

  createEffect(() => {
    if (!store.game.mode) navigate("/", { replace: true });
  });

  const mode = (): GameMode => store.game.mode || "hotseat";

  const isBlackActive = () =>
    store.game.currentPlayer === "black" && store.game.historyCursor === 0 && !store.game.gameOver;
  const isWhiteActive = () =>
    store.game.currentPlayer === "white" && store.game.historyCursor === 0 && !store.game.gameOver;

  const gameActive = () => !store.game.gameOver;

  const toolbarDefs: Record<string, ToolbarDef> = {
    newGame: { icon: <HomeIcon />, label: "New Game", onClick: () => navigate("/"), disabled: () => false },
    undo: { icon: <UndoIcon />, label: "Undo", onClick: handleUndo, disabled: () => !gameActive() || store.game.moveHistory.length === 0 },
    resign: { icon: <FlagIcon />, label: "Resign", onClick: handleResign, disabled: () => !gameActive() },
    draw: { icon: <HandshakeIcon />, label: "Draw", onClick: handleOfferDraw, disabled: () => !gameActive() },
  };

  const activeButtons = () => modeButtons[mode()] || modeButtons.hotseat;

  const hasSecondPanel = () => mode() === "online" || mode() === "ai";

  return (
    <div class="main-layout">
      {/* Mobile: GameStatus above board */}
      <div class="mobile-only mobile-status">
        <GameStatus />
      </div>

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

      {/* Desktop: right column with controls */}
      <div class="right-column desktop-only">
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

      {/* Mobile: bottom tab bar */}
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
        <button class="mobile-toolbar-item" onClick={() => setMovesSheetOpen(true)}>
          <ListIcon />
          <span>Moves</span>
        </button>
        {hasSecondPanel() && (
          <button class="mobile-toolbar-item" onClick={() => setChatSheetOpen(true)}>
            <ChatIcon />
            <span>{mode() === "online" ? "Chat" : "AI"}</span>
          </button>
        )}
      </nav>

      {/* Mobile bottom sheets */}
      <BottomSheet open={movesSheetOpen()} onOpenChange={setMovesSheetOpen} title="Moves">
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
      </BottomSheet>

      <BottomSheet open={chatSheetOpen()} onOpenChange={setChatSheetOpen} title={mode() === "online" ? "Chat" : "AI Info"}>
        <Switch>
          <Match when={mode() === "online"}>
            <Chat />
          </Match>
          <Match when={mode() === "ai"}>
            <AiInfoPanel />
          </Match>
        </Switch>
      </BottomSheet>
    </div>
  );
}
