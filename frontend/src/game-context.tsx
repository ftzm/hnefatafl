import {
  type Accessor,
  createContext,
  createMemo,
  createSignal,
  type ParentComponent,
  useContext,
} from "solid-js";
import { createStore } from "solid-js/store";
import {
  applyMoveToBoardRep,
  type BoardRep,
  cloneBoardRep,
  computeBoardAtMove,
  type GameOverState,
  type Move,
  type MovesMap,
  type PlayerColor,
  startBoard,
} from "./board-logic";
import { AsyncLock } from "./utils/async-lock";

export type GameMode = "hotseat" | "ai" | "online";

export interface GameState {
  boardRep: BoardRep;
  moveHistory: Move[];
  currentPlayer: PlayerColor;
  moves: MovesMap;
  gameOver: GameOverState | null;
  playerColor: PlayerColor | null;
  players: { black: string; white: string };
  historyCursor: number;
  loading: boolean;
}

export interface PendingAnimation {
  from: number;
  to: number;
  captures?: number[];
  restores?: number[];
  applyState?: () => void;
}

interface GameContextValue {
  store: { game: GameState };
  capturedPieces: Accessor<{ black: number; white: number }>;
  pendingAnimation: Accessor<PendingAnimation | null>;
  setPendingAnimation: (anim: PendingAnimation | null) => void;
  canViewPrev: Accessor<boolean>;
  canViewNext: Accessor<boolean>;
  movesDisabled: Accessor<boolean>;
  currentViewMoveIndex: Accessor<number>;
  lastMove: Accessor<Move | null>;
  applyMove: (move: Move) => void;
  applyExternalMove: (event: {
    move: Move;
    boardRep: BoardRep;
    currentPlayer: PlayerColor;
    moves: MovesMap;
  }) => void;
  setMoves: (moves: MovesMap) => void;
  setGameOver: (state: GameOverState | null) => void;
  reconcile: (response: {
    moves: MovesMap;
    gameOver: GameOverState | null;
  }) => void;
  undoLastMove: () => void;
  initGame: (config: Partial<GameState>) => void;
  viewPrev: () => Promise<void>;
  viewNext: () => Promise<void>;
  viewStart: () => Promise<void>;
  viewEnd: () => Promise<void>;
  jumpToMove: (moveIndex: number) => Promise<void>;
}

const GameContext = createContext<GameContextValue>();

const emptyBoard: BoardRep = {
  black: new Set(),
  white: new Set(),
  king: -1,
};

function initialGameState(): GameState {
  return {
    boardRep: emptyBoard,
    moveHistory: [],
    currentPlayer: "black",
    moves: {},
    gameOver: null,
    playerColor: null,
    players: { black: "Black", white: "White" },
    historyCursor: 0,
    loading: true,
  };
}

function boardAtCursor(moveHistory: Move[], cursor: number): BoardRep {
  if (moveHistory.length === 0 || cursor >= moveHistory.length) {
    return cloneBoardRep(startBoard);
  }
  const moveIndex = moveHistory.length - cursor - 1;
  return computeBoardAtMove(moveHistory, moveIndex);
}

function computeCapturesAtCursor(
  moveHistory: Move[],
  cursor: number,
): { black: number; white: number } {
  const captures = { black: 0, white: 0 };
  let board = cloneBoardRep(startBoard);
  const movesToReplay = moveHistory.length - cursor;
  for (let i = 0; i < movesToReplay; i++) {
    const move = moveHistory[i];
    if (move.captures) {
      for (const cap of move.captures) {
        if (board.black.has(cap)) captures.black++;
        else if (board.white.has(cap) || board.king === cap) captures.white++;
      }
    }
    board = applyMoveToBoardRep(board, move);
  }
  return captures;
}

export const GameProvider: ParentComponent = (props) => {
  const [store, setStore] = createStore<{ game: GameState }>({
    game: initialGameState(),
  });

  const navigationLock = new AsyncLock();

  const [pendingAnimation, setPendingAnimation] =
    createSignal<PendingAnimation | null>(null);

  const canViewPrev = createMemo(
    () => store.game.historyCursor < store.game.moveHistory.length,
  );
  const canViewNext = createMemo(() => store.game.historyCursor > 0);
  const movesDisabled = createMemo(() => store.game.historyCursor !== 0);
  const currentViewMoveIndex = createMemo(() => {
    if (store.game.moveHistory.length === 0) return -1;
    return store.game.moveHistory.length - store.game.historyCursor - 1;
  });
  const lastMove = createMemo(() => {
    const viewIdx = currentViewMoveIndex();
    if (viewIdx >= 0 && viewIdx < store.game.moveHistory.length) {
      return store.game.moveHistory[viewIdx];
    }
    return null;
  });
  const capturedPieces = createMemo(() =>
    computeCapturesAtCursor(
      store.game.moveHistory,
      store.game.historyCursor,
    ),
  );

  function applyMove(move: Move): void {
    const newHistory = [...store.game.moveHistory, move];
    const newBoard = applyMoveToBoardRep(store.game.boardRep, move);
    const nextPlayer: PlayerColor =
      store.game.currentPlayer === "black" ? "white" : "black";
    setStore("game", {
      moveHistory: newHistory,
      historyCursor: 0,
      currentPlayer: nextPlayer,
      boardRep: newBoard,
      moves: {},
      gameOver: null,
    });
  }

  function applyExternalMove(event: {
    move: Move;
    boardRep: BoardRep;
    currentPlayer: PlayerColor;
    moves: MovesMap;
  }): void {
    const move = event.move;
    setStore("game", {
      moveHistory: [...store.game.moveHistory, move],
      historyCursor: 0,
      currentPlayer: event.currentPlayer,
      boardRep: event.boardRep,
      moves: event.moves,
    });
    setPendingAnimation({
      from: move.from,
      to: move.to,
      captures: move.captures || [],
    });
  }

  function setMovesAction(moves: MovesMap): void {
    setStore("game", "moves", moves);
  }

  function setGameOverAction(state: GameOverState | null): void {
    setStore("game", "gameOver", state);
  }

  function reconcileAction(response: {
    moves: MovesMap;
    gameOver: GameOverState | null;
  }): void {
    setStore("game", {
      moves: response.moves,
      gameOver: response.gameOver,
    });
  }

  function undoLastMove(): void {
    if (store.game.moveHistory.length === 0) return;
    const newHistory = store.game.moveHistory.slice(0, -1);
    const newBoard =
      newHistory.length > 0
        ? boardAtCursor(newHistory, 0)
        : cloneBoardRep(startBoard);
    const prevPlayer: PlayerColor =
      store.game.currentPlayer === "black" ? "white" : "black";
    setStore("game", {
      moveHistory: newHistory,
      historyCursor: 0,
      currentPlayer: prevPlayer,
      boardRep: newBoard,
      moves: {},
    });
  }

  function initGame(config: Partial<GameState>): void {
    setStore("game", { ...initialGameState(), ...config, loading: false });
  }

  async function viewPrev(): Promise<void> {
    return navigationLock.withLock(async () => {
      if (store.game.historyCursor >= store.game.moveHistory.length) return;
      const newCursor = store.game.historyCursor + 1;
      const moveIndex =
        store.game.moveHistory.length - store.game.historyCursor - 1;
      const move = store.game.moveHistory[moveIndex];
      const newBoard = boardAtCursor(store.game.moveHistory, newCursor);
      setStore("game", {
        historyCursor: newCursor,
        boardRep: newBoard,
      });
      setPendingAnimation({
        from: move.to,
        to: move.from,
        restores: move.captures || [],
      });
    });
  }

  async function viewNext(): Promise<void> {
    return navigationLock.withLock(async () => {
      if (store.game.historyCursor <= 0) return;
      const newCursor = store.game.historyCursor - 1;
      const moveIndex = store.game.moveHistory.length - newCursor - 1;
      const move = store.game.moveHistory[moveIndex];
      const newBoard = boardAtCursor(store.game.moveHistory, newCursor);
      setStore("game", {
        historyCursor: newCursor,
        boardRep: newBoard,
      });
      setPendingAnimation({
        from: move.from,
        to: move.to,
        captures: move.captures || [],
      });
    });
  }

  async function viewStart(): Promise<void> {
    return navigationLock.withLock(async () => {
      if (store.game.historyCursor >= store.game.moveHistory.length) return;
      setStore("game", {
        historyCursor: store.game.moveHistory.length,
        boardRep: cloneBoardRep(startBoard),
      });
    });
  }

  async function viewEnd(): Promise<void> {
    return navigationLock.withLock(async () => {
      if (store.game.historyCursor <= 0) return;
      const newBoard = boardAtCursor(store.game.moveHistory, 0);
      setStore("game", {
        historyCursor: 0,
        boardRep: newBoard,
      });
    });
  }

  async function jumpToMove(moveIndex: number): Promise<void> {
    return navigationLock.withLock(async () => {
      const newCursor = store.game.moveHistory.length - moveIndex - 1;
      if (newCursor === store.game.historyCursor) return;
      const newBoard = boardAtCursor(store.game.moveHistory, newCursor);
      setStore("game", {
        historyCursor: newCursor,
        boardRep: newBoard,
      });
    });
  }

  const value: GameContextValue = {
    store,
    capturedPieces,
    pendingAnimation,
    setPendingAnimation,
    canViewPrev,
    canViewNext,
    movesDisabled,
    currentViewMoveIndex,
    lastMove,
    applyMove,
    applyExternalMove,
    setMoves: setMovesAction,
    setGameOver: setGameOverAction,
    reconcile: reconcileAction,
    undoLastMove,
    initGame,
    viewPrev,
    viewNext,
    viewStart,
    viewEnd,
    jumpToMove,
  };

  return (
    <GameContext.Provider value={value}>{props.children}</GameContext.Provider>
  );
};

export function useGame(): GameContextValue {
  const ctx = useContext(GameContext);
  if (!ctx) throw new Error("useGame must be used within GameProvider");
  return ctx;
}
