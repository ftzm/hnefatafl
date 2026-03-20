import { createMemo, createSignal, type Accessor } from "solid-js";
import { createStore } from "solid-js/store";
import {
  startBoard,
  generateMockMovesForColor,
  cloneBoardRep,
  applyMoveToBoardRep,
  computeBoardAtMove,
  type BoardRep,
  type Move,
  type MovesMap,
  type PlayerColor,
} from "./board-logic";
import { AsyncLock } from "./async-lock";

export type GameMode = "hotseat" | "ai" | "online";

export interface GameOverState {
  winner: PlayerColor | "draw";
  reason: string;
}

export interface GameState {
  id: string | null;
  mode: GameMode | null;
  players: { black: string; white: string };
  moveHistory: Move[];
  historyCursor: number;
  currentPlayer: PlayerColor;
  boardRep: BoardRep;
  moves: MovesMap;
  capturedPieces: { black: number; white: number };
  gameOver: GameOverState | null;
}

export interface PendingAnimation {
  from: number;
  to: number;
  captures?: number[];
  restores?: number[];
  applyState?: () => void;
}

const initialGameState = (): GameState => ({
  id: null,
  mode: null,
  players: { black: "Black", white: "White" },
  moveHistory: [],
  historyCursor: 0,
  currentPlayer: "black",
  boardRep: cloneBoardRep(startBoard),
  moves: generateMockMovesForColor(startBoard, "black"),
  capturedPieces: { black: 0, white: 0 },
  gameOver: null,
});

const [store, setStore] = createStore<{ game: GameState }>({
  game: initialGameState(),
});

const navigationLock = new AsyncLock();

const [pendingAnimation, setPendingAnimation] = createSignal<PendingAnimation | null>(null);
export { pendingAnimation, setPendingAnimation };

export const canViewPrev: Accessor<boolean> = createMemo(
  () => store.game.historyCursor < store.game.moveHistory.length
);
export const canViewNext: Accessor<boolean> = createMemo(() => store.game.historyCursor > 0);
export const movesDisabled: Accessor<boolean> = createMemo(() => store.game.historyCursor !== 0);
export const currentViewMoveIndex: Accessor<number> = createMemo(() => {
  if (store.game.moveHistory.length === 0) return -1;
  return store.game.moveHistory.length - store.game.historyCursor - 1;
});

export const lastMove: Accessor<Move | null> = createMemo(() => {
  const viewIdx = currentViewMoveIndex();
  if (viewIdx >= 0 && viewIdx < store.game.moveHistory.length) {
    return store.game.moveHistory[viewIdx];
  }
  return null;
});

export { store };

function boardAtCursor(moveHistory: Move[], cursor: number): BoardRep {
  if (moveHistory.length === 0 || cursor >= moveHistory.length) {
    return cloneBoardRep(startBoard);
  }
  const moveIndex = moveHistory.length - cursor - 1;
  return computeBoardAtMove(moveHistory, moveIndex);
}

function computeCapturesAtCursor(moveHistory: Move[], cursor: number): { black: number; white: number } {
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

export function handleMoveMade(move: Move): void {
  const newHistory = [...store.game.moveHistory, move];
  const newBoard = applyMoveToBoardRep(store.game.boardRep, move);

  const newCaptured = { ...store.game.capturedPieces };
  if (move.captures) {
    for (const cap of move.captures) {
      if (store.game.boardRep.black.has(cap)) newCaptured.black++;
      else if (store.game.boardRep.white.has(cap) || store.game.boardRep.king === cap)
        newCaptured.white++;
    }
  }

  const nextPlayer: PlayerColor = store.game.currentPlayer === "black" ? "white" : "black";
  const nextMoves = generateMockMovesForColor(newBoard, nextPlayer);

  setStore("game", {
    moveHistory: newHistory,
    historyCursor: 0,
    currentPlayer: nextPlayer,
    boardRep: newBoard,
    moves: nextMoves,
    capturedPieces: newCaptured,
  });
}

export async function handleViewPreviousMove(): Promise<void> {
  return navigationLock.withLock(async () => {
    if (store.game.historyCursor >= store.game.moveHistory.length) return;

    const newCursor = store.game.historyCursor + 1;
    const moveIndex = store.game.moveHistory.length - store.game.historyCursor - 1;
    const move = store.game.moveHistory[moveIndex];
    const newBoard = boardAtCursor(store.game.moveHistory, newCursor);
    const newCaptured = computeCapturesAtCursor(store.game.moveHistory, newCursor);

    setStore("game", {
      historyCursor: newCursor,
      boardRep: newBoard,
      capturedPieces: newCaptured,
    });
    setPendingAnimation({
      from: move.to,
      to: move.from,
      restores: move.captures || [],
    });
  });
}

export async function handleViewNextMove(): Promise<void> {
  return navigationLock.withLock(async () => {
    if (store.game.historyCursor <= 0) return;

    const newCursor = store.game.historyCursor - 1;
    const moveIndex = store.game.moveHistory.length - newCursor - 1;
    const move = store.game.moveHistory[moveIndex];
    const newBoard = boardAtCursor(store.game.moveHistory, newCursor);
    const newCaptured = computeCapturesAtCursor(store.game.moveHistory, newCursor);

    setPendingAnimation({
      from: move.from,
      to: move.to,
      captures: move.captures || [],
      applyState: () => setStore("game", {
        historyCursor: newCursor,
        boardRep: newBoard,
        capturedPieces: newCaptured,
      }),
    });
  });
}

export async function handleJumpToMove(moveIndex: number): Promise<void> {
  return navigationLock.withLock(async () => {
    const newCursor = store.game.moveHistory.length - moveIndex - 1;
    if (newCursor === store.game.historyCursor) return;

    const newBoard = boardAtCursor(store.game.moveHistory, newCursor);
    const newCaptured = computeCapturesAtCursor(store.game.moveHistory, newCursor);

    setStore("game", {
      historyCursor: newCursor,
      boardRep: newBoard,
      capturedPieces: newCaptured,
    });
  });
}

export async function handleViewStart(): Promise<void> {
  return navigationLock.withLock(async () => {
    if (store.game.historyCursor >= store.game.moveHistory.length) return;

    setStore("game", {
      historyCursor: store.game.moveHistory.length,
      boardRep: cloneBoardRep(startBoard),
      capturedPieces: { black: 0, white: 0 },
    });
  });
}

export async function handleViewEnd(): Promise<void> {
  return navigationLock.withLock(async () => {
    if (store.game.historyCursor <= 0) return;

    const newBoard = boardAtCursor(store.game.moveHistory, 0);
    const newCaptured = computeCapturesAtCursor(store.game.moveHistory, 0);

    setStore("game", {
      historyCursor: 0,
      boardRep: newBoard,
      capturedPieces: newCaptured,
    });
  });
}

export function handleResign(): void {
  if (store.game.gameOver) return;
  const winner: PlayerColor = store.game.currentPlayer === "black" ? "white" : "black";
  setStore("game", "gameOver", { winner, reason: "Resignation" });
}

export function handleOfferDraw(): void {
  if (store.game.gameOver) return;
  setStore("game", "gameOver", { winner: "draw", reason: "Draw agreed" });
}

export function handleNewGame(config: Partial<GameState> = {}): void {
  setStore("game", {
    ...initialGameState(),
    ...config,
  });
}

export function handleUndo(): void {
  if (store.game.gameOver) return;
  if (store.game.moveHistory.length === 0) return;

  const newHistory = store.game.moveHistory.slice(0, -1);
  const newBoard = newHistory.length > 0
    ? boardAtCursor(newHistory, 0)
    : cloneBoardRep(startBoard);
  const newCaptured = computeCapturesAtCursor(newHistory, 0);
  const prevPlayer: PlayerColor = store.game.currentPlayer === "black" ? "white" : "black";
  const newMoves = generateMockMovesForColor(newBoard, prevPlayer);

  setStore("game", {
    moveHistory: newHistory,
    historyCursor: 0,
    currentPlayer: prevPlayer,
    boardRep: newBoard,
    moves: newMoves,
    capturedPieces: newCaptured,
  });
}
