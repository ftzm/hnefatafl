import { createMemo, createSignal } from "solid-js";
import { createStore } from "solid-js/store";
import {
  startBoard,
  generateMockMovesForColor,
  cloneBoardRep,
  applyMoveToBoardRep,
  computeBoardAtMove,
} from "./board-logic.js";
import { AsyncLock } from "./async-lock.js";

const initialGameState = () => ({
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

const [store, setStore] = createStore({
  game: initialGameState(),
});

// Navigation lock
const navigationLock = new AsyncLock();

// Animation signal for history navigation
const [pendingAnimation, setPendingAnimation] = createSignal(null);
export { pendingAnimation, setPendingAnimation };

// Derived signals
export const canViewPrev = createMemo(
  () => store.game.historyCursor < store.game.moveHistory.length
);
export const canViewNext = createMemo(() => store.game.historyCursor > 0);
export const movesDisabled = createMemo(() => store.game.historyCursor !== 0);
export const currentViewMoveIndex = createMemo(() => {
  if (store.game.moveHistory.length === 0) return -1;
  return store.game.moveHistory.length - store.game.historyCursor - 1;
});

export const lastMove = createMemo(() => {
  const viewIdx = currentViewMoveIndex();
  if (viewIdx >= 0 && viewIdx < store.game.moveHistory.length) {
    return store.game.moveHistory[viewIdx];
  }
  return null;
});

export { store };

// Helper: compute board state at a given cursor position
function boardAtCursor(moveHistory, cursor) {
  if (moveHistory.length === 0 || cursor >= moveHistory.length) {
    return cloneBoardRep(startBoard);
  }
  const moveIndex = moveHistory.length - cursor - 1;
  return computeBoardAtMove(moveHistory, moveIndex);
}

// Helper: compute capture counts by replaying moves up to cursor
function computeCapturesAtCursor(moveHistory, cursor) {
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

/**
 * Handle a move being made by the player
 * @param {Object} move - { from, to, captures }
 */
export function handleMoveMade(move) {
  const newHistory = [...store.game.moveHistory, move];
  const newBoard = applyMoveToBoardRep(store.game.boardRep, move);

  // Count captured pieces
  const newCaptured = { ...store.game.capturedPieces };
  if (move.captures) {
    for (const cap of move.captures) {
      if (store.game.boardRep.black.has(cap)) newCaptured.black++;
      else if (store.game.boardRep.white.has(cap) || store.game.boardRep.king === cap)
        newCaptured.white++;
    }
  }

  const nextPlayer = store.game.currentPlayer === "black" ? "white" : "black";
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

/**
 * Navigate to previous move
 */
export async function handleViewPreviousMove() {
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

/**
 * Navigate to next move
 */
export async function handleViewNextMove() {
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

/**
 * Jump to a specific move by its index in moveHistory
 */
export async function handleJumpToMove(moveIndex) {
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

/**
 * Jump to start of game
 */
export async function handleViewStart() {
  return navigationLock.withLock(async () => {
    if (store.game.historyCursor >= store.game.moveHistory.length) return;

    setStore("game", {
      historyCursor: store.game.moveHistory.length,
      boardRep: cloneBoardRep(startBoard),
      capturedPieces: { black: 0, white: 0 },
    });
  });
}

/**
 * Jump to end (latest position)
 */
export async function handleViewEnd() {
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

export function handleResign() {
  if (store.game.gameOver) return;
  const winner = store.game.currentPlayer === "black" ? "white" : "black";
  setStore("game", "gameOver", { winner, reason: "Resignation" });
}

export function handleOfferDraw() {
  if (store.game.gameOver) return;
  setStore("game", "gameOver", { winner: "draw", reason: "Draw agreed" });
}

export function handleNewGame(config = {}) {
  setStore("game", {
    ...initialGameState(),
    ...config,
  });
}

export function handleUndo() {
  if (store.game.gameOver) return;
  if (store.game.moveHistory.length === 0) return;

  const newHistory = store.game.moveHistory.slice(0, -1);
  const newBoard = newHistory.length > 0
    ? boardAtCursor(newHistory, 0)
    : cloneBoardRep(startBoard);
  const newCaptured = computeCapturesAtCursor(newHistory, 0);
  const prevPlayer = store.game.currentPlayer === "black" ? "white" : "black";
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
