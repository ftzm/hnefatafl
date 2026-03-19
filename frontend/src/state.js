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

// Core game state
const [store, setStore] = createStore({
  moveHistory: [],
  historyCursor: 0,
  currentPlayer: "black",
  boardRep: cloneBoardRep(startBoard),
  moves: generateMockMovesForColor(startBoard, "black"),
  capturedPieces: { black: 0, white: 0 },
});

// Navigation lock
const navigationLock = new AsyncLock();

// Animation signal for history navigation
const [pendingAnimation, setPendingAnimation] = createSignal(null);
export { pendingAnimation, setPendingAnimation };

// Derived signals
export const canViewPrev = createMemo(
  () => store.historyCursor < store.moveHistory.length
);
export const canViewNext = createMemo(() => store.historyCursor > 0);
export const movesDisabled = createMemo(() => store.historyCursor !== 0);
export const currentViewMoveIndex = createMemo(() => {
  if (store.moveHistory.length === 0) return -1;
  return store.moveHistory.length - store.historyCursor - 1;
});

export const lastMove = createMemo(() => {
  const viewIdx = currentViewMoveIndex();
  if (viewIdx >= 0 && viewIdx < store.moveHistory.length) {
    return store.moveHistory[viewIdx];
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
  const newHistory = [...store.moveHistory, move];
  const newBoard = applyMoveToBoardRep(store.boardRep, move);

  // Count captured pieces
  const newCaptured = { ...store.capturedPieces };
  if (move.captures) {
    for (const cap of move.captures) {
      if (store.boardRep.black.has(cap)) newCaptured.black++;
      else if (store.boardRep.white.has(cap) || store.boardRep.king === cap)
        newCaptured.white++;
    }
  }

  const nextPlayer = store.currentPlayer === "black" ? "white" : "black";
  const nextMoves = generateMockMovesForColor(newBoard, nextPlayer);

  setStore({
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
    if (store.historyCursor >= store.moveHistory.length) return;

    const newCursor = store.historyCursor + 1;
    const moveIndex = store.moveHistory.length - store.historyCursor - 1;
    const move = store.moveHistory[moveIndex];
    const newBoard = boardAtCursor(store.moveHistory, newCursor);
    const newCaptured = computeCapturesAtCursor(store.moveHistory, newCursor);

    setStore({
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
    if (store.historyCursor <= 0) return;

    const newCursor = store.historyCursor - 1;
    const moveIndex = store.moveHistory.length - newCursor - 1;
    const move = store.moveHistory[moveIndex];
    const newBoard = boardAtCursor(store.moveHistory, newCursor);
    const newCaptured = computeCapturesAtCursor(store.moveHistory, newCursor);

    setPendingAnimation({
      from: move.from,
      to: move.to,
      captures: move.captures || [],
      applyState: () => setStore({
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
    const newCursor = store.moveHistory.length - moveIndex - 1;
    if (newCursor === store.historyCursor) return;

    const newBoard = boardAtCursor(store.moveHistory, newCursor);
    const newCaptured = computeCapturesAtCursor(store.moveHistory, newCursor);

    setStore({
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
    if (store.historyCursor >= store.moveHistory.length) return;

    setStore({
      historyCursor: store.moveHistory.length,
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
    if (store.historyCursor <= 0) return;

    const newBoard = boardAtCursor(store.moveHistory, 0);
    const newCaptured = computeCapturesAtCursor(store.moveHistory, 0);

    setStore({
      historyCursor: 0,
      boardRep: newBoard,
      capturedPieces: newCaptured,
    });
  });
}
