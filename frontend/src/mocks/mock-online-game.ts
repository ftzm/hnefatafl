import { createSignal } from "solid-js";
import type { OnlineGameService } from "../api/online-game-service";
import type { ClockMs, OnlineGameEvent } from "../api/types";
import {
  applyMoveToBoardRep,
  type BoardRep,
  cloneBoardRep,
  type Move,
  type PlayerColor,
  startBoard,
} from "../board-logic";
import type { TimeControlValue } from "../gameOptions";
import {
  checkGameOver,
  generateLegalMoves,
  pickRandomMove,
} from "./mock-game-logic";

const CHAT_RESPONSES = [
  "Good move!",
  "Interesting...",
  "I didn't see that coming.",
  "Hmm, let me think.",
  "Nice one!",
];

export function createMockOnlineGameService(): OnlineGameService {
  let creatorColor: PlayerColor = "black";
  let board: BoardRep = cloneBoardRep(startBoard);
  let currentPlayer: PlayerColor = "black";
  let moveHistory: Move[] = [];
  let playerToken: string | null = null;
  let inviteToken: string | null = null;
  let active = false;

  // Clock simulation. When timeControl is null the game is untimed and
  // every event carries a null clock. Otherwise both banks start at the
  // initial time and the side to move spends from turnStartedAtMs.
  let timeControl: TimeControlValue | null = null;
  let whiteMs = 0;
  let blackMs = 0;
  let turnStartedAtMs = 0;

  const [events, setEvents] = createSignal<OnlineGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);
  const [connecting] = createSignal(false);

  function emitIfActive(event: OnlineGameEvent) {
    if (active) setEvents(event);
  }

  function clockSnapshot(): ClockMs | null {
    if (!timeControl) return null;
    return { whiteMs, blackMs, turnStartedAtMs };
  }

  // Update the clock for a completed move by `mover`. The opening move
  // is free and only starts the opponent's clock; later moves deduct the
  // elapsed turn time and add the increment.
  function advanceClock(mover: PlayerColor, firstMove: boolean) {
    if (!timeControl) return;
    const now = Date.now();
    if (firstMove) {
      turnStartedAtMs = now;
      return;
    }
    const elapsed = now - turnStartedAtMs;
    const incMs = timeControl.increment * 1000;
    if (mover === "white") {
      whiteMs = Math.max(0, whiteMs - elapsed) + incMs;
    } else {
      blackMs = Math.max(0, blackMs - elapsed) + incMs;
    }
    turnStartedAtMs = now;
  }

  function performUndo() {
    if (moveHistory.length === 0) return;
    moveHistory.pop();
    board = cloneBoardRep(startBoard);
    currentPlayer = "black";
    for (const m of moveHistory) {
      board = applyMoveToBoardRep(board, m);
      currentPlayer = currentPlayer === "black" ? "white" : "black";
    }
    const moves = generateLegalMoves(board, currentPlayer);
    emitIfActive({
      type: "undoAccepted",
      moveCount: 1,
      boardRep: cloneBoardRep(board),
      currentPlayer,
      moves,
      clock: clockSnapshot(),
    });
  }

  function opponentTurn() {
    if (!active) return;
    const moves = generateLegalMoves(board, currentPlayer);
    const gameOver = checkGameOver(board, currentPlayer, moves);
    if (gameOver) {
      emitIfActive({
        type: "gameOver",
        winner: gameOver.winner,
        reason: gameOver.reason,
        clock: clockSnapshot(),
      });
      return;
    }

    const move = pickRandomMove(moves);
    if (!move) return;

    const mover = currentPlayer;
    const firstMove = moveHistory.length === 0;
    board = applyMoveToBoardRep(board, move);
    moveHistory.push(move);
    currentPlayer = currentPlayer === "black" ? "white" : "black";
    advanceClock(mover, firstMove);

    const nextMoves = generateLegalMoves(board, currentPlayer);
    emitIfActive({
      type: "moveMade",
      move,
      boardRep: cloneBoardRep(board),
      currentPlayer,
      moves: nextMoves,
      clock: clockSnapshot(),
    });

    const nextGameOver = checkGameOver(board, currentPlayer, nextMoves);
    if (nextGameOver) {
      setTimeout(() => {
        emitIfActive({
          type: "gameOver",
          winner: nextGameOver.winner,
          reason: nextGameOver.reason,
          clock: clockSnapshot(),
        });
      }, 100);
    }
  }

  function resolvePlayerColor(token: string): PlayerColor {
    if (token === playerToken) return creatorColor;
    return creatorColor === "black" ? "white" : "black";
  }

  const service: OnlineGameService = {
    createGame(opts) {
      creatorColor = opts.creatorColor;
      timeControl = opts.timeControl;
      playerToken = crypto.randomUUID();
      inviteToken = crypto.randomUUID();
      return Promise.resolve({
        playerToken,
        inviteToken,
      });
    },

    connect(token) {
      if (token !== playerToken && token !== inviteToken) return;
      board = cloneBoardRep(startBoard);
      currentPlayer = "black";
      moveHistory = [];
      active = true;
      setConnected(true);
      if (timeControl) {
        whiteMs = timeControl.initialTime * 1000;
        blackMs = timeControl.initialTime * 1000;
        turnStartedAtMs = Date.now();
      }

      const playerColor = resolvePlayerColor(token);
      const initialMoves = generateLegalMoves(board, currentPlayer);

      setEvents({
        type: "initialState",
        playerColor,
        boardRep: cloneBoardRep(board),
        currentPlayer,
        moves: initialMoves,
        moveHistory: [],
        gameOver: null,
        clock: clockSnapshot(),
      });

      setTimeout(() => emitIfActive({ type: "opponentJoined" }), 300);

      // If opponent (simulated) goes first
      const opponentColor = playerColor === "black" ? "white" : "black";
      if (currentPlayer === opponentColor) {
        setTimeout(() => opponentTurn(), 500);
      }
    },

    disconnect() {
      active = false;
      setConnected(false);
    },

    sendMove(move) {
      if (!active) return;
      const mover = currentPlayer;
      const firstMove = moveHistory.length === 0;
      board = applyMoveToBoardRep(board, move);
      moveHistory.push(move);
      currentPlayer = currentPlayer === "black" ? "white" : "black";
      advanceClock(mover, firstMove);

      const nextMoves = generateLegalMoves(board, currentPlayer);
      setEvents({
        type: "moveMade",
        move,
        boardRep: cloneBoardRep(board),
        currentPlayer,
        moves: nextMoves,
        clock: clockSnapshot(),
      });

      const gameOver = checkGameOver(board, currentPlayer, nextMoves);
      if (gameOver) {
        setTimeout(() => {
          emitIfActive({
            type: "gameOver",
            winner: gameOver.winner,
            reason: gameOver.reason,
            clock: clockSnapshot(),
          });
        }, 100);
        return;
      }

      setTimeout(() => opponentTurn(), 500);
    },

    resign() {
      if (!active) return;
      // The resigning player loses
      const winner = creatorColor === "black" ? "white" : "black";
      setEvents({
        type: "gameOver",
        winner,
        reason: "Resignation",
        clock: clockSnapshot(),
      });
    },

    offerDraw() {
      if (!active) return;
      setTimeout(() => {
        if (Math.random() < 0.5) {
          emitIfActive({
            type: "gameOver",
            winner: "draw",
            reason: "Draw agreed",
            clock: clockSnapshot(),
          });
        } else {
          emitIfActive({ type: "drawDeclined" });
        }
      }, 300);
    },

    acceptDraw() {
      if (!active) return;
      setEvents({
        type: "gameOver",
        winner: "draw",
        reason: "Draw agreed",
        clock: clockSnapshot(),
      });
    },

    declineDraw() {
      // No-op in mock — the mock auto-responds to draw offers
    },

    requestUndo() {
      if (!active) return;
      setTimeout(() => {
        if (Math.random() < 0.5) {
          performUndo();
        } else {
          emitIfActive({ type: "undoDeclined" });
        }
      }, 300);
    },

    acceptUndo() {
      if (!active || moveHistory.length === 0) return;
      performUndo();
    },

    declineUndo() {
      if (!active) return;
      setEvents({ type: "undoDeclined" });
    },

    sendChat(message) {
      if (!active) return;
      void message;
      setTimeout(() => {
        const response =
          CHAT_RESPONSES[Math.floor(Math.random() * CHAT_RESPONSES.length)];
        emitIfActive({ type: "chat", message: response });
      }, 500);
    },

    simulateTimeout(delayMs = 2000) {
      if (!active) return;
      const loser = currentPlayer;
      const winner = loser === "black" ? "white" : "black";
      setTimeout(() => {
        emitIfActive({
          type: "gameOver",
          winner,
          reason: "timeout",
          clock: clockSnapshot(),
        });
      }, delayMs);
    },

    events,
    connected,
    connecting,
  };

  // Expose for Playwright and manual browser console use.
  window.__simulateTimeout = (delayMs?: number) =>
    service.simulateTimeout?.(delayMs);

  return service;
}
