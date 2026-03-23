import { createSignal } from "solid-js";
import {
  applyMoveToBoardRep,
  type BoardRep,
  cloneBoardRep,
  type Move,
  type PlayerColor,
  startBoard,
} from "../board-logic";
import {
  checkGameOver,
  generateLegalMoves,
  pickRandomMove,
} from "./mock-game-logic";
import type { OnlineGameService } from "../api/online-game-service";
import type { OnlineGameEvent } from "../api/types";

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

  const [events, setEvents] = createSignal<OnlineGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);

  function emitIfActive(event: OnlineGameEvent) {
    if (active) setEvents(event);
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
      });
      return;
    }

    const move = pickRandomMove(moves);
    if (!move) return;

    board = applyMoveToBoardRep(board, move);
    moveHistory.push(move);
    currentPlayer = currentPlayer === "black" ? "white" : "black";

    const nextMoves = generateLegalMoves(board, currentPlayer);
    emitIfActive({
      type: "moveMade",
      move,
      boardRep: cloneBoardRep(board),
      currentPlayer,
      moves: nextMoves,
    });

    const nextGameOver = checkGameOver(board, currentPlayer, nextMoves);
    if (nextGameOver) {
      setTimeout(() => {
        emitIfActive({
          type: "gameOver",
          winner: nextGameOver.winner,
          reason: nextGameOver.reason,
        });
      }, 100);
    }
  }

  function resolvePlayerColor(token: string): PlayerColor {
    if (token === playerToken) return creatorColor;
    return creatorColor === "black" ? "white" : "black";
  }

  return {
    createGame(opts) {
      creatorColor = opts.creatorColor;
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
      board = applyMoveToBoardRep(board, move);
      moveHistory.push(move);
      currentPlayer = currentPlayer === "black" ? "white" : "black";

      const nextMoves = generateLegalMoves(board, currentPlayer);
      setEvents({
        type: "moveMade",
        move,
        boardRep: cloneBoardRep(board),
        currentPlayer,
        moves: nextMoves,
      });

      const gameOver = checkGameOver(board, currentPlayer, nextMoves);
      if (gameOver) {
        setTimeout(() => {
          emitIfActive({
            type: "gameOver",
            winner: gameOver.winner,
            reason: gameOver.reason,
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
      setEvents({ type: "gameOver", winner, reason: "Resignation" });
    },

    offerDraw() {
      if (!active) return;
      setTimeout(() => {
        if (Math.random() < 0.5) {
          emitIfActive({
            type: "gameOver",
            winner: "draw",
            reason: "Draw agreed",
          });
        } else {
          emitIfActive({ type: "drawDeclined" });
        }
      }, 300);
    },

    acceptDraw() {
      if (!active) return;
      setEvents({ type: "gameOver", winner: "draw", reason: "Draw agreed" });
    },

    declineDraw() {
      // No-op in mock — the mock auto-responds to draw offers
    },

    requestUndo() {
      if (!active) return;
      setTimeout(() => {
        if (Math.random() < 0.5) {
          emitIfActive({ type: "undoAccepted", moveCount: 1 });
        } else {
          emitIfActive({ type: "undoDeclined" });
        }
      }, 300);
    },

    acceptUndo() {
      if (!active || moveHistory.length === 0) return;
      moveHistory.pop();
      board = cloneBoardRep(startBoard);
      currentPlayer = "black";
      for (const m of moveHistory) {
        board = applyMoveToBoardRep(board, m);
        currentPlayer = currentPlayer === "black" ? "white" : "black";
      }
      setEvents({ type: "undoAccepted", moveCount: 1 });
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

    events,
    connected,
  };
}
