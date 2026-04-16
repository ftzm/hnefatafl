import { createSignal } from "solid-js";
import type { AiGameService } from "../api/ai-game-service";
import type { AiGameEvent } from "../api/types";
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

export function createMockAiGameService(): AiGameService {
  let gameConfig: { playerColor: PlayerColor } | null = null;
  let board: BoardRep = cloneBoardRep(startBoard);
  let currentPlayer: PlayerColor = "black";
  let moveHistory: Move[] = [];
  let token: string | null = null;
  let active = false;

  const [events, setEvents] = createSignal<AiGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);
  const [connecting] = createSignal(false);

  function emitIfActive(event: AiGameEvent) {
    if (active) setEvents(event);
  }

  function aiTurn() {
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

  return {
    createGame(opts) {
      gameConfig = opts;
      token = crypto.randomUUID();
      return Promise.resolve({ token });
    },

    connect(t) {
      if (t !== token || !gameConfig) return;
      board = cloneBoardRep(startBoard);
      currentPlayer = "black";
      moveHistory = [];
      active = true;
      setConnected(true);

      const initialMoves = generateLegalMoves(board, currentPlayer);
      setEvents({
        type: "initialState",
        playerColor: gameConfig.playerColor,
        boardRep: cloneBoardRep(board),
        currentPlayer,
        moves: initialMoves,
        moveHistory: [],
        gameOver: null,
      });

      // AI goes first when player chose white
      if (gameConfig.playerColor === "white") {
        setTimeout(() => aiTurn(), 500);
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

      setTimeout(() => aiTurn(), 500);
    },

    undo() {
      if (!active || moveHistory.length < 2) return;
      moveHistory = moveHistory.slice(0, -2);
      board = cloneBoardRep(startBoard);
      currentPlayer = "black";
      for (const m of moveHistory) {
        board = applyMoveToBoardRep(board, m);
        currentPlayer = currentPlayer === "black" ? "white" : "black";
      }
      const moves = generateLegalMoves(board, currentPlayer);
      emitIfActive({
        type: "undoAccepted",
        moveCount: 2,
        boardRep: cloneBoardRep(board),
        currentPlayer,
        moves,
        gameOver: checkGameOver(board, currentPlayer, moves),
      });
    },

    resign() {
      if (!active || !gameConfig) return;
      const winner = gameConfig.playerColor === "black" ? "white" : "black";
      setEvents({ type: "gameOver", winner, reason: "Resignation" });
    },

    events,
    connected,
    connecting,
  };
}
