import { createSignal } from "solid-js";
import {
  applyMoveToBoardRep,
  type BoardRep,
  cloneBoardRep,
  type Move,
  type PlayerColor,
  startBoard,
} from "../board-logic";
import type { AiGameService } from "../api/ai-game-service";
import {
  checkGameOver,
  generateLegalMoves,
  pickRandomMove,
} from "./mock-game-logic";
import type { AiGameEvent } from "../api/types";

export function createMockAiGameService(): AiGameService {
  let gameConfig: { playerColor: PlayerColor } | null = null;
  let board: BoardRep = cloneBoardRep(startBoard);
  let currentPlayer: PlayerColor = "black";
  let moveHistory: Move[] = [];
  let token: string | null = null;
  let active = false;

  const [events, setEvents] = createSignal<AiGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);

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

    resign() {
      if (!active || !gameConfig) return;
      const winner = gameConfig.playerColor === "black" ? "white" : "black";
      setEvents({ type: "gameOver", winner, reason: "Resignation" });
    },

    events,
    connected,
  };
}
