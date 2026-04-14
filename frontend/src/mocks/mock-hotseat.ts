import type { HotseatApi } from "../api/hotseat-api";
import type { HotseatActionResult, HotseatState } from "../api/types";
import {
  applyMoveToBoardRep,
  type BoardRep,
  cloneBoardRep,
  type Move,
  type PlayerColor,
  startBoard,
} from "../board-logic";
import { checkGameOver, generateLegalMoves } from "./mock-game-logic";

interface GameRecord {
  board: BoardRep;
  currentPlayer: PlayerColor;
  moveHistory: Move[];
}

export function createMockHotseatApi(): HotseatApi {
  const games = new Map<string, GameRecord>();

  function getOrFail(gameId: string): GameRecord {
    const game = games.get(gameId);
    if (!game) throw new Error(`Game ${gameId} not found`);
    return game;
  }

  function actionResult(game: GameRecord): HotseatActionResult {
    const moves = generateLegalMoves(game.board, game.currentPlayer);
    const gameOver = checkGameOver(game.board, game.currentPlayer, moves);
    return { moves, gameOver };
  }

  return {
    createGame() {
      const gameId = crypto.randomUUID();
      games.set(gameId, {
        board: cloneBoardRep(startBoard),
        currentPlayer: "black",
        moveHistory: [],
      });
      return Promise.resolve(gameId);
    },

    getState(gameId: string): Promise<HotseatState> {
      const game = getOrFail(gameId);
      const moves = generateLegalMoves(game.board, game.currentPlayer);
      const gameOver = checkGameOver(game.board, game.currentPlayer, moves);
      return Promise.resolve({
        boardRep: cloneBoardRep(game.board),
        currentPlayer: game.currentPlayer,
        moves,
        moveHistory: [...game.moveHistory],
        gameOver,
      });
    },

    sendMove(gameId, move) {
      const game = getOrFail(gameId);
      game.board = applyMoveToBoardRep(game.board, move);
      game.moveHistory.push(move);
      game.currentPlayer = game.currentPlayer === "black" ? "white" : "black";
      return Promise.resolve(actionResult(game));
    },

    undo(gameId) {
      const game = getOrFail(gameId);
      if (game.moveHistory.length === 0) {
        return Promise.resolve(actionResult(game));
      }
      game.moveHistory.pop();
      game.board = cloneBoardRep(startBoard);
      game.currentPlayer = "black";
      for (const m of game.moveHistory) {
        game.board = applyMoveToBoardRep(game.board, m);
        game.currentPlayer = game.currentPlayer === "black" ? "white" : "black";
      }
      return Promise.resolve(actionResult(game));
    },

    resign(gameId, color) {
      getOrFail(gameId);
      const winner = color === "black" ? "white" : "black";
      return Promise.resolve({
        moves: {},
        gameOver: { winner, reason: "resignation" },
      });
    },

    draw(gameId) {
      getOrFail(gameId);
      return Promise.resolve({
        moves: {},
        gameOver: { winner: "draw" as const, reason: "draw" },
      });
    },
  };
}
