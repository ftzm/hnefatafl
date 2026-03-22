import type { HotseatApi } from "./hotseat-api";
import {
  generateLegalMoves,
  checkGameOver,
  replayMoves,
} from "./mock-game-logic";

export function createMockHotseatApi(): HotseatApi {
  return {
    getGameState(moves) {
      const { board, currentPlayer } = replayMoves(moves);
      const legalMoves = generateLegalMoves(board, currentPlayer);
      const gameOver = checkGameOver(board, currentPlayer, legalMoves);
      return Promise.resolve({
        boardRep: board,
        currentPlayer,
        moves: legalMoves,
        gameOver,
      });
    },
  };
}
