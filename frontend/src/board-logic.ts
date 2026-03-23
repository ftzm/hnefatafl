import type { BoardRep, Move } from "./types";

export { type PlayerColor, type BoardRep, type Move, type MovesMap, type GameOverState } from "./types";

export const startBoard: BoardRep = {
  black: new Set([
    3, 4, 5, 6, 7, 16, 33, 44, 55, 66, 77, 43, 54, 65, 76, 87, 56, 64, 104, 113,
    114, 115, 116, 117,
  ]),
  white: new Set([38, 48, 49, 50, 58, 59, 61, 62, 70, 71, 72, 82]),
  king: 60,
};

export function cloneBoardRep(boardRep: BoardRep): BoardRep {
  return {
    black: new Set(boardRep.black),
    white: new Set(boardRep.white),
    king: boardRep.king,
  };
}

export function applyMoveToBoardRep(boardRep: BoardRep, move: Move): BoardRep {
  const newBoard = cloneBoardRep(boardRep);

  if (newBoard.black.has(move.from)) {
    newBoard.black.delete(move.from);
    newBoard.black.add(move.to);
  } else if (newBoard.white.has(move.from)) {
    newBoard.white.delete(move.from);
    newBoard.white.add(move.to);
  } else if (newBoard.king === move.from) {
    newBoard.king = move.to;
  }

  if (move.captures) {
    for (const cap of move.captures) {
      newBoard.black.delete(cap);
      newBoard.white.delete(cap);
      if (newBoard.king === cap) newBoard.king = -1;
    }
  }

  return newBoard;
}

export function computeBoardAtMove(
  moveHistory: Move[],
  moveIndex: number,
): BoardRep {
  let board = cloneBoardRep(startBoard);
  for (let i = 0; i <= moveIndex; i++) {
    board = applyMoveToBoardRep(board, moveHistory[i]);
  }
  return board;
}

export function indexToAlgebraic(index: number): string {
  const file = String.fromCharCode(97 + (index % 11));
  const rank = 11 - Math.floor(index / 11);
  return file + rank;
}
