import type { BoardRep, Move } from "./types";

export type {
  BoardRep,
  GameOverState,
  Move,
  MovesMap,
  PlayerColor,
} from "./types";

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

// === Piece-entity model ===
//
// The `BoardRep` is sufficient for game logic (it tracks which squares are
// occupied by which kind of piece) but it has no notion of *piece identity*:
// after a move, the "same" piece that was at `from` is now at `to`, but the
// representation only knows that one square has emptied and another has
// filled. For animation we need stable identity so the renderer can keep the
// same DOM node and animate its position.
//
// We assign each piece an id derived from its starting square (`b3`, `w38`,
// `king`). Walking the move history forward updates each piece's `square` and
// removes captured ones. This is a pure function of `(startBoard, history,
// cursor)`, so navigating forward, backward, or jumping to an arbitrary move
// always yields the same entities with the same ids — exactly what `<For>`
// needs to reuse DOM nodes and what CSS transitions need to animate them.

export type PieceKind = "black" | "white" | "king";

export interface Piece {
  id: string;
  kind: PieceKind;
  square: number;
}

function initialPieces(): Map<string, Piece> {
  const pieces = new Map<string, Piece>();
  for (const sq of startBoard.black) {
    pieces.set(`b${sq}`, { id: `b${sq}`, kind: "black", square: sq });
  }
  for (const sq of startBoard.white) {
    pieces.set(`w${sq}`, { id: `w${sq}`, kind: "white", square: sq });
  }
  pieces.set("king", { id: "king", kind: "king", square: startBoard.king });
  return pieces;
}

/**
 * Return the pieces visible at the given history cursor.
 *
 * `cursor` follows the same convention as `historyCursor` in the game store:
 * `0` is the present, larger values look further into the past. So
 * `moveHistory.length - cursor` moves are applied from the start position.
 */
export function computePiecesAtCursor(
  moveHistory: Move[],
  cursor: number,
): Piece[] {
  const pieces = initialPieces();
  // Index pieces by square for O(1) lookup during replay. We rebuild this map
  // lazily; for our scale (≤37 pieces, history length in the hundreds at most)
  // a linear scan would also be fine, but keeping it correct is easier than
  // re-doing it later.
  const bySquare = new Map<number, string>();
  for (const p of pieces.values()) bySquare.set(p.square, p.id);

  const movesToApply = Math.max(0, moveHistory.length - cursor);
  for (let i = 0; i < movesToApply; i++) {
    const move = moveHistory[i];
    const movedId = bySquare.get(move.from);
    if (movedId !== undefined) {
      const moved = pieces.get(movedId);
      if (moved) {
        bySquare.delete(move.from);
        moved.square = move.to;
        bySquare.set(move.to, moved.id);
      }
    }
    if (move.captures) {
      for (const cap of move.captures) {
        const capId = bySquare.get(cap);
        if (capId !== undefined) {
          pieces.delete(capId);
          bySquare.delete(cap);
        }
      }
    }
  }
  return Array.from(pieces.values());
}
