import { describe, expect, it } from "vitest";
import type { BoardRep } from "./board-logic";
import {
  applyMoveToBoardRep,
  cloneBoardRep,
  computeBoardAtMove,
  computePiecesAtCursor,
  indexToAlgebraic,
  startBoard,
} from "./board-logic";

function board(black: number[], white: number[], king: number): BoardRep {
  return { black: new Set(black), white: new Set(white), king };
}

describe("applyMoveToBoardRep", () => {
  it("moving a black piece removes it from the old position and adds it to the new", () => {
    const b = board([27, 50], [80], 60);
    const result = applyMoveToBoardRep(b, { from: 27, to: 28 });
    expect(result.black.has(27)).toBe(false);
    expect(result.black.has(28)).toBe(true);
    expect(result.black.has(50)).toBe(true);
  });

  it("moving the king updates the king field", () => {
    const b = board([27], [80], 60);
    const result = applyMoveToBoardRep(b, { from: 60, to: 49 });
    expect(result.king).toBe(49);
  });

  it("captures remove pieces from the correct set", () => {
    const b = board([27], [50, 51], 60);
    const result = applyMoveToBoardRep(b, {
      from: 27,
      to: 28,
      captures: [50, 60],
    });
    expect(result.white.has(50)).toBe(false);
    expect(result.white.has(51)).toBe(true);
    expect(result.king).toBe(-1);
  });

  it("a move without captures preserves all non-moving pieces", () => {
    const b = board([27, 50], [80, 90], 60);
    const result = applyMoveToBoardRep(b, { from: 27, to: 28 });
    expect(result.black.size).toBe(2);
    expect(result.white.size).toBe(2);
    expect(result.king).toBe(60);
  });

  it("total piece count decreases by exactly the number of captures", () => {
    const b = board([27], [49, 51, 39], 60);
    const before = b.black.size + b.white.size + (b.king >= 0 ? 1 : 0);
    const result = applyMoveToBoardRep(b, {
      from: 27,
      to: 50,
      captures: [49, 51],
    });
    const after =
      result.black.size + result.white.size + (result.king >= 0 ? 1 : 0);
    expect(before - after).toBe(2);
  });
});

describe("computeBoardAtMove", () => {
  it("index 0 returns the board after exactly one move applied to startBoard", () => {
    const move = { from: 3, to: 2 };
    const result = computeBoardAtMove([move], 0);
    const expected = applyMoveToBoardRep(cloneBoardRep(startBoard), move);
    expect(result.black).toEqual(expected.black);
    expect(result.white).toEqual(expected.white);
    expect(result.king).toBe(expected.king);
  });

  it("index N equals applying N+1 moves sequentially from startBoard", () => {
    const moves = [
      { from: 3, to: 2 },
      { from: 59, to: 57 },
      { from: 7, to: 8 },
    ];
    const result = computeBoardAtMove(moves, 2);
    let expected = cloneBoardRep(startBoard);
    for (const m of moves) {
      expected = applyMoveToBoardRep(expected, m);
    }
    expect(result.black).toEqual(expected.black);
    expect(result.white).toEqual(expected.white);
    expect(result.king).toBe(expected.king);
  });

  it("does not mutate startBoard", () => {
    const blackBefore = new Set(startBoard.black);
    const whiteBefore = new Set(startBoard.white);
    const kingBefore = startBoard.king;
    computeBoardAtMove([{ from: 3, to: 2 }], 0);
    expect(startBoard.black).toEqual(blackBefore);
    expect(startBoard.white).toEqual(whiteBefore);
    expect(startBoard.king).toBe(kingBefore);
  });
});

describe("cloneBoardRep", () => {
  it("mutating the clone does not affect the original", () => {
    const original = board([27, 50], [80], 60);
    const clone = cloneBoardRep(original);
    clone.black.delete(27);
    clone.black.add(99);
    clone.white.add(42);
    clone.king = 5;
    expect(original.black.has(27)).toBe(true);
    expect(original.black.has(99)).toBe(false);
    expect(original.white.has(42)).toBe(false);
    expect(original.king).toBe(60);
  });
});

describe("computePiecesAtCursor", () => {
  it("at cursor = history.length (start position), returns the 37 starting pieces", () => {
    const pieces = computePiecesAtCursor([], 0);
    expect(pieces).toHaveLength(
      startBoard.black.size + startBoard.white.size + 1,
    );
    const kinds = pieces.reduce(
      (acc, p) => {
        acc[p.kind] = (acc[p.kind] ?? 0) + 1;
        return acc;
      },
      {} as Record<string, number>,
    );
    expect(kinds.black).toBe(startBoard.black.size);
    expect(kinds.white).toBe(startBoard.white.size);
    expect(kinds.king).toBe(1);
  });

  it("ids are unique", () => {
    const pieces = computePiecesAtCursor([], 0);
    const ids = new Set(pieces.map((p) => p.id));
    expect(ids.size).toBe(pieces.length);
  });

  it("after a move, the same piece id is present at the new square", () => {
    const before = computePiecesAtCursor([], 0);
    const moverBefore = before.find((p) => p.square === 3);
    if (!moverBefore) throw new Error("expected a piece at square 3");
    const after = computePiecesAtCursor([{ from: 3, to: 2 }], 0);
    const moverAfter = after.find((p) => p.id === moverBefore.id);
    expect(moverAfter).toBeDefined();
    expect(moverAfter?.square).toBe(2);
  });

  it("captured pieces are absent at the post-capture cursor and present beforehand", () => {
    const moves = [{ from: 44, to: 58, captures: [59] }];
    const beforeCapture = computePiecesAtCursor(moves, 1); // before move 0
    const afterCapture = computePiecesAtCursor(moves, 0); // after move 0
    expect(beforeCapture.some((p) => p.square === 59)).toBe(true);
    expect(afterCapture.some((p) => p.square === 59)).toBe(false);
  });

  it("navigating backward past a capture restores the captured piece with its original id", () => {
    const moves = [{ from: 44, to: 58, captures: [59] }];
    const initial = computePiecesAtCursor([], 0);
    const capturedInitial = initial.find((p) => p.square === 59);
    if (!capturedInitial) throw new Error("expected piece at 59 initially");

    // Forward: after the capture, the piece is gone.
    const forward = computePiecesAtCursor(moves, 0);
    expect(forward.find((p) => p.id === capturedInitial.id)).toBeUndefined();

    // Backward: scrubbing back puts the same id back at square 59.
    const back = computePiecesAtCursor(moves, 1);
    const restored = back.find((p) => p.id === capturedInitial.id);
    expect(restored).toBeDefined();
    expect(restored?.square).toBe(59);
  });

  it("piece identity is stable across many moves of the same piece", () => {
    const moves = [
      { from: 3, to: 2 },
      { from: 59, to: 57 },
      { from: 2, to: 1 },
    ];
    const start = computePiecesAtCursor([], 0);
    const tracked = start.find((p) => p.square === 3);
    if (!tracked) throw new Error("expected piece at square 3 initially");

    const afterAll = computePiecesAtCursor(moves, 0);
    const found = afterAll.find((p) => p.id === tracked.id);
    expect(found).toBeDefined();
    expect(found?.square).toBe(1);
  });

  it("moving the king preserves the king id", () => {
    const after = computePiecesAtCursor([{ from: 60, to: 49 }], 0);
    const king = after.find((p) => p.kind === "king");
    expect(king).toBeDefined();
    expect(king?.id).toBe("king");
    expect(king?.square).toBe(49);
  });

  it("does not mutate startBoard", () => {
    const blackBefore = new Set(startBoard.black);
    computePiecesAtCursor([{ from: 3, to: 2 }], 0);
    expect(startBoard.black).toEqual(blackBefore);
  });
});

describe("indexToAlgebraic", () => {
  it("maps known positions correctly", () => {
    expect(indexToAlgebraic(0)).toBe("a11");
    expect(indexToAlgebraic(10)).toBe("k11");
    expect(indexToAlgebraic(60)).toBe("f6");
    expect(indexToAlgebraic(120)).toBe("k1");
  });
});
