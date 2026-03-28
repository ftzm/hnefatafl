import { describe, expect, it } from "vitest";
import type { BoardRep, MovesMap } from "../board-logic";
import {
  checkGameOver,
  computeCaptures,
  generateLegalMoves,
} from "../mocks/mock-game-logic";

function board(black: number[], white: number[], king: number): BoardRep {
  return { black: new Set(black), white: new Set(white), king };
}

function destinations(moves: MovesMap, piece: number): number[] {
  return (moves[piece] || []).map(([to]) => to);
}

describe("generateLegalMoves", () => {
  it("includes all empty squares along cardinal lines from the piece", () => {
    // Black at 27, king at 100.
    // Up: 16, 5. Down: 38, 49, 71, 82, 93, 104, 115 (throne 60 excluded).
    // Left: 26, 25, 24, 23, 22. Right: 28, 29, 30, 31, 32.
    const b = board([27], [], 100);
    const moves = generateLegalMoves(b, "black");
    expect(destinations(moves, 27)).toHaveLength(19);
  });

  it("destinations stop before an occupied square", () => {
    // Black at 27, friendly piece at 38 blocks the downward line.
    // Down destinations from 27 should not include 38 or anything beyond it.
    const b = board([27, 38], [], 100);
    const moves = generateLegalMoves(b, "black");
    const dests = destinations(moves, 27);
    expect(dests).not.toContain(38);
    expect(dests).not.toContain(49);
    expect(dests).toContain(16); // up still open
    expect(dests).toContain(28); // right still open
  });

  it("non-king pieces cannot land on corner squares", () => {
    // Black at 1 (b11), corner 0 is one step left.
    const b = board([1], [], 100);
    const moves = generateLegalMoves(b, "black");
    expect(destinations(moves, 1)).not.toContain(0);
  });

  it("non-king pieces pass through the empty throne but cannot stop on it", () => {
    // Black at 38 (f8), king at 100. Throne at 60 is empty.
    // Downward line: 49 is reachable, 60 is not, 71 and beyond are reachable.
    const b = board([38], [], 100);
    const moves = generateLegalMoves(b, "black");
    const dests = destinations(moves, 38);
    expect(dests).toContain(49);
    expect(dests).not.toContain(60);
    expect(dests).toContain(71);
    expect(dests).toContain(82);
  });

  it("king can land on corners and throne", () => {
    // King at 5 (f11). Corners 0 and 10 are on the same row.
    // Throne at 60 is straight down: 16, 27, 38, 49, 60.
    const b = board([], [], 5);
    const moves = generateLegalMoves(b, "white");
    const dests = destinations(moves, 5);
    expect(dests).toContain(0);
    expect(dests).toContain(10);
    expect(dests).toContain(60);
  });

  it("a fully surrounded piece has no legal destinations", () => {
    // Black at 50, blocked on all 4 sides by white at 49, 51, 39, 61.
    const b = board([50], [49, 51, 39, 61], 100);
    const moves = generateLegalMoves(b, "black");
    expect(moves[50]).toBeUndefined();
  });

  it("pieces on the board edge do not wrap to the next row", () => {
    // Black at 21 (k10, right edge). Index 22 is a9 (next row, left edge).
    const b = board([21], [], 100);
    const moves = generateLegalMoves(b, "black");
    const dests = destinations(moves, 21);
    expect(dests).not.toContain(22);
    expect(dests).toContain(20); // left along row
    expect(dests).toContain(32); // down in column
  });
});

describe("computeCaptures", () => {
  it("standard sandwich — enemy between mover and friendly piece is captured", () => {
    // Black at 49 and 52. Black moves 52 to 51. White at 50 is sandwiched: 49-50-51.
    const b = board([49, 52], [50], 100);
    const caps = computeCaptures(b, 52, 51, "black");
    expect(caps).toEqual([50]);
  });

  it("adjacent enemy without a hostile square beyond is not captured", () => {
    // Black moves 52 to 51. White at 50. No black piece at 49 — nothing beyond to complete the sandwich.
    const b = board([52], [50], 100);
    const caps = computeCaptures(b, 52, 51, "black");
    expect(caps).toHaveLength(0);
  });

  it("corner square acts as hostile — enemy between mover and corner is captured", () => {
    // Corner at 0. White at 1. Black moves from 3 to 2.
    // Sandwich: 0(corner) - 1(white) - 2(black lands).
    const b = board([3], [1], 100);
    const caps = computeCaptures(b, 3, 2, "black");
    expect(caps).toEqual([1]);
  });

  it("empty throne acts as hostile — enemy between mover and throne is captured", () => {
    // King at 100, throne at 60 is empty. White at 59. Black moves from 57 to 58.
    // Sandwich: 58(black lands) - 59(white) - 60(empty throne, hostile).
    const b = board([57], [59], 100);
    const caps = computeCaptures(b, 57, 58, "black");
    expect(caps).toEqual([59]);
  });

  it("a single move can capture multiple pieces", () => {
    // Black moves from 61 to 50. White at 49, 51, 39.
    // Black already at 48, 52, 28 to complete each sandwich.
    // Left: 48-49-50. Right: 50-51-52. Up: 28-39-50.
    const b = board([48, 52, 28, 61], [49, 51, 39], 100);
    const caps = computeCaptures(b, 61, 50, "black");
    expect(caps).toHaveLength(3);
    expect(caps).toContain(49);
    expect(caps).toContain(51);
    expect(caps).toContain(39);
  });

  it("the moving piece is not self-captured by landing between two enemies", () => {
    // Black moves from 61 to 50. White at 49 and 51 flank the destination.
    // No friendly pieces beyond — so no sandwich on the whites, and black is not captured.
    const b = board([61], [49, 51], 100);
    const caps = computeCaptures(b, 61, 50, "black");
    expect(caps).toHaveLength(0);
  });
});

describe("king capture via computeCaptures", () => {
  it("king on throne is captured when all 4 adjacent squares have black pieces", () => {
    // King at 60 (throne). Black at 71, 59, 61 already placed. Black moves 38 to 49.
    const b = board([38, 71, 59, 61], [], 60);
    const caps = computeCaptures(b, 38, 49, "black");
    expect(caps).toContain(60);
  });

  it("king with fewer than 4 hostile sides is not captured", () => {
    // King at 50. Black moves 52 to 51. Only east side hostile.
    const b = board([52], [], 50);
    const caps = computeCaptures(b, 52, 51, "black");
    expect(caps).not.toContain(50);
  });

  it("king adjacent to throne is captured with throne counting as one hostile side", () => {
    // King at 59. Neighbors: 58(left), 60(throne), 48(up), 70(down).
    // Black at 48, 70. Black moves 57 to 58. Throne provides the 4th side.
    const b = board([48, 70, 57], [], 59);
    const caps = computeCaptures(b, 57, 58, "black");
    expect(caps).toContain(59);
  });

  it("king in open field is captured when surrounded on all 4 sides by black", () => {
    // King at 50. Black at 39, 61, 49. Black moves 52 to 51.
    const b = board([39, 61, 49, 52], [], 50);
    const caps = computeCaptures(b, 52, 51, "black");
    expect(caps).toContain(50);
  });

  it("white moves cannot capture the king", () => {
    // computeCaptures only checks king capture when movingColor is "black".
    // King at 50, surrounded by white on 3 sides. White moves to complete the 4th.
    const b = board([], [39, 49, 52], 50);
    const caps = computeCaptures(b, 52, 51, "white");
    expect(caps).not.toContain(50);
  });
});

describe("checkGameOver", () => {
  it("king on a corner — white wins", () => {
    const b = board([27], [], 0);
    const result = checkGameOver(b, "black", { 27: [[28]] });
    expect(result).toEqual({ winner: "white", reason: "King escaped" });
  });

  it("king captured (position -1) — black wins", () => {
    const b = board([27], [], -1);
    const result = checkGameOver(b, "white", { 27: [[28]] });
    expect(result).toEqual({ winner: "black", reason: "King captured" });
  });

  it("current player has no legal moves — opponent wins", () => {
    const b = board([50], [49, 51, 39, 61], 100);
    const result = checkGameOver(b, "black", {});
    expect(result).toEqual({ winner: "white", reason: "No legal moves" });
  });

  it("returns null when game is still in progress", () => {
    const b = board([27], [50], 60);
    const result = checkGameOver(b, "black", { 27: [[28]] });
    expect(result).toBeNull();
  });
});
