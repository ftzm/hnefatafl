import { describe, expect, it } from "vitest";
import {
  mapApiMove,
  mapBoard,
  mapGameOver,
  mapHistory,
  mapMoves,
} from "./mappers";

describe("mapBoard", () => {
  it("converts arrays to Sets and passes king through", () => {
    const result = mapBoard({ black: [1, 2, 3], white: [10, 20], king: 60 });
    expect(result.black).toEqual(new Set([1, 2, 3]));
    expect(result.white).toEqual(new Set([10, 20]));
    expect(result.king).toBe(60);
  });

  it("handles empty arrays", () => {
    const result = mapBoard({ black: [], white: [], king: 60 });
    expect(result.black).toEqual(new Set());
    expect(result.white).toEqual(new Set());
  });
});

describe("mapApiMove", () => {
  it("maps captures when present", () => {
    const result = mapApiMove({ from: 5, to: 16, captures: [10, 11] });
    expect(result).toEqual({ from: 5, to: 16, captures: [10, 11] });
  });

  it("sets captures to undefined when empty", () => {
    const result = mapApiMove({ from: 5, to: 16, captures: [] });
    expect(result).toEqual({ from: 5, to: 16, captures: undefined });
  });
});

describe("mapMoves", () => {
  it("converts string keys to number keys", () => {
    const result = mapMoves({
      "5": [{ to: 16, captures: [] }],
      "60": [
        { to: 49, captures: [] },
        { to: 71, captures: [70] },
      ],
    });
    expect(result[5]).toEqual([{ to: 16, captures: [] }]);
    expect(result[60]).toHaveLength(2);
    expect(result[60][1]).toEqual({ to: 71, captures: [70] });
  });

  it("handles empty map", () => {
    expect(mapMoves({})).toEqual({});
  });
});

describe("mapGameOver", () => {
  it("returns null for ongoing game", () => {
    expect(mapGameOver({ state: "ongoing" })).toBeNull();
  });

  it("returns winner and reason for finished game", () => {
    const result = mapGameOver({
      state: "finished",
      winner: "white",
      reason: "king_escaped",
    });
    expect(result).toEqual({ winner: "white", reason: "king_escaped" });
  });

  it("handles draw", () => {
    const result = mapGameOver({
      state: "finished",
      winner: "draw",
      reason: "draw",
    });
    expect(result).toEqual({ winner: "draw", reason: "draw" });
  });
});

describe("mapHistory", () => {
  it("extracts moves from history entries, dropping color", () => {
    const result = mapHistory([
      { color: "black", move: { from: 5, to: 16, captures: [] } },
      { color: "white", move: { from: 60, to: 49, captures: [50] } },
    ]);
    expect(result).toEqual([
      { from: 5, to: 16, captures: undefined },
      { from: 60, to: 49, captures: [50] },
    ]);
  });

  it("handles empty history", () => {
    expect(mapHistory([])).toEqual([]);
  });
});
