import { describe, expect, it } from "vitest";
import { formatClockMs, hasUndoableMove } from "./GameLayout";

describe("formatClockMs", () => {
  it("formats minutes and seconds", () => {
    expect(formatClockMs(300_000)).toBe("5:00");
  });

  it("pads seconds to two digits", () => {
    expect(formatClockMs(65_000)).toBe("1:05");
  });

  it("formats zero", () => {
    expect(formatClockMs(0)).toBe("0:00");
  });

  it("truncates sub-second values", () => {
    expect(formatClockMs(5_999)).toBe("0:05");
  });

  it("formats large values", () => {
    expect(formatClockMs(3_600_000)).toBe("60:00");
  });

  it("clamps negative to zero", () => {
    expect(formatClockMs(-1000)).toBe("0:00");
  });
});

describe("hasUndoableMove", () => {
  it("online Black can undo after its own first move", () => {
    expect(hasUndoableMove("black", 0)).toBe(false);
    expect(hasUndoableMove("black", 1)).toBe(true);
  });

  it("online White cannot undo until it has moved", () => {
    // After Black's opening move (length 1) White has not moved yet.
    expect(hasUndoableMove("white", 1)).toBe(false);
    expect(hasUndoableMove("white", 2)).toBe(true);
  });

  it("hotseat/AI (no player color) can undo any played move", () => {
    expect(hasUndoableMove(null, 0)).toBe(false);
    expect(hasUndoableMove(null, 1)).toBe(true);
  });
});
