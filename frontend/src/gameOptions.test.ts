import { describe, expect, it } from "vitest";
import { getTimeControl, timeOptions } from "./gameOptions";

describe("timeOptions", () => {
  it("has exactly 3 timed options", () => {
    const timed = timeOptions.filter((o) => o.timeControl !== null);
    expect(timed).toHaveLength(3);
  });

  it("untimed option has null timeControl", () => {
    expect(getTimeControl("none")).toBeNull();
  });

  it("timed options have correct seconds values", () => {
    const expected = [
      { label: "5 min", initialTime: 300, increment: 0 },
      { label: "10 min", initialTime: 600, increment: 0 },
      { label: "15 min", initialTime: 900, increment: 0 },
    ];
    for (const { label, initialTime, increment } of expected) {
      const opt = timeOptions.find((o) => o.label === label);
      expect(opt?.timeControl).toEqual({ initialTime, increment });
    }
  });

  it("throws for unknown value", () => {
    expect(() => getTimeControl("unknown")).toThrow();
  });
});
