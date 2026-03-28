import { render } from "@solidjs/testing-library";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import Clock from "./Clock";

describe("Clock", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("displays formatted time from initialSeconds", () => {
    const { container } = render(() => (
      <Clock initialSeconds={300} active={false} />
    ));
    expect(container.textContent).toBe("5:00");
  });

  it("counts down when active", () => {
    const { container } = render(() => (
      <Clock initialSeconds={300} active={true} />
    ));
    expect(container.textContent).toBe("5:00");
    vi.advanceTimersByTime(3000);
    expect(container.textContent).toBe("4:57");
  });
});
