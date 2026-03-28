import { render } from "@solidjs/testing-library";
import { describe, expect, it } from "vitest";
import PlayerBar from "./PlayerBar";

describe("PlayerBar", () => {
  it("renders the correct number of captured piece indicators", () => {
    const { container } = render(() => (
      <PlayerBar color="black" capturedCount={3} active={false} />
    ));
    expect(container.querySelectorAll(".cap-piece").length).toBe(3);
  });

  it("updates captured count when props change", () => {
    const { container, unmount } = render(() => (
      <PlayerBar color="black" capturedCount={1} active={false} />
    ));
    expect(container.querySelectorAll(".cap-piece").length).toBe(1);
    unmount();
    const { container: c2 } = render(() => (
      <PlayerBar color="black" capturedCount={4} active={false} />
    ));
    expect(c2.querySelectorAll(".cap-piece").length).toBe(4);
  });
});
