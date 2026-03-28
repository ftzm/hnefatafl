import { render } from "@solidjs/testing-library";
import { describe, expect, it } from "vitest";
import { GameProvider, useGame } from "../game-context";
import MoveHistory from "./MoveHistory";

function renderHistory() {
  let ctx!: ReturnType<typeof useGame>;
  const result = render(() => (
    <GameProvider>
      {(() => {
        ctx = useGame();
        return null;
      })()}
      <MoveHistory />
    </GameProvider>
  ));
  return { ...result, ctx };
}

describe("MoveHistory", () => {
  it("renders the correct number of move rows (paired black-white)", () => {
    const { container, ctx } = renderHistory();
    ctx.applyMove({ from: 3, to: 2 });
    ctx.applyMove({ from: 59, to: 57 });
    ctx.applyMove({ from: 7, to: 8 });
    ctx.applyMove({ from: 61, to: 63 });
    ctx.applyMove({ from: 4, to: 15 });
    // 5 moves = 3 rows (2 full pairs + 1 unpaired)
    expect(container.querySelectorAll(".move-row").length).toBe(3);
  });

  it("the most recent move has the 'current' class", () => {
    const { container, ctx } = renderHistory();
    ctx.applyMove({ from: 3, to: 2 });
    ctx.applyMove({ from: 59, to: 57 });
    const currentEls = container.querySelectorAll(".current");
    expect(currentEls.length).toBe(1);
    expect(currentEls[0].textContent).toContain("e6-c6");
  });

  it("navigating backward updates which move has the 'current' class", async () => {
    const { container, ctx } = renderHistory();
    ctx.applyMove({ from: 3, to: 2 });
    ctx.applyMove({ from: 59, to: 57 });
    await ctx.viewPrev();
    const currentEl = container.querySelector(".current");
    expect(currentEl).not.toBeNull();
    // After viewPrev, current should be the first move (d11-c11, i.e. index 3→2)
    expect(currentEl?.textContent).toContain("d11");
  });
});
