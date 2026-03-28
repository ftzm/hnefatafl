import { render } from "@solidjs/testing-library";
import { describe, expect, it, vi } from "vitest";
import type { Move } from "../board-logic";
import { startBoard } from "../board-logic";
import { GameProvider, useGame } from "../game-context";
import Board from "./Board";

const blackMove: Move = { from: 3, to: 2 };

function renderBoard(opts?: { onMove?: (m: Move) => void; init?: () => void }) {
  const onMove = opts?.onMove ?? vi.fn();
  let ctx!: ReturnType<typeof useGame>;
  const result = render(() => (
    <GameProvider>
      {(() => {
        ctx = useGame();
        opts?.init?.call(null);
        return null;
      })()}
      <Board onMove={onMove} />
    </GameProvider>
  ));
  return { ...result, ctx, onMove };
}

function pieceAt(container: HTMLElement, index: number): string | null {
  const square = container.querySelector(`[data-index="${index}"]`);
  if (!square) return null;
  const piece = square.querySelector(".piece");
  if (!piece) return null;
  if (piece.classList.contains("black")) return "black";
  if (piece.classList.contains("white")) return "white";
  if (piece.classList.contains("king")) return "king";
  return null;
}

describe("Board", () => {
  it("renders pieces at the correct starting positions", () => {
    const { container } = renderBoard();
    for (const idx of startBoard.black) {
      expect(pieceAt(container, idx)).toBe("black");
    }
    for (const idx of startBoard.white) {
      expect(pieceAt(container, idx)).toBe("white");
    }
    expect(pieceAt(container, startBoard.king)).toBe("king");
  });

  it("after a move, piece is at the new square and absent from the old", () => {
    const { container, ctx } = renderBoard();
    expect(pieceAt(container, 3)).toBe("black");
    ctx.applyMove(blackMove);
    expect(pieceAt(container, 3)).toBe(null);
    expect(pieceAt(container, 2)).toBe("black");
  });

  it("after a capture, the captured piece is absent from the board", () => {
    const { container, ctx } = renderBoard();
    expect(pieceAt(container, 59)).toBe("white");
    ctx.applyMove({ from: 44, to: 58, captures: [59] });
    expect(pieceAt(container, 59)).toBe(null);
  });

  it("clicking a piece with legal moves highlights exactly its destinations", () => {
    const { container, ctx } = renderBoard();
    // Give piece at index 3 some legal moves
    ctx.setMoves({ 3: [[2], [14]] });
    const square3 = container.querySelector('[data-index="3"]') as HTMLElement;
    square3.click();
    const highlighted = container.querySelectorAll(".valid-move");
    const indices = Array.from(highlighted).map((el) =>
      Number(el.getAttribute("data-index")),
    );
    expect(indices.sort((a, b) => a - b)).toEqual([2, 14]);
  });

  it("clicking a different piece updates highlights to the new piece's destinations", () => {
    const { container, ctx } = renderBoard();
    ctx.setMoves({ 3: [[2]], 4: [[15]] });
    (container.querySelector('[data-index="3"]') as HTMLElement).click();
    expect(
      container
        .querySelector('[data-index="2"]')
        ?.classList.contains("valid-move"),
    ).toBe(true);
    (container.querySelector('[data-index="4"]') as HTMLElement).click();
    expect(
      container
        .querySelector('[data-index="2"]')
        ?.classList.contains("valid-move"),
    ).toBe(false);
    expect(
      container
        .querySelector('[data-index="15"]')
        ?.classList.contains("valid-move"),
    ).toBe(true);
  });

  it("clicking an empty non-highlighted square clears all highlights", () => {
    const { container, ctx } = renderBoard();
    ctx.setMoves({ 3: [[2]] });
    (container.querySelector('[data-index="3"]') as HTMLElement).click();
    expect(container.querySelectorAll(".valid-move").length).toBe(1);
    // Click an empty square that is not a legal destination
    (container.querySelector('[data-index="30"]') as HTMLElement).click();
    expect(container.querySelectorAll(".valid-move").length).toBe(0);
  });

  it("when movesDisabled, clicking a piece does not produce highlights", async () => {
    const { container, ctx } = renderBoard();
    ctx.setMoves({ 3: [[2]] });
    ctx.applyMove(blackMove);
    ctx.applyMove({ from: 59, to: 57 });
    // Navigate back so movesDisabled is true
    await ctx.viewPrev();
    expect(ctx.movesDisabled()).toBe(true);
    (container.querySelector('[data-index="3"]') as HTMLElement).click();
    expect(container.querySelectorAll(".valid-move").length).toBe(0);
  });
});
