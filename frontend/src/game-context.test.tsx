import { render } from "@solidjs/testing-library";
import { describe, expect, it } from "vitest";
import type { Move } from "./board-logic";
import { startBoard } from "./board-logic";
import { GameProvider, useGame } from "./game-context";

// A move from the starting position that black can make.
const blackMove: Move = { from: 3, to: 2 };
// A move white can make after black's first move.
const whiteMove: Move = { from: 59, to: 57 };

function setupContext() {
  let ctx!: ReturnType<typeof useGame>;
  render(() => (
    <GameProvider>
      {(() => {
        ctx = useGame();
        ctx.initGame({
          boardRep: {
            black: new Set(startBoard.black),
            white: new Set(startBoard.white),
            king: startBoard.king,
          },
        });
        return null;
      })()}
    </GameProvider>
  ));
  return ctx;
}

describe("game context — state transitions", () => {
  it("applyMove alternates currentPlayer", () => {
    const ctx = setupContext();
    expect(ctx.store.game.currentPlayer).toBe("black");
    ctx.applyMove(blackMove);
    expect(ctx.store.game.currentPlayer).toBe("white");
    ctx.applyMove(whiteMove);
    expect(ctx.store.game.currentPlayer).toBe("black");
  });

  it("applyMove resets historyCursor to 0", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    await ctx.viewPrev();
    expect(ctx.store.game.historyCursor).toBe(1);
    ctx.applyMove({ from: 7, to: 8 });
    expect(ctx.store.game.historyCursor).toBe(0);
  });

  it("applyMove with captures increments the correct capturedPieces count", () => {
    const ctx = setupContext();
    expect(ctx.store.game.capturedPieces).toEqual({ black: 0, white: 0 });
    // Black captures a white piece. White piece at 59 is in the starting position.
    ctx.applyMove({ from: 44, to: 58, captures: [59] });
    expect(ctx.store.game.capturedPieces.white).toBe(1);
    expect(ctx.store.game.capturedPieces.black).toBe(0);
  });

  it("undoLastMove on empty history is a no-op", () => {
    const ctx = setupContext();
    const before = ctx.store.game.currentPlayer;
    ctx.undoLastMove();
    expect(ctx.store.game.currentPlayer).toBe(before);
    expect(ctx.store.game.moveHistory).toHaveLength(0);
  });

  it("undoLastMove restores the board to the state before the last move", () => {
    const ctx = setupContext();
    const boardBefore = {
      black: new Set(ctx.store.game.boardRep.black),
      white: new Set(ctx.store.game.boardRep.white),
      king: ctx.store.game.boardRep.king,
    };
    ctx.applyMove(blackMove);
    ctx.undoLastMove();
    expect(ctx.store.game.boardRep.black).toEqual(boardBefore.black);
    expect(ctx.store.game.boardRep.white).toEqual(boardBefore.white);
    expect(ctx.store.game.boardRep.king).toBe(boardBefore.king);
  });

  it("undoLastMove after a capture restores the capturedPieces count", () => {
    const ctx = setupContext();
    ctx.applyMove({ from: 44, to: 58, captures: [59] });
    expect(ctx.store.game.capturedPieces.white).toBe(1);
    ctx.undoLastMove();
    expect(ctx.store.game.capturedPieces.white).toBe(0);
  });

  it("two undos restore the board to two moves ago", () => {
    const ctx = setupContext();
    const boardAtStart = {
      black: new Set(ctx.store.game.boardRep.black),
      white: new Set(ctx.store.game.boardRep.white),
      king: ctx.store.game.boardRep.king,
    };
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    ctx.undoLastMove();
    ctx.undoLastMove();
    expect(ctx.store.game.boardRep.black).toEqual(boardAtStart.black);
    expect(ctx.store.game.boardRep.white).toEqual(boardAtStart.white);
    expect(ctx.store.game.boardRep.king).toBe(boardAtStart.king);
    expect(ctx.store.game.currentPlayer).toBe("black");
  });
});

function snapshotBoard(ctx: ReturnType<typeof useGame>) {
  return {
    black: new Set(ctx.store.game.boardRep.black),
    white: new Set(ctx.store.game.boardRep.white),
    king: ctx.store.game.boardRep.king,
    captures: { ...ctx.store.game.capturedPieces },
    cursor: ctx.store.game.historyCursor,
  };
}

function expectBoardEqual(
  ctx: ReturnType<typeof useGame>,
  snapshot: ReturnType<typeof snapshotBoard>,
) {
  expect(ctx.store.game.boardRep.black).toEqual(snapshot.black);
  expect(ctx.store.game.boardRep.white).toEqual(snapshot.white);
  expect(ctx.store.game.boardRep.king).toBe(snapshot.king);
  expect(ctx.store.game.capturedPieces).toEqual(snapshot.captures);
  expect(ctx.store.game.historyCursor).toBe(snapshot.cursor);
}

describe("game context — history navigation round-trips", () => {
  it("viewStart then viewEnd returns to the current position", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    const before = snapshotBoard(ctx);
    await ctx.viewStart();
    await ctx.viewEnd();
    expectBoardEqual(ctx, before);
  });

  it("jumpToMove to an earlier move and back to the last move restores state", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    ctx.applyMove({ from: 7, to: 8 });
    const before = snapshotBoard(ctx);
    await ctx.jumpToMove(0);
    await ctx.jumpToMove(2);
    expectBoardEqual(ctx, before);
  });
});

describe("game context — history navigation boundaries", () => {
  it("viewPrev at the earliest position is a no-op", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    await ctx.viewStart();
    const before = snapshotBoard(ctx);
    await ctx.viewPrev();
    expectBoardEqual(ctx, before);
  });

  it("viewEnd at the latest position is a no-op", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    const before = snapshotBoard(ctx);
    await ctx.viewEnd();
    expectBoardEqual(ctx, before);
  });

  it("viewStart shows the starting position with zero captures", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    await ctx.viewStart();
    expect(ctx.store.game.boardRep.black).toEqual(startBoard.black);
    expect(ctx.store.game.boardRep.white).toEqual(startBoard.white);
    expect(ctx.store.game.boardRep.king).toBe(startBoard.king);
    expect(ctx.store.game.capturedPieces).toEqual({ black: 0, white: 0 });
  });
});

describe("game context — history navigation correctness", () => {
  it("after 3 moves, viewPrev shows the board after move 2", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    const afterMove2 = snapshotBoard(ctx);
    ctx.applyMove({ from: 7, to: 8 });
    await ctx.viewPrev();
    // Board should match after move 2, but cursor will differ (it's 1 now).
    expect(ctx.store.game.boardRep.black).toEqual(afterMove2.black);
    expect(ctx.store.game.boardRep.white).toEqual(afterMove2.white);
    expect(ctx.store.game.boardRep.king).toBe(afterMove2.king);
  });

  it("navigating back past a capture reduces the captured count", async () => {
    const ctx = setupContext();
    // Move 1: black captures a white piece
    ctx.applyMove({ from: 44, to: 58, captures: [59] });
    expect(ctx.store.game.capturedPieces.white).toBe(1);
    // Move 2: some white move
    ctx.applyMove({ from: 61, to: 63 });
    // Navigate to before the capture
    await ctx.viewStart();
    expect(ctx.store.game.capturedPieces.white).toBe(0);
  });

  it("jumpToMove(0) shows the board after only the first move", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    const afterMove1 = {
      black: new Set(ctx.store.game.boardRep.black),
      white: new Set(ctx.store.game.boardRep.white),
      king: ctx.store.game.boardRep.king,
    };
    ctx.applyMove(whiteMove);
    ctx.applyMove({ from: 7, to: 8 });
    await ctx.jumpToMove(0);
    expect(ctx.store.game.boardRep.black).toEqual(afterMove1.black);
    expect(ctx.store.game.boardRep.white).toEqual(afterMove1.white);
    expect(ctx.store.game.boardRep.king).toBe(afterMove1.king);
  });

  it("jumpToMove shows the correct cumulative capture count", async () => {
    const ctx = setupContext();
    // Move 1: black captures a white piece
    ctx.applyMove({ from: 44, to: 58, captures: [59] });
    // Move 2: white moves
    ctx.applyMove({ from: 61, to: 63 });
    // Move 3: black captures another
    ctx.applyMove({ from: 55, to: 62, captures: [63] });
    expect(ctx.store.game.capturedPieces.white).toBe(2);
    // Jump back to after move 1: only 1 capture so far
    await ctx.jumpToMove(0);
    expect(ctx.store.game.capturedPieces.white).toBe(1);
  });
});

describe("game context — derived signals", () => {
  it("canViewPrev is false when there are no moves", () => {
    const ctx = setupContext();
    expect(ctx.canViewPrev()).toBe(false);
  });

  it("canViewPrev is false when already at the earliest position", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    await ctx.viewStart();
    expect(ctx.canViewPrev()).toBe(false);
  });

  it("canViewNext is false when cursor is at the present", () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    expect(ctx.canViewNext()).toBe(false);
  });

  it("movesDisabled is true when viewing a past position", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    await ctx.viewPrev();
    expect(ctx.movesDisabled()).toBe(true);
  });

  it("lastMove reflects the move at the current cursor position", async () => {
    const ctx = setupContext();
    ctx.applyMove(blackMove);
    ctx.applyMove(whiteMove);
    // At present, lastMove is the most recent move
    expect(ctx.lastMove()?.from).toBe(whiteMove.from);
    expect(ctx.lastMove()?.to).toBe(whiteMove.to);
    // Navigate back — lastMove should be the first move
    await ctx.viewPrev();
    expect(ctx.lastMove()?.from).toBe(blackMove.from);
    expect(ctx.lastMove()?.to).toBe(blackMove.to);
  });
});
