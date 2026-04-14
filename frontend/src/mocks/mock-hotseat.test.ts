import { describe, expect, it } from "vitest";
import { startBoard } from "../board-logic";
import { createMockHotseatApi } from "./mock-hotseat";

describe("createMockHotseatApi", () => {
  it("createGame returns a gameId and getState returns initial board", async () => {
    const api = createMockHotseatApi();
    const gameId = await api.createGame();
    expect(gameId).toBeTruthy();

    const state = await api.getState(gameId);
    expect(state.currentPlayer).toBe("black");
    expect(state.boardRep.black).toEqual(startBoard.black);
    expect(state.boardRep.white).toEqual(startBoard.white);
    expect(state.boardRep.king).toBe(startBoard.king);
    expect(state.moveHistory).toEqual([]);
    expect(state.gameOver).toBeNull();
    expect(Object.keys(state.moves).length).toBeGreaterThan(0);
  });

  it("sendMove updates board and switches player", async () => {
    const api = createMockHotseatApi();
    const gameId = await api.createGame();
    const state = await api.getState(gameId);

    const from = Number(Object.keys(state.moves)[0]);
    const move = { from, to: state.moves[from][0].to };

    const result = await api.sendMove(gameId, move);
    expect(result.gameOver).toBeNull();

    const updated = await api.getState(gameId);
    expect(updated.currentPlayer).toBe("white");
    expect(updated.moveHistory).toHaveLength(1);
    expect(updated.moveHistory[0].from).toBe(move.from);
    expect(updated.moveHistory[0].to).toBe(move.to);
  });

  it("undo reverts the last move", async () => {
    const api = createMockHotseatApi();
    const gameId = await api.createGame();
    const initial = await api.getState(gameId);

    const from = Number(Object.keys(initial.moves)[0]);
    const move = { from, to: initial.moves[from][0].to };
    await api.sendMove(gameId, move);

    await api.undo(gameId);
    const reverted = await api.getState(gameId);
    expect(reverted.currentPlayer).toBe("black");
    expect(reverted.moveHistory).toHaveLength(0);
  });

  it("undo on empty history is a no-op", async () => {
    const api = createMockHotseatApi();
    const gameId = await api.createGame();
    const result = await api.undo(gameId);
    expect(result.gameOver).toBeNull();

    const state = await api.getState(gameId);
    expect(state.currentPlayer).toBe("black");
    expect(state.moveHistory).toHaveLength(0);
  });

  it("resign returns game over with opponent as winner", async () => {
    const api = createMockHotseatApi();
    const gameId = await api.createGame();
    const result = await api.resign(gameId, "black");
    expect(result.gameOver?.winner).toBe("white");
    expect(result.gameOver?.reason).toBe("resignation");
  });

  it("draw returns game over with draw", async () => {
    const api = createMockHotseatApi();
    const gameId = await api.createGame();
    const result = await api.draw(gameId);
    expect(result.gameOver?.winner).toBe("draw");
  });

  it("throws on nonexistent gameId", () => {
    const api = createMockHotseatApi();
    expect(() => api.getState("nonexistent")).toThrow("not found");
    expect(() => api.sendMove("nonexistent", { from: 0, to: 1 })).toThrow(
      "not found",
    );
  });
});
