import { beforeEach, describe, expect, it, vi } from "vitest";
import { getLastWebSocket, resetLastWebSocket } from "../test-setup";
import { createGameWebSocket } from "./ws-utils";

describe("createGameWebSocket", () => {
  const onMessage = vi.fn();
  const onConnected = vi.fn();
  const onDisconnected = vi.fn();

  beforeEach(() => {
    resetLastWebSocket();
    onMessage.mockClear();
    onConnected.mockClear();
    onDisconnected.mockClear();
  });

  function createWs() {
    return createGameWebSocket<{ type: string }>({
      url: "/test/ws",
      onMessage,
      onConnected,
      onDisconnected,
    });
  }

  it("constructs WebSocket with correct URL", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    expect(fake.url).toBe("ws://localhost:3000/test/ws");
  });

  it("sends auth message on open", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    fake.simulateOpen();
    expect(fake.sent).toEqual([
      JSON.stringify({ type: "auth", token: "tok123" }),
    ]);
  });

  it("calls onConnected on open", () => {
    const ws = createWs();
    ws.open("tok123");
    getLastWebSocket().simulateOpen();
    expect(onConnected).toHaveBeenCalledOnce();
  });

  it("forwards parsed messages to onMessage", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    fake.simulateOpen();
    fake.simulateMessage({ type: "gameState", data: "test" });
    expect(onMessage).toHaveBeenCalledWith({ type: "gameState", data: "test" });
  });

  it("filters out error messages", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    fake.simulateOpen();
    fake.simulateMessage({
      type: "error",
      code: "invalid_move",
      message: "bad",
    });
    expect(onMessage).not.toHaveBeenCalled();
  });

  it("calls onDisconnected on close", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    fake.simulateOpen();
    fake.simulateClose();
    expect(onDisconnected).toHaveBeenCalledOnce();
  });

  it("send() JSON-stringifies and sends via WebSocket", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    fake.simulateOpen();
    ws.send({ type: "move", from: 5, to: 16 });
    expect(fake.sent).toContain(
      JSON.stringify({ type: "move", from: 5, to: 16 }),
    );
  });

  it("close() triggers onclose on the WebSocket", () => {
    const ws = createWs();
    ws.open("tok123");
    const fake = getLastWebSocket();
    fake.simulateOpen();
    ws.close();
    expect(onDisconnected).toHaveBeenCalledOnce();
  });
});
