import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { getLastWebSocket, resetLastWebSocket } from "../test-setup";
import { createGameWebSocket } from "./ws-utils";

describe("createGameWebSocket", () => {
  const onMessage = vi.fn();
  const onConnected = vi.fn();
  const onDisconnected = vi.fn();
  const onConnecting = vi.fn();
  const onError = vi.fn();

  beforeEach(() => {
    vi.useFakeTimers();
    resetLastWebSocket();
    onMessage.mockClear();
    onConnected.mockClear();
    onDisconnected.mockClear();
    onConnecting.mockClear();
    onError.mockClear();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  function createWs() {
    return createGameWebSocket<{ type: string }>({
      url: "/test/ws",
      onMessage,
      onConnected,
      onDisconnected,
      onConnecting,
      onError,
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

  it("calls onError for server error messages", () => {
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
    expect(onError).toHaveBeenCalledWith({
      code: "invalid_move",
      message: "bad",
    });
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

  describe("auto-reconnect", () => {
    it("schedules reconnect when connection fails before open", () => {
      const ws = createWs();
      ws.open("tok123");
      const fake = getLastWebSocket();
      fake.simulateError();
      fake.simulateClose();
      expect(onConnecting).toHaveBeenCalledWith(1);
      expect(onDisconnected).toHaveBeenCalledOnce();
    });

    it("schedules reconnect when connection drops after open", () => {
      const ws = createWs();
      ws.open("tok123");
      const fake = getLastWebSocket();
      fake.simulateOpen();
      fake.simulateClose();
      expect(onConnecting).toHaveBeenCalledWith(1);
    });

    it("creates a new WebSocket after backoff delay", () => {
      const ws = createWs();
      ws.open("tok123");
      getLastWebSocket().simulateClose();
      resetLastWebSocket();

      vi.advanceTimersByTime(1000);
      const fake2 = getLastWebSocket();
      expect(fake2.url).toBe("ws://localhost:3000/test/ws");
    });

    it("calls onConnected on successful reconnect", () => {
      const ws = createWs();
      ws.open("tok123");
      getLastWebSocket().simulateOpen();
      expect(onConnected).toHaveBeenCalledTimes(1);

      getLastWebSocket().simulateClose();
      vi.advanceTimersByTime(1000);
      getLastWebSocket().simulateOpen();
      expect(onConnected).toHaveBeenCalledTimes(2);
    });

    it("uses exponential backoff", () => {
      const ws = createWs();
      ws.open("tok123");

      // First failure -> 1s backoff
      getLastWebSocket().simulateClose();
      expect(onConnecting).toHaveBeenCalledWith(1);
      resetLastWebSocket();
      vi.advanceTimersByTime(999);
      expect(() => getLastWebSocket()).toThrow();
      vi.advanceTimersByTime(1);
      const fake2 = getLastWebSocket();

      // Second failure -> 2s backoff
      fake2.simulateClose();
      expect(onConnecting).toHaveBeenCalledWith(2);
      resetLastWebSocket();
      vi.advanceTimersByTime(1999);
      expect(() => getLastWebSocket()).toThrow();
      vi.advanceTimersByTime(1);
      const fake3 = getLastWebSocket();

      // Third failure -> 4s backoff
      fake3.simulateClose();
      expect(onConnecting).toHaveBeenCalledWith(3);
    });

    it("does not reconnect on client-initiated close", () => {
      const ws = createWs();
      ws.open("tok123");
      getLastWebSocket().simulateOpen();
      ws.close();
      expect(onConnecting).not.toHaveBeenCalled();
      vi.advanceTimersByTime(60000);
      expect(onConnecting).not.toHaveBeenCalled();
    });

    it("resets retry count after successful reconnect", () => {
      const ws = createWs();
      ws.open("tok123");

      // Fail a few times to build up backoff
      getLastWebSocket().simulateClose();
      vi.advanceTimersByTime(1000);
      getLastWebSocket().simulateClose();
      vi.advanceTimersByTime(2000);

      // Successful reconnect
      getLastWebSocket().simulateOpen();
      onConnecting.mockClear();

      // Next failure should start at 1s again
      getLastWebSocket().simulateClose();
      expect(onConnecting).toHaveBeenCalledWith(1);
    });
  });
});
