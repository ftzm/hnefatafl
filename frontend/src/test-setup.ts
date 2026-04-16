import { vi } from "vitest";

globalThis.ResizeObserver = class {
  observe() {}
  unobserve() {}
  disconnect() {}
} as unknown as typeof ResizeObserver;

Element.prototype.animate = vi.fn().mockReturnValue({
  finished: Promise.resolve(),
  cancel: vi.fn(),
});

Element.prototype.scrollIntoView = vi.fn();

// --- FakeWebSocket for testing ws-utils and services ---

let lastCreatedWebSocket: FakeWebSocket | null = null;

export class FakeWebSocket {
  url: string;
  sent: string[] = [];
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onclose: (() => void) | null = null;
  onerror: ((event: Event) => void) | null = null;

  constructor(url: string) {
    this.url = url;
    lastCreatedWebSocket = this;
  }

  send(data: string) {
    this.sent.push(data);
  }

  close() {
    this.onclose?.();
  }

  simulateOpen() {
    this.onopen?.();
  }

  simulateMessage(data: unknown) {
    this.onmessage?.({ data: JSON.stringify(data) });
  }

  simulateClose() {
    this.onclose?.();
  }

  simulateError() {
    this.onerror?.(new Event("error"));
  }
}

export function getLastWebSocket(): FakeWebSocket {
  if (!lastCreatedWebSocket) throw new Error("No WebSocket created");
  return lastCreatedWebSocket;
}

export function resetLastWebSocket() {
  lastCreatedWebSocket = null;
}

globalThis.WebSocket = FakeWebSocket as unknown as typeof WebSocket;
