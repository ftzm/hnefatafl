export interface GameWebSocket {
  open(token: string): void;
  close(): void;
  send(msg: unknown): void;
}

export function createGameWebSocket<TServerMsg>(config: {
  url: string;
  onMessage: (msg: TServerMsg) => void;
  onConnected: () => void;
  onDisconnected: () => void;
}): GameWebSocket {
  let ws: WebSocket | null = null;

  return {
    open(token: string) {
      const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
      const wsUrl = `${protocol}//${window.location.host}${config.url}`;
      ws = new WebSocket(wsUrl);

      ws.onopen = () => {
        ws?.send(JSON.stringify({ type: "auth", token }));
        config.onConnected();
      };

      ws.onmessage = (event) => {
        const data = JSON.parse(event.data as string) as
          | TServerMsg
          | { type: "error"; code: string; message: string };
        if (
          typeof data === "object" &&
          data !== null &&
          "type" in data &&
          data.type === "error"
        ) {
          console.error("WebSocket error:", data);
          return;
        }
        config.onMessage(data as TServerMsg);
      };

      ws.onclose = () => {
        config.onDisconnected();
      };
    },

    close() {
      ws?.close();
      ws = null;
    },

    send(msg: unknown) {
      ws?.send(JSON.stringify(msg));
    },
  };
}
