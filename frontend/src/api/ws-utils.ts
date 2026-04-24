export interface ServerError {
  code: string;
  message: string;
  fatal: boolean;
}

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
  onConnecting?: (attempt: number) => void;
  onError?: (error: ServerError) => void;
}): GameWebSocket {
  let ws: WebSocket | null = null;
  let token: string | null = null;
  let closedByClient = false;
  let retryTimer: ReturnType<typeof setTimeout> | null = null;
  let retryAttempt = 0;

  const MAX_BACKOFF = 30000;

  function connect() {
    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    const wsUrl = `${protocol}//${window.location.host}${config.url}`;
    ws = new WebSocket(wsUrl);

    ws.onopen = () => {
      retryAttempt = 0;
      ws?.send(JSON.stringify({ type: "auth", token }));
      config.onConnected();
    };

    ws.onmessage = (event) => {
      const data = JSON.parse(event.data as string) as
        | TServerMsg
        | { type: "error"; code: string; message: string; fatal: boolean };
      if (
        typeof data === "object" &&
        data !== null &&
        "type" in data &&
        data.type === "error"
      ) {
        const err = data as {
          type: "error";
          code: string;
          message: string;
          fatal: boolean;
        };
        console.error("WebSocket error:", err);
        if (err.fatal) {
          closedByClient = true;
          cancelRetry();
          ws?.close();
        }
        config.onError?.({
          code: err.code,
          message: err.message,
          fatal: err.fatal,
        });
        return;
      }
      config.onMessage(data as TServerMsg);
    };

    ws.onerror = () => {
      // Browser fires onerror before onclose; onclose handles the logic.
    };

    ws.onclose = () => {
      config.onDisconnected();
      if (!closedByClient) {
        scheduleReconnect();
      }
    };
  }

  function scheduleReconnect() {
    retryAttempt++;
    const delay = Math.min(1000 * 2 ** (retryAttempt - 1), MAX_BACKOFF);
    config.onConnecting?.(retryAttempt);
    retryTimer = setTimeout(() => {
      retryTimer = null;
      connect();
    }, delay);
  }

  function cancelRetry() {
    if (retryTimer !== null) {
      clearTimeout(retryTimer);
      retryTimer = null;
    }
  }

  return {
    open(t: string) {
      token = t;
      closedByClient = false;
      retryAttempt = 0;
      cancelRetry();
      connect();
    },

    close() {
      closedByClient = true;
      cancelRetry();
      ws?.close();
      ws = null;
    },

    send(msg: unknown) {
      ws?.send(JSON.stringify(msg));
    },
  };
}
