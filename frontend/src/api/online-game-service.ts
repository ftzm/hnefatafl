import type { Accessor } from "solid-js";
import { createSignal } from "solid-js";
import type { Move, PlayerColor } from "../board-logic";
import type { TimeControlValue } from "../gameOptions";
import { api } from "./client";
import type { components } from "./generated/rest";
import {
  mapApiMove,
  mapBoard,
  mapGameOver,
  mapHistory,
  mapMoves,
} from "./mappers";
import type { OnlineGameEvent } from "./types";
import { createGameWebSocket, type ServerError } from "./ws-utils";

type OnlineServerMessage = components["schemas"]["OnlineServerMessage"];

export interface OnlineGameService {
  createGame(opts: {
    creatorColor: PlayerColor;
    timeControl: TimeControlValue | null;
  }): Promise<{ playerToken: string; inviteToken: string }>;
  connect(token: string): void;
  disconnect(): void;
  sendMove(move: Move): void;
  resign(): void;
  offerDraw(): void;
  acceptDraw(): void;
  declineDraw(): void;
  requestUndo(): void;
  acceptUndo(): void;
  declineUndo(): void;
  sendChat(message: string): void;
  simulateTimeout?(delayMs?: number): void;
  events: Accessor<OnlineGameEvent | undefined>;
  connected: Accessor<boolean>;
  connecting: Accessor<boolean>;
}

function mapServerMessage(
  msg: OnlineServerMessage,
): OnlineGameEvent | undefined {
  switch (msg.type) {
    case "gameState":
      return {
        type: "initialState",
        playerColor: msg.playerColor,
        boardRep: mapBoard(msg.board),
        currentPlayer: msg.turn,
        moves: mapMoves(msg.validMoves),
        moveHistory: mapHistory(msg.history),
        gameOver: mapGameOver(msg.status),
        clock: msg.clock ?? null,
      };
    case "moveMade":
      return {
        type: "moveMade",
        move: mapApiMove(msg.move),
        boardRep: mapBoard(msg.board),
        currentPlayer: msg.turn,
        moves: mapMoves(msg.validMoves),
        clock: msg.clock ?? null,
      };
    case "gameOver":
      if (msg.status.state === "finished") {
        return {
          type: "gameOver",
          winner: msg.status.winner,
          reason: msg.status.reason,
          clock: msg.clock ?? null,
        };
      }
      return {
        type: "gameOver",
        winner: "draw",
        reason: "unknown",
        clock: msg.clock ?? null,
      };
    case "drawOffered":
      return { type: "drawOffer", by: msg.by };
    case "drawDeclined":
      return { type: "drawDeclined" };
    case "drawCancelled":
      return { type: "drawCancelled" };
    case "undoRequested":
      return { type: "undoRequest", by: msg.by };
    case "undoAccepted":
      return {
        type: "undoAccepted",
        moveCount: msg.moveCount,
        boardRep: mapBoard(msg.board),
        currentPlayer: msg.turn,
        moves: mapMoves(msg.validMoves),
        clock: msg.clock ?? null,
      };
    case "undoDeclined":
      return { type: "undoDeclined" };
    case "undoCancelled":
      return { type: "undoCancelled" };
    case "opponentJoined":
      return { type: "opponentJoined" };
    case "opponentLeft":
      return { type: "opponentLeft" };
    case "clockUpdated":
      return {
        type: "clockUpdated",
        clock: {
          whiteMs: msg.whiteMs,
          blackMs: msg.blackMs,
          turnStartedAtMs: msg.turnStartedAtMs,
        },
      };
  }
}

export function createOnlineGameService(opts?: {
  onError?: (error: ServerError) => void;
}): OnlineGameService {
  const [events, setEvents] = createSignal<OnlineGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);
  const [connecting, setConnecting] = createSignal(false);

  const ws = createGameWebSocket<OnlineServerMessage>({
    url: "/online/ws",
    onMessage: (msg) => setEvents(mapServerMessage(msg)),
    onConnected: () => {
      setConnected(true);
      setConnecting(false);
    },
    onDisconnected: () => setConnected(false),
    onConnecting: () => setConnecting(true),
    onError: opts?.onError,
  });

  return {
    async createGame(opts) {
      const { data, error } = await api.POST("/online", {
        body: { timeControl: opts.timeControl ?? undefined },
      });
      if (error || !data) throw new Error("Failed to create online game");
      const playerToken =
        opts.creatorColor === "white" ? data.whiteToken : data.blackToken;
      const inviteToken =
        opts.creatorColor === "white" ? data.blackToken : data.whiteToken;
      return { playerToken, inviteToken };
    },

    connect(token) {
      ws.open(token);
    },

    disconnect() {
      ws.close();
    },

    sendMove(move) {
      ws.send({ type: "move", from: move.from, to: move.to });
    },

    resign() {
      ws.send({ type: "resign" });
    },

    offerDraw() {
      ws.send({ type: "offerDraw" });
    },

    acceptDraw() {
      ws.send({ type: "acceptDraw" });
    },

    declineDraw() {
      ws.send({ type: "declineDraw" });
    },

    requestUndo() {
      ws.send({ type: "requestUndo" });
    },

    acceptUndo() {
      ws.send({ type: "acceptUndo" });
    },

    declineUndo() {
      ws.send({ type: "declineUndo" });
    },

    sendChat() {
      // Chat not yet supported by backend
    },

    events,
    connected,
    connecting,
  };
}
