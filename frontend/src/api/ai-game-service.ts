import type { Accessor } from "solid-js";
import { createSignal } from "solid-js";
import type { Move, PlayerColor } from "../board-logic";
import { api } from "./client";
import type { components } from "./generated/rest";
import {
  mapApiMove,
  mapBoard,
  mapGameOver,
  mapHistory,
  mapMoves,
} from "./mappers";
import type { AiGameEvent } from "./types";
import { createGameWebSocket } from "./ws-utils";

type AIServerMessage = components["schemas"]["AIServerMessage"];

export interface AiGameService {
  createGame(opts: { playerColor: PlayerColor }): Promise<{ token: string }>;
  connect(token: string): void;
  disconnect(): void;
  sendMove(move: Move): void;
  resign(): void;
  events: Accessor<AiGameEvent | undefined>;
  connected: Accessor<boolean>;
}

function mapServerMessage(msg: AIServerMessage): AiGameEvent {
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
      };
    case "moveMade":
      return {
        type: "moveMade",
        move: mapApiMove(msg.move),
        boardRep: mapBoard(msg.board),
        currentPlayer: msg.turn,
        moves: mapMoves(msg.validMoves),
      };
    case "gameOver":
      if (msg.status.state === "finished") {
        return {
          type: "gameOver",
          winner: msg.status.winner,
          reason: msg.status.reason,
        };
      }
      return {
        type: "gameOver",
        winner: "draw",
        reason: "unknown",
      };
    case "undoAccepted":
      return {
        type: "initialState",
        playerColor: "black",
        boardRep: mapBoard(msg.board),
        currentPlayer: msg.turn,
        moves: mapMoves(msg.validMoves),
        moveHistory: [],
        gameOver: mapGameOver(msg.status),
      };
  }
}

export function createAiGameService(): AiGameService {
  const [events, setEvents] = createSignal<AiGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);

  const ws = createGameWebSocket<AIServerMessage>({
    url: "/ai/ws",
    onMessage: (msg) => setEvents(mapServerMessage(msg)),
    onConnected: () => setConnected(true),
    onDisconnected: () => setConnected(false),
  });

  return {
    async createGame(opts) {
      const { data, error } = await api.POST("/ai", {
        body: { playerColor: opts.playerColor },
      });
      if (error || !data) throw new Error("Failed to create AI game");
      return { token: data.token };
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

    events,
    connected,
  };
}
