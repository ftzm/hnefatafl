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
import type { OnlineGameEvent } from "./types";
import { createGameWebSocket } from "./ws-utils";

type OnlineServerMessage = components["schemas"]["OnlineServerMessage"];

export interface OnlineGameService {
  createGame(opts: {
    creatorColor: PlayerColor;
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
  events: Accessor<OnlineGameEvent | undefined>;
  connected: Accessor<boolean>;
}

function mapServerMessage(msg: OnlineServerMessage): OnlineGameEvent {
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
    case "drawOffered":
      return { type: "drawOffer" };
    case "drawDeclined":
      return { type: "drawDeclined" };
    case "undoRequested":
      return { type: "undoRequest" };
    case "undoAccepted":
      return { type: "undoAccepted", moveCount: msg.moveCount };
    case "undoDeclined":
      return { type: "undoDeclined" };
    case "opponentJoined":
      return { type: "opponentJoined" };
    case "opponentLeft":
      return { type: "opponentLeft" };
  }
}

export function createOnlineGameService(): OnlineGameService {
  const [events, setEvents] = createSignal<OnlineGameEvent | undefined>();
  const [connected, setConnected] = createSignal(false);

  const ws = createGameWebSocket<OnlineServerMessage>({
    url: "/online/ws",
    onMessage: (msg) => setEvents(mapServerMessage(msg)),
    onConnected: () => setConnected(true),
    onDisconnected: () => setConnected(false),
  });

  return {
    async createGame(opts) {
      const { data, error } = await api.POST("/online");
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
  };
}
