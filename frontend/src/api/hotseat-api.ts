import type { Move, PlayerColor } from "../board-logic";
import { api } from "./client";
import { mapBoard, mapGameOver, mapHistory, mapMoves } from "./mappers";
import type { HotseatActionResult, HotseatState } from "./types";

export interface HotseatApi {
  createGame(): Promise<string>;
  getState(gameId: string): Promise<HotseatState>;
  sendMove(gameId: string, move: Move): Promise<HotseatActionResult>;
  undo(gameId: string): Promise<HotseatActionResult>;
  resign(gameId: string, color: PlayerColor): Promise<HotseatActionResult>;
  draw(gameId: string): Promise<HotseatActionResult>;
}

export function createHotseatApi(): HotseatApi {
  return {
    async createGame() {
      const { data, error } = await api.POST("/hotseat");
      if (error || !data) throw new Error("Failed to create hotseat game");
      return data.gameId;
    },

    async getState(gameId) {
      const { data, error } = await api.GET("/hotseat/{id}", {
        params: { path: { id: gameId } },
      });
      if (error || !data) throw new Error("Failed to get hotseat game state");
      return {
        boardRep: mapBoard(data.board),
        currentPlayer: data.turn,
        moves: mapMoves(data.validMoves),
        moveHistory: mapHistory(data.history),
        gameOver: mapGameOver(data.status),
      } satisfies HotseatState;
    },

    async sendMove(gameId, move) {
      const { data, error } = await api.POST("/hotseat/{id}/move", {
        params: { path: { id: gameId } },
        body: { from: move.from, to: move.to, captures: move.captures ?? [] },
      });
      if (error || !data) throw new Error("Failed to send move");
      return {
        moves: mapMoves(data.validMoves),
        gameOver: mapGameOver(data.status),
      } satisfies HotseatActionResult;
    },

    async undo(gameId) {
      const { data, error } = await api.POST("/hotseat/{id}/undo", {
        params: { path: { id: gameId } },
      });
      if (error || !data) throw new Error("Failed to undo");
      return {
        moves: mapMoves(data.validMoves),
        gameOver: mapGameOver(data.status),
      } satisfies HotseatActionResult;
    },

    async resign(gameId, color) {
      const { data, error } = await api.POST("/hotseat/{id}/resign", {
        params: { path: { id: gameId } },
        body: color,
      });
      if (error || !data) throw new Error("Failed to resign");
      return {
        moves: mapMoves(data.validMoves),
        gameOver: mapGameOver(data.status),
      } satisfies HotseatActionResult;
    },

    async draw(gameId) {
      const { data, error } = await api.POST("/hotseat/{id}/draw", {
        params: { path: { id: gameId } },
      });
      if (error || !data) throw new Error("Failed to draw");
      return {
        moves: mapMoves(data.validMoves),
        gameOver: mapGameOver(data.status),
      } satisfies HotseatActionResult;
    },
  };
}
