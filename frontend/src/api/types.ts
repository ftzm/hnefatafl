import type {
  BoardRep,
  GameOverState,
  Move,
  MovesMap,
  PlayerColor,
} from "../board-logic";

export interface HotseatState {
  boardRep: BoardRep;
  currentPlayer: PlayerColor;
  moves: MovesMap;
  moveHistory: Move[];
  gameOver: GameOverState | null;
}

export interface HotseatActionResult {
  moves: MovesMap;
  gameOver: GameOverState | null;
}

export type AiGameEvent =
  | {
      type: "initialState";
      playerColor: PlayerColor;
      boardRep: BoardRep;
      currentPlayer: PlayerColor;
      moves: MovesMap;
      moveHistory: Move[];
      gameOver: GameOverState | null;
    }
  | {
      type: "moveMade";
      move: Move;
      boardRep: BoardRep;
      currentPlayer: PlayerColor;
      moves: MovesMap;
    }
  | { type: "gameOver"; winner: PlayerColor | "draw"; reason: string };

export type OnlineGameEvent =
  | {
      type: "initialState";
      playerColor: PlayerColor;
      boardRep: BoardRep;
      currentPlayer: PlayerColor;
      moves: MovesMap;
      moveHistory: Move[];
      gameOver: GameOverState | null;
    }
  | {
      type: "moveMade";
      move: Move;
      boardRep: BoardRep;
      currentPlayer: PlayerColor;
      moves: MovesMap;
    }
  | { type: "gameOver"; winner: PlayerColor | "draw"; reason: string }
  | { type: "opponentJoined" }
  | { type: "opponentLeft" }
  | { type: "drawOffer" }
  | { type: "drawDeclined" }
  | { type: "undoRequest" }
  | { type: "undoAccepted"; moveCount: number }
  | { type: "undoDeclined" }
  | { type: "chat"; message: string };
