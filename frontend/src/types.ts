export type PlayerColor = "black" | "white";

export interface BoardRep {
  black: Set<number>;
  white: Set<number>;
  king: number;
}

export interface Move {
  from: number;
  to: number;
  captures?: number[];
}

export type MovesMap = Record<number, { to: number; captures: number[] }[]>;

export interface GameOverState {
  winner: PlayerColor | "draw";
  reason: string;
}
