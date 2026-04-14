import type { BoardRep, GameOverState, Move, MovesMap } from "../board-logic";
import type { components } from "./generated/rest";

type ApiBoard = components["schemas"]["ApiBoard"];
type ApiMove = components["schemas"]["ApiMove"];
type ApiGameStatus = components["schemas"]["ApiGameStatus"];
type ValidMovesMap = components["schemas"]["ValidMovesMap"];
type HistoryEntry = components["schemas"]["HistoryEntry"];

export function mapBoard(wire: ApiBoard): BoardRep {
  return {
    black: new Set(wire.black),
    white: new Set(wire.white),
    king: wire.king,
  };
}

export function mapApiMove(wire: ApiMove): Move {
  return {
    from: wire.from,
    to: wire.to,
    captures: wire.captures.length > 0 ? wire.captures : undefined,
  };
}

export function mapMoves(wire: ValidMovesMap): MovesMap {
  const result: MovesMap = {};
  for (const [key, destinations] of Object.entries(wire)) {
    result[Number(key)] = destinations;
  }
  return result;
}

export function mapGameOver(wire: ApiGameStatus): GameOverState | null {
  if (wire.state === "ongoing") return null;
  return { winner: wire.winner, reason: wire.reason };
}

export function mapHistory(wire: HistoryEntry[]): Move[] {
  return wire.map((entry) => mapApiMove(entry.move));
}
