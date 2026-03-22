import type { Move } from "../board-logic";
import type { HotseatResponse } from "./types";

export interface HotseatApi {
  getGameState(moves: Move[]): Promise<HotseatResponse>;
}
