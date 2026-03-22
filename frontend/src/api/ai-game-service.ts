import type { Accessor } from "solid-js";
import type { Move, PlayerColor } from "../board-logic";
import type { AiGameEvent } from "./types";

export interface AiGameService {
  createGame(opts: { playerColor: PlayerColor }): Promise<{ token: string }>;
  connect(token: string): void;
  disconnect(): void;
  sendMove(move: Move): void;
  resign(): void;
  events: Accessor<AiGameEvent | undefined>;
  connected: Accessor<boolean>;
}
