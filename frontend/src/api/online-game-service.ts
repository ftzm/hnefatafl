import type { Accessor } from "solid-js";
import type { Move, PlayerColor } from "../board-logic";
import type { OnlineGameEvent } from "./types";

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
