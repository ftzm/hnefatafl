export interface InProgressGame {
  id: string;
  opponent: string;
  opponentDetail?: string;
  mode: string;
  yourColor: string;
  moveCount: number;
  lastPlayed: string;
  status: string;
  isYourTurn: boolean;
}

export interface CompletedGame {
  id: string;
  opponent: string;
  opponentDetail?: string;
  mode: string;
  yourColor: string;
  result: string;
  resultDetail: string;
  moveCount: number;
  completedAt: string;
  isWin: boolean;
}

export const mockInProgressGames: InProgressGame[] = [
  {
    id: "game-1",
    opponent: "Guest",
    mode: "hotseat",
    yourColor: "black",
    moveCount: 14,
    lastPlayed: "2 min ago",
    status: "Your move",
    isYourTurn: true,
  },
  {
    id: "game-2",
    opponent: "AI",
    opponentDetail: "medium",
    mode: "ai",
    yourColor: "white",
    moveCount: 8,
    lastPlayed: "1 hr ago",
    status: "Waiting",
    isYourTurn: false,
  },
];

export const mockCompletedGames: CompletedGame[] = [
  {
    id: "game-3",
    opponent: "Guest",
    mode: "hotseat",
    yourColor: "white",
    result: "White wins",
    resultDetail: "King escaped",
    moveCount: 32,
    completedAt: "Yesterday",
    isWin: true,
  },
  {
    id: "game-4",
    opponent: "AI",
    opponentDetail: "hard",
    mode: "ai",
    yourColor: "black",
    result: "Black wins",
    resultDetail: "King surrounded",
    moveCount: 47,
    completedAt: "3 days ago",
    isWin: false,
  },
];
