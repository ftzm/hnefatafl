export interface InProgressGame {
  id: string;
  opponent: string;
  mode: string;
  yourColor: string;
  moveCount: number;
  lastPlayed: string;
  status: string;
}

export interface CompletedGame {
  id: string;
  opponent: string;
  mode: string;
  yourColor: string;
  result: string;
  resultDetail: string;
  moveCount: number;
  completedAt: string;
}

export const mockInProgressGames: InProgressGame[] = [
  {
    id: "game-1",
    opponent: "Guest",
    mode: "hotseat",
    yourColor: "black",
    moveCount: 14,
    lastPlayed: "2 min ago",
    status: "Your turn",
  },
  {
    id: "game-2",
    opponent: "AI (Easy)",
    mode: "ai",
    yourColor: "white",
    moveCount: 8,
    lastPlayed: "1 hour ago",
    status: "AI thinking",
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
  },
  {
    id: "game-4",
    opponent: "AI (Medium)",
    mode: "ai",
    yourColor: "black",
    result: "Black wins",
    resultDetail: "King surrounded",
    moveCount: 47,
    completedAt: "3 days ago",
  },
];
