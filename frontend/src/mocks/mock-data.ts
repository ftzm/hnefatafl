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
  {
    id: "game-5",
    opponent: "Ragnar",
    mode: "online",
    yourColor: "white",
    moveCount: 22,
    lastPlayed: "5 min ago",
    status: "Your move",
    isYourTurn: true,
  },
  {
    id: "game-6",
    opponent: "AI",
    opponentDetail: "hard",
    mode: "ai",
    yourColor: "black",
    moveCount: 31,
    lastPlayed: "3 hr ago",
    status: "Waiting",
    isYourTurn: false,
  },
  {
    id: "game-7",
    opponent: "Sigrid",
    mode: "online",
    yourColor: "black",
    moveCount: 6,
    lastPlayed: "Yesterday",
    status: "Your move",
    isYourTurn: true,
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
  {
    id: "game-8",
    opponent: "Bjorn",
    mode: "online",
    yourColor: "white",
    result: "White wins",
    resultDetail: "King escaped",
    moveCount: 28,
    completedAt: "4 days ago",
    isWin: true,
  },
  {
    id: "game-9",
    opponent: "AI",
    opponentDetail: "easy",
    mode: "ai",
    yourColor: "black",
    result: "Black wins",
    resultDetail: "King surrounded",
    moveCount: 19,
    completedAt: "5 days ago",
    isWin: true,
  },
  {
    id: "game-10",
    opponent: "Freya",
    mode: "online",
    yourColor: "black",
    result: "White wins",
    resultDetail: "King escaped",
    moveCount: 41,
    completedAt: "1 week ago",
    isWin: false,
  },
  {
    id: "game-11",
    opponent: "Guest",
    mode: "hotseat",
    yourColor: "black",
    result: "Black wins",
    resultDetail: "King surrounded",
    moveCount: 55,
    completedAt: "2 weeks ago",
    isWin: true,
  },
];
