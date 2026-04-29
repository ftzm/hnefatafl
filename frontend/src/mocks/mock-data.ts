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
    lastPlayed: "2m",
    status: "your turn",
    isYourTurn: true,
  },
  {
    id: "game-2",
    opponent: "AI",
    opponentDetail: "medium",
    mode: "ai",
    yourColor: "white",
    moveCount: 8,
    lastPlayed: "1h",
    status: "waiting",
    isYourTurn: false,
  },
  {
    id: "game-5",
    opponent: "Ragnar",
    mode: "online",
    yourColor: "white",
    moveCount: 22,
    lastPlayed: "5m",
    status: "your turn",
    isYourTurn: true,
  },
  {
    id: "game-6",
    opponent: "AI",
    opponentDetail: "hard",
    mode: "ai",
    yourColor: "black",
    moveCount: 31,
    lastPlayed: "3h",
    status: "waiting",
    isYourTurn: false,
  },
  {
    id: "game-7",
    opponent: "Sigrid",
    mode: "online",
    yourColor: "black",
    moveCount: 6,
    lastPlayed: "1d",
    status: "your turn",
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
    completedAt: "1d",
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
    completedAt: "3d",
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
    completedAt: "4d",
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
    completedAt: "5d",
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
    completedAt: "1w",
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
    completedAt: "2w",
    isWin: true,
  },
];
