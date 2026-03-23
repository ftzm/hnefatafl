import {
  applyMoveToBoardRep,
  type BoardRep,
  cloneBoardRep,
  type GameOverState,
  type Move,
  type MovesMap,
  type PlayerColor,
  startBoard,
} from "../board-logic";

const CORNERS = new Set([0, 10, 110, 120]);
const THRONE = 60;
const DIRECTIONS = [-11, 11, -1, 1];

function canStep(from: number, dir: number): boolean {
  if (dir === -1) return from % 11 > 0;
  if (dir === 1) return from % 11 < 10;
  if (dir === -11) return from >= 11;
  if (dir === 11) return from <= 109;
  return false;
}

function isEnemyAt(
  board: BoardRep,
  pos: number,
  movingColor: PlayerColor,
): boolean {
  if (movingColor === "black") {
    return board.white.has(pos) || pos === board.king;
  }
  return board.black.has(pos);
}

function isFriendlyAt(
  board: BoardRep,
  pos: number,
  movingColor: PlayerColor,
): boolean {
  if (movingColor === "black") {
    return board.black.has(pos);
  }
  return board.white.has(pos) || pos === board.king;
}

function isHostile(
  board: BoardRep,
  pos: number,
  movingColor: PlayerColor,
): boolean {
  if (CORNERS.has(pos)) return true;
  if (pos === THRONE && board.king !== THRONE) return true;
  return isFriendlyAt(board, pos, movingColor);
}

function isHostileToKing(board: BoardRep, pos: number): boolean {
  return (
    board.black.has(pos) ||
    CORNERS.has(pos) ||
    (pos === THRONE && board.king !== THRONE)
  );
}

function movePiece(board: BoardRep, from: number, to: number): BoardRep {
  const b = cloneBoardRep(board);
  if (b.black.has(from)) {
    b.black.delete(from);
    b.black.add(to);
  } else if (b.white.has(from)) {
    b.white.delete(from);
    b.white.add(to);
  } else if (b.king === from) {
    b.king = to;
  }
  return b;
}

function checkKingCapture(board: BoardRep): boolean {
  const kp = board.king;
  if (kp < 0) return false;

  if (kp === THRONE) {
    return DIRECTIONS.every(
      (dir) => canStep(kp, dir) && board.black.has(kp + dir),
    );
  }

  const nextToThrone = DIRECTIONS.some(
    (dir) => canStep(kp, dir) && kp + dir === THRONE,
  );

  if (nextToThrone) {
    return DIRECTIONS.every((dir) => {
      if (!canStep(kp, dir)) return false;
      const adj = kp + dir;
      return adj === THRONE || board.black.has(adj);
    });
  }

  // Standard sandwich on either axis
  if (canStep(kp, -11) && canStep(kp, 11)) {
    if (isHostileToKing(board, kp - 11) && isHostileToKing(board, kp + 11)) {
      return true;
    }
  }
  if (canStep(kp, -1) && canStep(kp, 1)) {
    if (isHostileToKing(board, kp - 1) && isHostileToKing(board, kp + 1)) {
      return true;
    }
  }
  return false;
}

export function computeCaptures(
  board: BoardRep,
  from: number,
  to: number,
  movingColor: PlayerColor,
): number[] {
  const tempBoard = movePiece(board, from, to);
  const captures: number[] = [];

  for (const dir of DIRECTIONS) {
    if (!canStep(to, dir)) continue;
    const adj = to + dir;

    if (!isEnemyAt(tempBoard, adj, movingColor)) continue;

    // King captured via separate multi-directional check
    if (adj === tempBoard.king) continue;

    // Standard sandwich
    if (!canStep(adj, dir)) continue;
    const beyond = adj + dir;
    if (isHostile(tempBoard, beyond, movingColor)) {
      captures.push(adj);
    }
  }

  // King capture (only black can capture the king)
  if (movingColor === "black" && tempBoard.king >= 0) {
    if (checkKingCapture(tempBoard)) {
      captures.push(tempBoard.king);
    }
  }

  return captures;
}

export function generateLegalMoves(
  board: BoardRep,
  color: PlayerColor,
): MovesMap {
  const moves: MovesMap = {};
  const pieces: number[] =
    color === "black"
      ? [...board.black]
      : [...board.white, board.king].filter((p) => p >= 0);
  const occupied = new Set([...board.black, ...board.white]);
  if (board.king >= 0) occupied.add(board.king);

  for (const piece of pieces) {
    const isKing = piece === board.king;
    const pieceMoves: number[][] = [];

    for (const dir of DIRECTIONS) {
      let current = piece;
      while (canStep(current, dir)) {
        const next = current + dir;

        if (occupied.has(next)) break;

        if (CORNERS.has(next)) {
          if (isKing) {
            pieceMoves.push([next]);
          }
          break;
        }

        if (next === THRONE && !isKing) {
          current = next;
          continue;
        }

        const captures = computeCaptures(board, piece, next, color);
        pieceMoves.push([next, ...captures]);
        current = next;
      }
    }

    if (pieceMoves.length > 0) {
      moves[piece] = pieceMoves;
    }
  }

  return moves;
}

export function checkGameOver(
  board: BoardRep,
  currentPlayer: PlayerColor,
  moves: MovesMap,
): GameOverState | null {
  if (CORNERS.has(board.king)) {
    return { winner: "white", reason: "King escaped" };
  }
  if (board.king === -1) {
    return { winner: "black", reason: "King captured" };
  }
  if (Object.keys(moves).length === 0) {
    const winner = currentPlayer === "black" ? "white" : "black";
    return { winner, reason: "No legal moves" };
  }
  return null;
}

export function replayMoves(moves: Move[]): {
  board: BoardRep;
  currentPlayer: PlayerColor;
} {
  let board = cloneBoardRep(startBoard);
  let currentPlayer: PlayerColor = "black";
  for (const move of moves) {
    board = applyMoveToBoardRep(board, move);
    currentPlayer = currentPlayer === "black" ? "white" : "black";
  }
  return { board, currentPlayer };
}

export function pickRandomMove(moves: MovesMap): Move | null {
  const pieces = Object.keys(moves).map(Number);
  if (pieces.length === 0) return null;
  const piece = pieces[Math.floor(Math.random() * pieces.length)];
  const dests = moves[piece];
  const dest = dests[Math.floor(Math.random() * dests.length)];
  const [to, ...captures] = dest;
  return {
    from: piece,
    to,
    captures: captures.length > 0 ? captures : undefined,
  };
}
