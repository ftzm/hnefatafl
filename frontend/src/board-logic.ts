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

export type MovesMap = Record<number, number[][]>;

export interface GameOverState {
  winner: PlayerColor | "draw";
  reason: string;
}

export const startBoard: BoardRep = {
	black: new Set([
		3, 4, 5, 6, 7, 16, 33, 44, 55, 66, 77, 43, 54, 65, 76, 87, 56, 64, 104, 113,
		114, 115, 116, 117,
	]),
	white: new Set([38, 48, 49, 50, 58, 59, 61, 62, 70, 71, 72, 82]),
	king: 60,
};

function findKeyByValue(map: Map<number, number>, value: number): number | null {
	for (const [key, val] of map.entries()) {
		if (val === value) {
			return key;
		}
	}
	return null;
}

function filterMap(
	map: Map<number, number>,
	predicate: (key: number, value: number) => boolean,
): Map<number, number> {
	return new Map(
		[...map.entries()].filter(([key, value]) => predicate(key, value))
	);
}

interface Transformation {
	captures: number[];
	pieceTransformations: Map<number, number>;
}

export function reduceMovesToTransformation(moves: Move[]): Transformation {
	const result = moves.reduce<Transformation>((acc, move) => {
		const sortedCaptures = [...(move.captures || [])].sort((a, b) => a - b);
		const capturedOriginalPositions = sortedCaptures.map((capturedPosition) => {
			const originalPosition = findKeyByValue(acc.pieceTransformations, capturedPosition);
			if (originalPosition) {
				acc.pieceTransformations.delete(originalPosition);
				return originalPosition;
			}
			return capturedPosition;
		});
		acc.captures.push(...capturedOriginalPositions);

		const origin = findKeyByValue(acc.pieceTransformations, move.from) || move.from;
		acc.pieceTransformations.set(origin, move.to);

		return acc;
	}, {
		captures: [],
		pieceTransformations: new Map(),
	});

	result.pieceTransformations = filterMap(
		result.pieceTransformations,
		(from, to) => from !== to
	);
	return result;
}

interface Restoration {
	index: number;
	color: PlayerColor;
}

interface ReverseTransformation {
	restorations: Restoration[];
	pieceTransformations: Map<number, number>;
}

export function reverseMovesToTransformation(moves: Move[], startColor: PlayerColor): ReverseTransformation {
	let currentColor: PlayerColor = startColor;

	const result = moves.reduceRight<ReverseTransformation>((acc, move) => {
		const capturedColor: PlayerColor = currentColor === "black" ? "white" : "black";

		const sortedCaptures = [...(move.captures || [])].sort((a, b) => b - a);
		sortedCaptures.forEach((index) => {
			acc.restorations.push({ index, color: capturedColor });
		});

		const restorationIndex = acc.restorations.findIndex((r) => r.index === move.to);
		if (restorationIndex !== -1) {
			acc.restorations[restorationIndex].index = move.from;
		}

		if (!acc.restorations.some((r) => r.index === move.from)) {
			const origin = findKeyByValue(acc.pieceTransformations, move.to) || move.to;
			acc.pieceTransformations.set(origin, move.from);
		}

		currentColor = currentColor === "black" ? "white" : "black";

		return acc;
	}, {
		restorations: [],
		pieceTransformations: new Map(),
	});

	result.pieceTransformations = filterMap(
		result.pieceTransformations,
		(from, to) => from !== to
	);
	return result;
}

export function cloneBoardRep(boardRep: BoardRep): BoardRep {
	return {
		black: new Set(boardRep.black),
		white: new Set(boardRep.white),
		king: boardRep.king,
	};
}

export function applyMoveToBoardRep(boardRep: BoardRep, move: Move): BoardRep {
	const newBoard = cloneBoardRep(boardRep);

	if (newBoard.black.has(move.from)) {
		newBoard.black.delete(move.from);
		newBoard.black.add(move.to);
	} else if (newBoard.white.has(move.from)) {
		newBoard.white.delete(move.from);
		newBoard.white.add(move.to);
	} else if (newBoard.king === move.from) {
		newBoard.king = move.to;
	}

	if (move.captures) {
		for (const cap of move.captures) {
			newBoard.black.delete(cap);
			newBoard.white.delete(cap);
			if (newBoard.king === cap) newBoard.king = -1;
		}
	}

	return newBoard;
}

export function computeBoardAtMove(moveHistory: Move[], moveIndex: number): BoardRep {
	let board = cloneBoardRep(startBoard);
	for (let i = 0; i <= moveIndex; i++) {
		board = applyMoveToBoardRep(board, moveHistory[i]);
	}
	return board;
}

export function indexToAlgebraic(index: number): string {
	const file = String.fromCharCode(97 + (index % 11));
	const rank = 11 - Math.floor(index / 11);
	return file + rank;
}
