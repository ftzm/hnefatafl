/**
 * Board representation and move transformation logic
 */

export const startBoard = {
	black: new Set([
		3, 4, 5, 6, 7, 16, 33, 44, 55, 66, 77, 43, 54, 65, 76, 87, 56, 64, 104, 113,
		114, 115, 116, 117,
	]),
	white: new Set([38, 48, 49, 50, 58, 59, 61, 62, 70, 71, 72, 82]),
	king: 60,
};

function findKeyByValue(map, value) {
	for (const [key, val] of map.entries()) {
		if (val === value) {
			return key;
		}
	}
	return null;
}

function filterMap(map, predicate) {
	return new Map(
		[...map.entries()].filter(([key, value]) => predicate(key, value))
	);
}

export function reduceMovesToTransformation(moves) {
	const result = moves.reduce((acc, move) => {
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

export function reverseMovesToTransformation(moves, startColor) {
	let currentColor = startColor;

	const result = moves.reduceRight((acc, move) => {
		const capturedColor = currentColor === "black" ? "white" : "black";

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

export function generateMockMovesForColor(boardRep, color) {
	const oppositeColor = color === "black" ? "white" : "black";
	const moves = {};

	const emptySquares = [];
	const occupiedSquares = new Set([...boardRep.black, ...boardRep.white, boardRep.king]);
	for (let i = 0; i < 121; i++) {
		if (!occupiedSquares.has(i)) {
			emptySquares.push(i);
		}
	}

	const oppositeColorSquares = oppositeColor === "black"
		? [...boardRep.black]
		: [...boardRep.white, boardRep.king];

	const colorSquares = color === "black" ? [...boardRep.black] : [...boardRep.white, boardRep.king];

	colorSquares.forEach(pieceIndex => {
		const pieceMoves = [];
		const numMoves = Math.min(10, emptySquares.length);
		const usedSquares = new Set();

		for (let j = 0; j < numMoves; j++) {
			let destination;
			do {
				destination = emptySquares[Math.floor(Math.random() * emptySquares.length)];
			} while (
				usedSquares.has(destination) &&
				usedSquares.size < emptySquares.length
			);

			if (usedSquares.has(destination)) break;
			usedSquares.add(destination);

			if (oppositeColorSquares.length > 0) {
				const captureSquare = oppositeColorSquares[
					Math.floor(Math.random() * oppositeColorSquares.length)
				];
				pieceMoves.push([destination, captureSquare]);
			}
		}

		if (pieceMoves.length > 0) {
			moves[pieceIndex] = pieceMoves;
		}
	});

	return moves;
}

/**
 * Clones a BoardRep (deep copy of Sets)
 */
export function cloneBoardRep(boardRep) {
	return {
		black: new Set(boardRep.black),
		white: new Set(boardRep.white),
		king: boardRep.king,
	};
}

/**
 * Apply a single move to a BoardRep, returning a new BoardRep
 */
export function applyMoveToBoardRep(boardRep, move) {
	const newBoard = cloneBoardRep(boardRep);

	// Move the piece
	if (newBoard.black.has(move.from)) {
		newBoard.black.delete(move.from);
		newBoard.black.add(move.to);
	} else if (newBoard.white.has(move.from)) {
		newBoard.white.delete(move.from);
		newBoard.white.add(move.to);
	} else if (newBoard.king === move.from) {
		newBoard.king = move.to;
	}

	// Apply captures
	if (move.captures) {
		for (const cap of move.captures) {
			newBoard.black.delete(cap);
			newBoard.white.delete(cap);
			if (newBoard.king === cap) newBoard.king = -1;
		}
	}

	return newBoard;
}

/**
 * Compute board state at a given move index by replaying from start
 */
export function computeBoardAtMove(moveHistory, moveIndex) {
	let board = cloneBoardRep(startBoard);
	for (let i = 0; i <= moveIndex; i++) {
		board = applyMoveToBoardRep(board, moveHistory[i]);
	}
	return board;
}

/**
 * Converts square index to algebraic notation for 11x11 board
 */
export function indexToAlgebraic(index) {
	const file = String.fromCharCode(97 + (index % 11)); // a-k
	const rank = 11 - Math.floor(index / 11); // 11-1
	return file + rank;
}
