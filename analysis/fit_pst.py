#!/usr/bin/env python3
"""
Fit piece-square table values via logistic regression.

For each position sampled from the game database, builds an 87-element feature
vector (29 quarter-symmetric indices × 3 piece types: black, white, king) and
fits P(white_wins) = sigmoid(Σ w·x + bias).

The learned weights are directly usable as PST values in the engine.

Usage:
    python fit_pst.py <path-to-db.db> [--samples-per-game N] [--min-move M] [--max-move M] [--C regularization]
"""

import argparse
import sqlite3
import sys
import random
import numpy as np

# ---------------------------------------------------------------------------
# Quarter-symmetry mapping: 121 squares → 29 canonical indices
# ---------------------------------------------------------------------------

# The 29 canonical quarter-board positions (from score.c)
QUARTER_INDICES = [
    1, 2, 3, 4, 5, 11, 12, 13, 14, 15, 16, 22, 23, 24, 25, 26, 27,
    33, 34, 35, 36, 37, 38, 44, 45, 46, 47, 48, 49,
]

# rotate_right[121] from layer.c
ROTATE_RIGHT = [
    10, 21, 32, 43, 54, 65, 76, 87, 98, 109, 120,
    9, 20, 31, 42, 53, 64, 75, 86, 97, 108, 119,
    8, 19, 30, 41, 52, 63, 74, 85, 96, 107, 118,
    7, 18, 29, 40, 51, 62, 73, 84, 95, 106, 117,
    6, 17, 28, 39, 50, 61, 72, 83, 94, 105, 116,
    5, 16, 27, 38, 49, 60, 71, 82, 93, 104, 115,
    4, 15, 26, 37, 48, 59, 70, 81, 92, 103, 114,
    3, 14, 25, 36, 47, 58, 69, 80, 91, 102, 113,
    2, 13, 24, 35, 46, 57, 68, 79, 90, 101, 112,
    1, 12, 23, 34, 45, 56, 67, 78, 89, 100, 111,
    0, 11, 22, 33, 44, 55, 66, 77, 88, 99, 110,
]


def build_square_to_quarter():
    """Build a mapping from each of 121 squares to its quarter index (0..28).

    Squares not covered by any quarter rotation (e.g. center files/ranks that
    overlap with the center or position 0/corners) map to -1.
    """
    sq_to_q = [-1] * 121
    for qi, base_sq in enumerate(QUARTER_INDICES):
        sq = base_sq
        for _ in range(4):
            sq_to_q[sq] = qi
            sq = ROTATE_RIGHT[sq]
    return sq_to_q


SQ_TO_QUARTER = build_square_to_quarter()

# Number of features: 29 quarter positions × 3 piece types
N_QUARTER = 29
N_FEATURES = N_QUARTER * 3  # 87


# ---------------------------------------------------------------------------
# Board decoding
# ---------------------------------------------------------------------------

def layer_squares(lower: int, upper: int):
    """Yield square indices where bits are set in a 121-bit layer."""
    # Lower 64 bits → squares 0..63
    bits = lower & 0xFFFFFFFFFFFFFFFF
    while bits:
        sq = bits & (-bits)  # lowest set bit
        yield sq.bit_length() - 1
        bits ^= sq
    # Upper 57 bits → squares 64..120
    bits = upper & 0x01FFFFFFFFFFFFFF
    while bits:
        sq = bits & (-bits)
        yield 64 + sq.bit_length() - 1
        bits ^= sq


def position_to_features(black_lower, black_upper, white_lower, white_upper, king_pos):
    """Convert a board state to an 87-element feature vector."""
    features = np.zeros(N_FEATURES, dtype=np.float64)

    for sq in layer_squares(black_lower, black_upper):
        qi = SQ_TO_QUARTER[sq]
        if qi >= 0:
            features[qi] += 1  # black offset = 0

    for sq in layer_squares(white_lower, white_upper):
        qi = SQ_TO_QUARTER[sq]
        if qi >= 0:
            features[N_QUARTER + qi] += 1  # white offset = 29

    qi = SQ_TO_QUARTER[king_pos]
    if qi >= 0:
        features[2 * N_QUARTER + qi] = 1  # king offset = 58

    return features


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_data(db_path, samples_per_game, min_move, max_move, seed):
    """Load (features, labels) from the database."""
    random.seed(seed)
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()

    # Get finished games with outcomes
    cur.execute("""
        SELECT id, game_status FROM game
        WHERE game_status IS NOT NULL
          AND game_status != 'ongoing'
          AND game_status != 'abandoned'
          AND game_status != 'draw'
    """)
    games = cur.fetchall()

    X_rows = []
    y_rows = []

    for game_id, status in games:
        if status.startswith("white_won"):
            label = 1
        elif status.startswith("black_won"):
            label = 0
        else:
            continue

        # Get total move count for this game
        cur.execute(
            "SELECT MAX(move_number) FROM move WHERE game_id = ?", (game_id,)
        )
        row = cur.fetchone()
        if row is None or row[0] is None:
            continue
        max_move_num = row[0]

        # Determine which moves to sample
        lo = min(min_move, max_move_num)
        hi = min(max_move, max_move_num)
        if hi < lo:
            lo = hi
        available = list(range(lo, hi + 1))
        n_samples = min(samples_per_game, len(available))
        if n_samples == 0:
            continue
        sampled_moves = random.sample(available, n_samples)

        for move_num in sampled_moves:
            cur.execute(
                """SELECT black_lower, black_upper, white_lower, white_upper, king
                   FROM move WHERE game_id = ? AND move_number = ?""",
                (game_id, move_num),
            )
            mrow = cur.fetchone()
            if mrow is None:
                continue
            bl, bu, wl, wu, king = mrow
            features = position_to_features(bl, bu, wl, wu, king)
            X_rows.append(features)
            y_rows.append(label)

    conn.close()
    return np.array(X_rows), np.array(y_rows)


# ---------------------------------------------------------------------------
# Output formatting
# ---------------------------------------------------------------------------

def print_quarter_table(name, values):
    """Print a quarter table as a C array initializer."""
    print(f"u32 {name}[29] = {{")
    for i, idx in enumerate(QUARTER_INDICES):
        comma = "," if i < 28 else ""
        print(f"    {int(values[i]):>6}{comma}  // {idx}")
    print("};")
    print()


def print_board_heatmap(name, values):
    """Print a visual 11x11 heatmap of PST values."""
    # Expand quarter values to full board
    full = [0] * 121
    for qi, base_sq in enumerate(QUARTER_INDICES):
        sq = base_sq
        for _ in range(4):
            full[sq] = int(values[qi])
            sq = ROTATE_RIGHT[sq]

    print(f"  {name} heatmap (board view, row 10 = top):")
    print()
    for row in range(10, -1, -1):
        cells = []
        for col in range(10, -1, -1):
            sq = row * 11 + col
            cells.append(f"{full[sq]:>5}")
        print("  " + " ".join(cells))
    print()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Fit PST values via logistic regression")
    parser.add_argument("db", help="Path to SQLite database")
    parser.add_argument("--samples-per-game", type=int, default=3,
                        help="Positions to sample per game (default: 3)")
    parser.add_argument("--min-move", type=int, default=8,
                        help="Earliest move to sample from (default: 8)")
    parser.add_argument("--max-move", type=int, default=60,
                        help="Latest move to sample from (default: 60)")
    parser.add_argument("--C", type=float, default=1.0, dest="reg_C",
                        help="Inverse regularization strength (default: 1.0)")
    parser.add_argument("--scale", type=float, default=100.0,
                        help="Scale factor for output PST values (default: 100)")
    parser.add_argument("--seed", type=int, default=42,
                        help="Random seed (default: 42)")
    args = parser.parse_args()

    print(f"Loading data from {args.db}...")
    X, y = load_data(args.db, args.samples_per_game, args.min_move, args.max_move, args.seed)

    if len(X) == 0:
        print("No positions found. Check that the database has completed games.", file=sys.stderr)
        sys.exit(1)

    n_white = np.sum(y == 1)
    n_black = np.sum(y == 0)
    print(f"Loaded {len(X)} positions ({n_white} white wins, {n_black} black wins)")

    # Fit logistic regression
    try:
        from sklearn.linear_model import LogisticRegression
    except ImportError:
        print("scikit-learn is required: pip install scikit-learn", file=sys.stderr)
        sys.exit(1)

    model = LogisticRegression(
        penalty="l2",
        C=args.reg_C,
        max_iter=2000,
        solver="lbfgs",
    )
    model.fit(X, y)

    # Training accuracy
    acc = model.score(X, y)
    print(f"Training accuracy: {acc:.3f}")
    print(f"Bias (intercept): {model.intercept_[0]:.4f}")
    print()

    # Extract and scale weights
    weights = model.coef_[0]  # shape (87,)
    black_w = weights[0:N_QUARTER] * args.scale
    white_w = weights[N_QUARTER:2*N_QUARTER] * args.scale
    king_w = weights[2*N_QUARTER:3*N_QUARTER] * args.scale

    # White-perspective PST: positive = good for white
    # Logistic regression learned P(white_wins), so positive weights favor white.
    # For black pieces, a positive weight means "black piece here helps white",
    # so we negate for black's own PST (black wants to avoid those squares).
    # For white pieces and king, positive weight = good for white, keep as-is.

    print("=" * 60)
    print("LEARNED PST VALUES (white-perspective, scaled)")
    print("Positive = good for white")
    print("=" * 60)
    print()

    print("// Black pawn PST (negated: positive = good for black)")
    print_quarter_table("black_pst_quarter", -black_w)
    print_board_heatmap("Black pawn", -black_w)

    print("// White pawn PST (positive = good for white)")
    print_quarter_table("white_pst_quarter", white_w)
    print_board_heatmap("White pawn", white_w)

    print("// King PST (positive = good for white)")
    print_quarter_table("king_pst_quarter", king_w)
    print_board_heatmap("King", king_w)

    # Also dump raw log-odds weights for analysis
    print("=" * 60)
    print("RAW LOG-ODDS WEIGHTS (unscaled)")
    print("=" * 60)
    print()
    for i, idx in enumerate(QUARTER_INDICES):
        print(f"  sq {idx:>3}:  black={weights[i]:>+.4f}  "
              f"white={weights[N_QUARTER+i]:>+.4f}  "
              f"king={weights[2*N_QUARTER+i]:>+.4f}")


if __name__ == "__main__":
    main()
