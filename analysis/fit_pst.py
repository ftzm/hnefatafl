#!/usr/bin/env python3
"""
Fit piece-square table values via logistic regression.

For each position sampled from the game database, builds an 87-element feature
vector (29 quarter-symmetric indices x 3 piece types: black, white, king) and
fits P(white_wins) = sigmoid(sum w*x + bias).

The learned weights are directly usable as PST values in the engine.

Usage:
    python fit_pst.py stats <db>
    python fit_pst.py fit <db> [--stages N] [--samples-per-game N] [--C val] [--scale val]
    python fit_pst.py fit <db> --plot [--output-dir DIR]
"""

import argparse
import os
import sqlite3
import sys
import random
import numpy as np

# ---------------------------------------------------------------------------
# Quarter-symmetry mapping: 121 squares -> 29 canonical indices
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

# Number of features: 29 quarter positions x 3 piece types
N_QUARTER = 29
N_FEATURES = N_QUARTER * 3  # 87

# Corner-adjacent squares: king can escape in one move, so these are effectively won
# Corners are 0, 10, 110, 120; adjacent squares are:
CORNER_ADJACENT = frozenset({
    1, 11,      # adjacent to corner 0
    9, 21,      # adjacent to corner 10
    99, 111,    # adjacent to corner 110
    109, 119,   # adjacent to corner 120
})


# ---------------------------------------------------------------------------
# Board decoding
# ---------------------------------------------------------------------------

def layer_squares(lower: int, upper: int):
    """Yield square indices where bits are set in a 121-bit layer."""
    # Lower 64 bits -> squares 0..63
    bits = lower & 0xFFFFFFFFFFFFFFFF
    while bits:
        sq = bits & (-bits)  # lowest set bit
        yield sq.bit_length() - 1
        bits ^= sq
    # Upper 57 bits -> squares 64..120
    bits = upper & 0x01FFFFFFFFFFFFFF
    while bits:
        sq = bits & (-bits)
        yield 64 + sq.bit_length() - 1
        bits ^= sq


def count_bits(lower: int, upper: int):
    """Count number of set bits in a 121-bit layer."""
    lo = lower & 0xFFFFFFFFFFFFFFFF
    hi = upper & 0x01FFFFFFFFFFFFFF
    return bin(lo).count('1') + bin(hi).count('1')


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

def get_decided_games(cur):
    """Get all games with a definitive outcome (not ongoing/abandoned/draw)."""
    cur.execute("""
        SELECT id, game_status FROM game
        WHERE game_status IS NOT NULL
          AND game_status != 'ongoing'
          AND game_status != 'abandoned'
          AND game_status != 'draw'
    """)
    return cur.fetchall()


def game_label(status):
    """Return 1 for white win, 0 for black win, None otherwise."""
    if status.startswith("white_won"):
        return 1
    elif status.startswith("black_won"):
        return 0
    return None


def load_data(db_path, samples_per_game, min_move, max_move, seed, stages=None):
    """Load (features, labels) from the database.

    If stages is set to N, each game is divided into N equal phases based on
    its own length. Returns a dict mapping stage index -> (X, y).
    If stages is None, returns a single (X, y) tuple.
    """
    random.seed(seed)
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()
    games = get_decided_games(cur)

    if stages:
        stage_data = {i: ([], []) for i in range(stages)}
    else:
        X_rows, y_rows = [], []

    for game_id, status in games:
        label = game_label(status)
        if label is None:
            continue

        cur.execute(
            "SELECT MAX(move_number) FROM move WHERE game_id = ?", (game_id,)
        )
        row = cur.fetchone()
        if row is None or row[0] is None:
            continue
        max_move_num = row[0]

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

            # Skip positions where king is corner-adjacent (effectively already won)
            if king in CORNER_ADJACENT:
                continue

            features = position_to_features(bl, bu, wl, wu, king)

            if stages:
                # Divide game into N equal phases
                # move_num / max_move_num gives progress through the game (0..1)
                progress = move_num / max_move_num if max_move_num > 0 else 0
                stage_idx = min(int(progress * stages), stages - 1)
                stage_data[stage_idx][0].append(features)
                stage_data[stage_idx][1].append(label)
            else:
                X_rows.append(features)
                y_rows.append(label)

    conn.close()

    if stages:
        return {
            i: (np.array(xs) if xs else np.array([]).reshape(0, N_FEATURES),
                np.array(ys) if ys else np.array([]))
            for i, (xs, ys) in stage_data.items()
        }
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


def expand_quarter_to_board(values):
    """Expand 29-element quarter values to full 11x11 board."""
    full = np.zeros(121, dtype=np.float64)
    for qi, base_sq in enumerate(QUARTER_INDICES):
        sq = base_sq
        for _ in range(4):
            full[sq] = values[qi]
            sq = ROTATE_RIGHT[sq]
    return full.reshape(11, 11)


def plot_heatmaps(piece_data, title, output_path, stage_label=None):
    """Generate a graphical heatmap figure for PST values.

    Args:
        piece_data: dict mapping piece name -> 29-element quarter values
        title: Overall title for the figure
        output_path: Path to save the figure
        stage_label: Optional stage label for the filename
    """
    try:
        import matplotlib.pyplot as plt
        import matplotlib.colors as mcolors
    except ImportError:
        print("matplotlib is required for plotting: pip install matplotlib",
              file=sys.stderr)
        return

    n_pieces = len(piece_data)
    fig, axes = plt.subplots(1, n_pieces, figsize=(5 * n_pieces, 5))
    if n_pieces == 1:
        axes = [axes]

    for ax, (piece_name, values) in zip(axes, piece_data.items()):
        board = expand_quarter_to_board(values)
        # Flip so row 10 is at top (matching print_board_heatmap)
        board = np.flipud(board)

        # Use diverging colormap centered at 0
        vmax = max(abs(board.min()), abs(board.max()))
        vmin = -vmax if vmax > 0 else -1

        im = ax.imshow(board, cmap='RdYlGn', vmin=vmin, vmax=vmax,
                       aspect='equal')

        # Add value annotations
        for i in range(11):
            for j in range(11):
                val = board[i, j]
                # Choose text color based on background brightness
                text_color = 'white' if abs(val) > vmax * 0.6 else 'black'
                ax.text(j, i, f'{int(val)}', ha='center', va='center',
                        fontsize=7, color=text_color)

        ax.set_title(piece_name, fontsize=12, fontweight='bold')
        ax.set_xticks(range(11))
        ax.set_yticks(range(11))
        ax.set_xticklabels([str(10-i) for i in range(11)], fontsize=8)
        ax.set_yticklabels([str(10-i) for i in range(11)], fontsize=8)

        # Add colorbar
        cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
        cbar.ax.tick_params(labelsize=8)

    fig.suptitle(title, fontsize=14, fontweight='bold')
    plt.tight_layout()

    # Save figure
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    print(f"  Saved heatmap: {output_path}")
    plt.close(fig)


def plot_combined_heatmaps(all_stages_data, output_dir):
    """Generate a combined figure showing all stages side by side.

    Args:
        all_stages_data: list of (stage_name, piece_data) tuples
        output_dir: Directory to save the figure
    """
    try:
        import matplotlib.pyplot as plt
    except ImportError:
        print("matplotlib is required for plotting: pip install matplotlib",
              file=sys.stderr)
        return

    n_stages = len(all_stages_data)
    n_pieces = 3  # black, white, king

    fig, axes = plt.subplots(n_pieces, n_stages, figsize=(5 * n_stages, 4 * n_pieces))
    if n_stages == 1:
        axes = axes.reshape(-1, 1)

    piece_names = ["Black pawn", "White pawn", "King"]

    for col, (stage_name, piece_data) in enumerate(all_stages_data):
        for row, piece_name in enumerate(piece_names):
            ax = axes[row, col]
            values = piece_data[piece_name]
            board = expand_quarter_to_board(values)
            board = np.flipud(board)

            vmax = max(abs(board.min()), abs(board.max()))
            vmin = -vmax if vmax > 0 else -1

            im = ax.imshow(board, cmap='RdYlGn', vmin=vmin, vmax=vmax,
                           aspect='equal')

            for i in range(11):
                for j in range(11):
                    val = board[i, j]
                    text_color = 'white' if abs(val) > vmax * 0.6 else 'black'
                    ax.text(j, i, f'{int(val)}', ha='center', va='center',
                            fontsize=6, color=text_color)

            if row == 0:
                ax.set_title(stage_name.upper(), fontsize=11, fontweight='bold')
            if col == 0:
                ax.set_ylabel(piece_name, fontsize=10, fontweight='bold')

            ax.set_xticks([])
            ax.set_yticks([])

    fig.suptitle("PST Values by Game Stage", fontsize=14, fontweight='bold', y=1.02)
    plt.tight_layout()

    output_path = os.path.join(output_dir, "pst_all_stages.png")
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    print(f"  Saved combined heatmap: {output_path}")
    plt.close(fig)


def fit_and_print(X, y, reg_C, scale, label="", plot=False, output_dir=None):
    """Fit logistic regression and print results.

    Returns:
        dict mapping piece name -> scaled weights, or None if fitting failed.
    """
    try:
        from sklearn.linear_model import LogisticRegression
    except ImportError:
        print("scikit-learn is required: pip install scikit-learn", file=sys.stderr)
        sys.exit(1)

    if len(X) == 0 or len(np.unique(y)) < 2:
        print(f"  Skipping{' ' + label if label else ''}: not enough data "
              f"({len(X)} positions, need both white and black wins)")
        print()
        return None

    n_white = np.sum(y == 1)
    n_black = np.sum(y == 0)
    print(f"  Positions: {len(X)} ({n_white} white wins, {n_black} black wins)")

    model = LogisticRegression(penalty="l2", C=reg_C, max_iter=2000, solver="lbfgs")
    model.fit(X, y)

    acc = model.score(X, y)
    print(f"  Training accuracy: {acc:.3f}")
    print(f"  Bias (intercept): {model.intercept_[0]:.4f}")
    print()

    weights = model.coef_[0]
    black_w = weights[0:N_QUARTER] * scale
    white_w = weights[N_QUARTER:2*N_QUARTER] * scale
    king_w = weights[2*N_QUARTER:3*N_QUARTER] * scale

    suffix = f"_{label}" if label else ""

    print(f"// Black pawn PST (negated: positive = good for black)")
    print_quarter_table(f"black_pst_quarter{suffix}", -black_w)
    print_board_heatmap("Black pawn", -black_w)

    print(f"// White pawn PST (positive = good for white)")
    print_quarter_table(f"white_pst_quarter{suffix}", white_w)
    print_board_heatmap("White pawn", white_w)

    print(f"// King PST (positive = good for white)")
    print_quarter_table(f"king_pst_quarter{suffix}", king_w)
    print_board_heatmap("King", king_w)

    print("RAW LOG-ODDS WEIGHTS:")
    for i, idx in enumerate(QUARTER_INDICES):
        print(f"  sq {idx:>3}:  black={weights[i]:>+.4f}  "
              f"white={weights[N_QUARTER+i]:>+.4f}  "
              f"king={weights[2*N_QUARTER+i]:>+.4f}")
    print()

    # Prepare piece data for plotting
    piece_data = {
        "Black pawn": -black_w,
        "White pawn": white_w,
        "King": king_w,
    }

    if plot and output_dir:
        filename = f"pst_{label}.png" if label else "pst.png"
        output_path = os.path.join(output_dir, filename)
        title = f"PST Values - {label.upper()}" if label else "PST Values"
        plot_heatmaps(piece_data, title, output_path)

    return piece_data


# ---------------------------------------------------------------------------
# Stats command
# ---------------------------------------------------------------------------

def cmd_stats(args):
    """Print game length statistics."""
    conn = sqlite3.connect(args.db)
    cur = conn.cursor()

    # Total games by status
    cur.execute("""
        SELECT game_status, COUNT(*) FROM game
        WHERE game_status IS NOT NULL
        GROUP BY game_status
        ORDER BY COUNT(*) DESC
    """)
    status_counts = cur.fetchall()

    print("=== Game outcomes ===")
    print()
    total = 0
    for status, count in status_counts:
        print(f"  {status:<35s} {count:>5}")
        total += count
    print(f"  {'TOTAL':<35s} {total:>5}")
    print()

    # Game lengths (moves per game) for decided games
    games = get_decided_games(cur)
    lengths = []
    white_lengths = []
    black_lengths = []

    for game_id, status in games:
        label = game_label(status)
        if label is None:
            continue
        cur.execute(
            "SELECT MAX(move_number) FROM move WHERE game_id = ?", (game_id,)
        )
        row = cur.fetchone()
        if row is None or row[0] is None:
            continue
        length = row[0]
        lengths.append(length)
        if label == 1:
            white_lengths.append(length)
        else:
            black_lengths.append(length)

    conn.close()

    if not lengths:
        print("No completed games with moves found.")
        return

    lengths = np.array(lengths)
    print(f"=== Game length (decided games only, n={len(lengths)}) ===")
    print()
    print(f"  Mean:   {np.mean(lengths):.1f} moves")
    print(f"  Median: {np.median(lengths):.1f} moves")
    print(f"  Std:    {np.std(lengths):.1f}")
    print(f"  Min:    {np.min(lengths)}")
    print(f"  Max:    {np.max(lengths)}")
    print()

    # Percentiles
    print("  Percentiles:")
    for p in [10, 25, 33, 50, 67, 75, 90]:
        print(f"    {p:>3}th: {np.percentile(lengths, p):.0f} moves")
    print()

    # Histogram
    print("  Distribution (histogram):")
    bin_edges = np.arange(0, np.max(lengths) + 10, 10)
    counts, _ = np.histogram(lengths, bins=bin_edges)
    max_count = max(counts) if len(counts) > 0 else 1
    for i, count in enumerate(counts):
        lo, hi = int(bin_edges[i]), int(bin_edges[i+1])
        bar = "#" * int(40 * count / max_count) if max_count > 0 else ""
        print(f"    {lo:>3}-{hi:<3} | {bar} {count}")
    print()

    # Tercile boundaries (for --stages 3)
    t1 = np.percentile(lengths, 33.3)
    t2 = np.percentile(lengths, 66.7)
    print(f"  Tercile boundaries: {t1:.0f} and {t2:.0f} moves")
    print(f"    Short games:  <= {t1:.0f} moves")
    print(f"    Medium games: {t1:.0f}-{t2:.0f} moves")
    print(f"    Long games:   > {t2:.0f} moves")
    print()

    # White vs black win lengths
    if white_lengths and black_lengths:
        print(f"  White wins (n={len(white_lengths)}): "
              f"mean={np.mean(white_lengths):.1f}, median={np.median(white_lengths):.1f}")
        print(f"  Black wins (n={len(black_lengths)}): "
              f"mean={np.mean(black_lengths):.1f}, median={np.median(black_lengths):.1f}")
        print()


# ---------------------------------------------------------------------------
# Fit command
# ---------------------------------------------------------------------------

def cmd_fit(args):
    """Fit PST values via logistic regression."""
    # Set up output directory for plots if needed
    output_dir = None
    if args.plot:
        output_dir = args.output_dir or os.path.dirname(os.path.abspath(args.db))
        os.makedirs(output_dir, exist_ok=True)
        print(f"Plots will be saved to: {output_dir}")
        print()

    if args.stages:
        print(f"Loading data from {args.db} (splitting into {args.stages} stages)...")
        stage_data = load_data(
            args.db, args.samples_per_game, args.min_move, args.max_move,
            args.seed, stages=args.stages,
        )

        # Combine all stages for an overall fit first
        all_X = np.concatenate([stage_data[i][0] for i in range(args.stages)])
        all_y = np.concatenate([stage_data[i][1] for i in range(args.stages)])

        print("=" * 60)
        print("OVERALL (all stages combined)")
        print("=" * 60)
        print()
        overall_piece_data = fit_and_print(
            all_X, all_y, args.reg_C, args.scale, label="overall",
            plot=args.plot, output_dir=output_dir
        )

        # Then fit each stage separately
        stage_names = {
            3: ["early", "mid", "late"],
            2: ["early", "late"],
        }
        names = stage_names.get(args.stages,
                                [f"stage{i}" for i in range(args.stages)])

        all_stages_data = []
        if overall_piece_data:
            all_stages_data.append(("overall", overall_piece_data))

        for i in range(args.stages):
            X, y = stage_data[i]
            print("=" * 60)
            print(f"STAGE {i+1}/{args.stages}: {names[i].upper()}")
            print(f"(moves in the {names[i]} third of each game)")
            print("=" * 60)
            print()
            piece_data = fit_and_print(
                X, y, args.reg_C, args.scale, label=names[i],
                plot=args.plot, output_dir=output_dir
            )
            if piece_data:
                all_stages_data.append((names[i], piece_data))

        # Generate combined comparison plot
        if args.plot and len(all_stages_data) > 1:
            plot_combined_heatmaps(all_stages_data, output_dir)

    else:
        print(f"Loading data from {args.db}...")
        X, y = load_data(
            args.db, args.samples_per_game, args.min_move, args.max_move, args.seed,
        )
        if len(X) == 0:
            print("No positions found. Check that the database has completed games.",
                  file=sys.stderr)
            sys.exit(1)

        print("=" * 60)
        print("LEARNED PST VALUES")
        print("=" * 60)
        print()
        fit_and_print(
            X, y, args.reg_C, args.scale,
            plot=args.plot, output_dir=output_dir
        )


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Piece-square table analysis")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # stats subcommand
    stats_parser = subparsers.add_parser("stats", help="Print game length statistics")
    stats_parser.add_argument("db", help="Path to SQLite database")

    # fit subcommand
    fit_parser = subparsers.add_parser("fit", help="Fit PST values via logistic regression")
    fit_parser.add_argument("db", help="Path to SQLite database")
    fit_parser.add_argument("--stages", type=int, default=None,
                            help="Split each game into N stages and fit separately")
    fit_parser.add_argument("--samples-per-game", type=int, default=3,
                            help="Positions to sample per game (default: 3)")
    fit_parser.add_argument("--min-move", type=int, default=8,
                            help="Earliest move to sample from (default: 8)")
    fit_parser.add_argument("--max-move", type=int, default=200,
                            help="Latest move to sample from (default: 200)")
    fit_parser.add_argument("--C", type=float, default=1.0, dest="reg_C",
                            help="Inverse regularization strength (default: 1.0)")
    fit_parser.add_argument("--scale", type=float, default=100.0,
                            help="Scale factor for output PST values (default: 100)")
    fit_parser.add_argument("--seed", type=int, default=42,
                            help="Random seed (default: 42)")
    fit_parser.add_argument("--plot", action="store_true",
                            help="Generate graphical heatmap plots")
    fit_parser.add_argument("--output-dir", type=str, default=None,
                            help="Directory for plot output (default: same as db)")

    args = parser.parse_args()

    if args.command == "stats":
        cmd_stats(args)
    elif args.command == "fit":
        cmd_fit(args)


if __name__ == "__main__":
    main()
