"""
Compute the maximum number of rook-like moves on an 11x11 board
for n pieces (n = 2..38).
"""
import random

SIZE = 11

def compute_moves(pieces):
    """Given a set of piece positions, compute total moves."""
    # Build row and column lookup for fast neighbor finding
    row_pieces = [[] for _ in range(SIZE)]
    col_pieces = [[] for _ in range(SIZE)]
    for r, c in pieces:
        row_pieces[r].append(c)
        col_pieces[c].append(r)
    for i in range(SIZE):
        row_pieces[i].sort()
        col_pieces[i].sort()

    total = 0
    for r, c in pieces:
        # Horizontal moves
        cols = row_pieces[r]
        idx = cols.index(c)
        # Left
        left = c - (cols[idx-1] + 1) if idx > 0 else c
        # Right
        right = (cols[idx+1] - 1) - c if idx < len(cols) - 1 else (SIZE - 1) - c
        # Vertical moves
        rows = col_pieces[c]
        idy = rows.index(r)
        # Up
        up = r - (rows[idy-1] + 1) if idy > 0 else r
        # Down
        down = (rows[idy+1] - 1) - r if idy < len(rows) - 1 else (SIZE - 1) - r
        total += left + right + up + down
    return total


def non_attacking_rooks(n):
    """Place n non-attacking rooks (n <= 11)."""
    return {(i, i) for i in range(n)}


def border_minus(remove_count):
    """Full border minus some pieces, optimized."""
    border = set()
    for i in range(SIZE):
        border.add((0, i))
        border.add((SIZE-1, i))
        border.add((i, 0))
        border.add((i, SIZE-1))
    return border  # 40 pieces


def hill_climb(pieces, max_iters=5000):
    """Local search: try moving one piece at a time to improve total moves."""
    pieces = set(pieces)
    best = compute_moves(pieces)
    improved = True
    iters = 0
    while improved and iters < max_iters:
        improved = False
        iters += 1
        piece_list = list(pieces)
        random.shuffle(piece_list)
        for p in piece_list:
            pieces.remove(p)
            best_pos = p
            best_val = compute_moves(pieces | {p})
            # Try all empty positions
            for r in range(SIZE):
                for c in range(SIZE):
                    if (r, c) not in pieces and (r, c) != p:
                        val = compute_moves(pieces | {(r, c)})
                        if val > best_val:
                            best_val = val
                            best_pos = (r, c)
                            improved = True
            pieces.add(best_pos)
        best = compute_moves(pieces)
    return pieces, best


def greedy_add(n):
    """Greedily add pieces one at a time to maximize total moves."""
    pieces = set()
    for _ in range(n):
        best_pos = None
        best_val = -1
        for r in range(SIZE):
            for c in range(SIZE):
                if (r, c) not in pieces:
                    val = compute_moves(pieces | {(r, c)})
                    if val > best_val:
                        best_val = val
                        best_pos = (r, c)
        pieces.add(best_pos)
    return pieces, compute_moves(pieces)


def greedy_remove(pieces_set):
    """Greedily remove pieces one at a time if it improves total moves."""
    pieces = set(pieces_set)
    improved = True
    while improved:
        improved = False
        best_remove = None
        best_val = compute_moves(pieces)
        for p in list(pieces):
            val = compute_moves(pieces - {p})
            if val > best_val:
                best_val = val
                best_remove = p
                improved = True
        if best_remove:
            pieces.remove(best_remove)
    return pieces, compute_moves(pieces)


def random_placement(n):
    """Random placement of n pieces."""
    all_pos = [(r, c) for r in range(SIZE) for c in range(SIZE)]
    selected = random.sample(all_pos, n)
    return set(selected)


def find_max_for_n(n, num_random_trials=20):
    """Find the maximum total moves for exactly n pieces."""
    best_val = 0
    best_pieces = None

    # Strategy 1: Greedy add
    pieces, val = greedy_add(n)
    if val > best_val:
        best_val = val
        best_pieces = pieces

    # Strategy 2: Non-attacking rooks + greedy add for extras
    if n <= SIZE:
        pieces = non_attacking_rooks(n)
        val = compute_moves(pieces)
        if val > best_val:
            best_val = val
            best_pieces = pieces
    else:
        pieces = non_attacking_rooks(SIZE)
        for _ in range(n - SIZE):
            bp = None
            bv = -1
            for r in range(SIZE):
                for c in range(SIZE):
                    if (r, c) not in pieces:
                        v = compute_moves(pieces | {(r, c)})
                        if v > bv:
                            bv = v
                            bp = (r, c)
            pieces.add(bp)
        val = compute_moves(pieces)
        if val > best_val:
            best_val = val
            best_pieces = pieces

    # Strategy 3: Border-based (for larger n)
    border = border_minus(0)
    if n <= 40:
        # Try full border then greedily remove to get n pieces
        if n < 40:
            pieces = set(border)
            while len(pieces) > n:
                best_remove_pos = None
                best_remove_val = -1
                for p in list(pieces):
                    v = compute_moves(pieces - {p})
                    if v > best_remove_val:
                        best_remove_val = v
                        best_remove_pos = p
                pieces.remove(best_remove_pos)
            val = compute_moves(pieces)
            if val > best_val:
                best_val = val
                best_pieces = pieces
        else:
            val = compute_moves(border)
            if val > best_val:
                best_val = val
                best_pieces = border

    # Strategy 4: Hill climbing from best so far
    if best_pieces and len(best_pieces) == n:
        pieces, val = hill_climb(best_pieces)
        # Might have changed count, recheck
        if len(pieces) == n and val > best_val:
            best_val = val
            best_pieces = pieces

    # Strategy 5: Random + hill climb
    for _ in range(num_random_trials):
        pieces = random_placement(n)
        pieces, val = hill_climb(pieces)
        if len(pieces) == n and val > best_val:
            best_val = val
            best_pieces = pieces

    return best_val, best_pieces


def main():
    overall_max = 0
    overall_n = 0
    overall_pieces = None

    print(f"{'n':>3} | {'max_moves':>9} | arrangement")
    print("-" * 60)

    for n in range(2, 39):
        val, pieces = find_max_for_n(n)
        if val > overall_max:
            overall_max = val
            overall_n = n
            overall_pieces = pieces
        print(f"{n:3d} | {val:9d} | {sorted(pieces) if n <= 11 else '...'}")

    print()
    print(f"Overall maximum: {overall_max} moves with {overall_n} pieces")
    if overall_pieces:
        # Print the board
        print(f"\nOptimal arrangement ({overall_n} pieces):")
        for r in range(SIZE):
            row = ""
            for c in range(SIZE):
                row += "X " if (r, c) in overall_pieces else ". "
            print(row)


if __name__ == "__main__":
    random.seed(42)
    main()
