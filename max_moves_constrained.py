"""
Compute the maximum number of rook-like moves on an 11x11 board
for n pieces (n = 2..38), with a constraint that at most MAX_MOVERS
pieces can actually move (the rest are obstacles).
"""
import random

SIZE = 11
MAX_MOVERS = 24


def compute_moves_for_movers(all_pieces, movers):
    """Compute total moves for only the 'movers' subset, with all pieces blocking."""
    row_pieces = [[] for _ in range(SIZE)]
    col_pieces = [[] for _ in range(SIZE)]
    for r, c in all_pieces:
        row_pieces[r].append(c)
        col_pieces[c].append(r)
    for i in range(SIZE):
        row_pieces[i].sort()
        col_pieces[i].sort()

    total = 0
    for r, c in movers:
        cols = row_pieces[r]
        idx = cols.index(c)
        left = c - (cols[idx-1] + 1) if idx > 0 else c
        right = (cols[idx+1] - 1) - c if idx < len(cols) - 1 else (SIZE - 1) - c
        rows = col_pieces[c]
        idy = rows.index(r)
        up = r - (rows[idy-1] + 1) if idy > 0 else r
        down = (rows[idy+1] - 1) - r if idy < len(rows) - 1 else (SIZE - 1) - r
        total += left + right + up + down
    return total


def compute_moves_all(pieces):
    """Compute total moves when ALL pieces can move."""
    return compute_moves_for_movers(pieces, pieces)


def best_movers_subset(all_pieces, max_movers):
    """Given a placement, find which subset of up to max_movers pieces yields max moves.
    Uses greedy: pick piece with most moves, then recalc (moves don't change by picking subset
    since ALL pieces are still on the board as obstacles regardless)."""
    # Key insight: which pieces are "movers" doesn't affect blocking.
    # ALL pieces block regardless. We just sum moves of the chosen movers.
    # So we simply pick the max_movers pieces with the most individual moves.
    row_pieces = [[] for _ in range(SIZE)]
    col_pieces = [[] for _ in range(SIZE)]
    for r, c in all_pieces:
        row_pieces[r].append(c)
        col_pieces[c].append(r)
    for i in range(SIZE):
        row_pieces[i].sort()
        col_pieces[i].sort()

    piece_moves = []
    for r, c in all_pieces:
        cols = row_pieces[r]
        idx = cols.index(c)
        left = c - (cols[idx-1] + 1) if idx > 0 else c
        right = (cols[idx+1] - 1) - c if idx < len(cols) - 1 else (SIZE - 1) - c
        rows = col_pieces[c]
        idy = rows.index(r)
        up = r - (rows[idy-1] + 1) if idy > 0 else r
        down = (rows[idy+1] - 1) - r if idy < len(rows) - 1 else (SIZE - 1) - r
        piece_moves.append((left + right + up + down, (r, c)))

    # Pick top max_movers by individual move count
    piece_moves.sort(reverse=True)
    chosen = min(max_movers, len(piece_moves))
    movers = {pos for _, pos in piece_moves[:chosen]}
    total = sum(m for m, _ in piece_moves[:chosen])
    return total, movers


def non_attacking_rooks(n):
    return {(i, i) for i in range(n)}


def border_set():
    border = set()
    for i in range(SIZE):
        border.add((0, i))
        border.add((SIZE-1, i))
        border.add((i, 0))
        border.add((i, SIZE-1))
    return border


def hill_climb(pieces, max_movers, max_iters=5000):
    """Local search: move one piece at a time to improve best-movers total."""
    pieces = set(pieces)
    best_val, _ = best_movers_subset(pieces, max_movers)
    improved = True
    iters = 0
    while improved and iters < max_iters:
        improved = False
        iters += 1
        for p in list(pieces):
            pieces.remove(p)
            cur_val, _ = best_movers_subset(pieces | {p}, max_movers)
            best_pos = p
            best_local = cur_val
            for r in range(SIZE):
                for c in range(SIZE):
                    if (r, c) not in pieces and (r, c) != p:
                        val, _ = best_movers_subset(pieces | {(r, c)}, max_movers)
                        if val > best_local:
                            best_local = val
                            best_pos = (r, c)
                            improved = True
            pieces.add(best_pos)
        best_val, _ = best_movers_subset(pieces, max_movers)
    return pieces, best_val


def greedy_add(n, max_movers):
    pieces = set()
    for _ in range(n):
        best_pos = None
        best_val = -1
        for r in range(SIZE):
            for c in range(SIZE):
                if (r, c) not in pieces:
                    val, _ = best_movers_subset(pieces | {(r, c)}, max_movers)
                    if val > best_val:
                        best_val = val
                        best_pos = (r, c)
        pieces.add(best_pos)
    return pieces, best_val


def random_placement(n):
    all_pos = [(r, c) for r in range(SIZE) for c in range(SIZE)]
    return set(random.sample(all_pos, n))


def find_max_for_n(n, max_movers, num_random_trials=15):
    best_val = 0
    best_pieces = None
    best_movers = None

    # Strategy 1: Greedy add
    pieces, val = greedy_add(n, max_movers)
    movers_val, movers = best_movers_subset(pieces, max_movers)
    if movers_val > best_val:
        best_val = movers_val
        best_pieces = pieces
        best_movers = movers

    # Strategy 2: Non-attacking rooks base
    if n <= SIZE:
        pieces = non_attacking_rooks(n)
    else:
        pieces = non_attacking_rooks(SIZE)
        for _ in range(n - SIZE):
            bp, bv = None, -1
            for r in range(SIZE):
                for c in range(SIZE):
                    if (r, c) not in pieces:
                        v, _ = best_movers_subset(pieces | {(r, c)}, max_movers)
                        if v > bv:
                            bv = v
                            bp = (r, c)
            pieces.add(bp)
    val, movers = best_movers_subset(pieces, max_movers)
    if val > best_val:
        best_val = val
        best_pieces = pieces
        best_movers = movers

    # Strategy 3: Border-based
    border = border_set()
    if n <= 40:
        if n < 40:
            pieces = set(border)
            while len(pieces) > n:
                brp, brv = None, -1
                for p in list(pieces):
                    v, _ = best_movers_subset(pieces - {p}, max_movers)
                    if v > brv:
                        brv = v
                        brp = p
                pieces.remove(brp)
        else:
            pieces = set(border)
        val, movers = best_movers_subset(pieces, max_movers)
        if val > best_val:
            best_val = val
            best_pieces = pieces
            best_movers = movers

    # Strategy 4: Hill climb from best
    if best_pieces and len(best_pieces) == n:
        pieces, val = hill_climb(set(best_pieces), max_movers)
        if len(pieces) == n:
            movers_val, movers = best_movers_subset(pieces, max_movers)
            if movers_val > best_val:
                best_val = movers_val
                best_pieces = pieces
                best_movers = movers

    # Strategy 5: Random + hill climb
    for _ in range(num_random_trials):
        pieces = random_placement(n)
        pieces, val = hill_climb(pieces, max_movers)
        if len(pieces) == n:
            movers_val, movers = best_movers_subset(pieces, max_movers)
            if movers_val > best_val:
                best_val = movers_val
                best_pieces = pieces
                best_movers = movers

    return best_val, best_pieces, best_movers


def main():
    overall_max = 0
    overall_n = 0
    overall_pieces = None
    overall_movers = None

    print(f"Max movers constraint: {MAX_MOVERS}")
    print(f"{'n':>3} | {'movers':>6} | {'max_moves':>9}")
    print("-" * 35)

    for n in range(2, 39):
        val, pieces, movers = find_max_for_n(n, MAX_MOVERS)
        n_movers = len(movers) if movers else 0
        if val > overall_max:
            overall_max = val
            overall_n = n
            overall_pieces = pieces
            overall_movers = movers
        print(f"{n:3d} | {n_movers:6d} | {val:9d}")

    print()
    print(f"Overall maximum: {overall_max} moves with {overall_n} total pieces "
          f"({len(overall_movers)} movers)")

    if overall_pieces:
        print(f"\nOptimal arrangement ({overall_n} pieces, M=mover, O=obstacle):")
        for r in range(SIZE):
            row = ""
            for c in range(SIZE):
                if (r, c) in overall_movers:
                    row += "M "
                elif (r, c) in overall_pieces:
                    row += "O "
                else:
                    row += ". "
            print(row)

        # Also show unconstrained for comparison
        all_val = compute_moves_all(overall_pieces)
        print(f"\nFor comparison: all {overall_n} pieces moving = {all_val} moves")


if __name__ == "__main__":
    random.seed(42)
    main()
