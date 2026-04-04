"""
Maximum rook-like moves on 11x11 board with Hnefatafl constraints:
- Max 24 movers (pieces 2-38), rest are obstacles
- Pieces CANNOT occupy: 4 corners + center square (5,5)
- Pieces CAN slide THROUGH the center square (doesn't block)
"""
import random

SIZE = 11
MAX_MOVERS = 24
CENTER = (5, 5)
CORNERS = {(0, 0), (0, 10), (10, 0), (10, 10)}
FORBIDDEN = CORNERS | {CENTER}
VALID_POS = [(r, c) for r in range(SIZE) for c in range(SIZE) if (r, c) not in FORBIDDEN]

# Precompute forbidden as a fast lookup
_forbidden = [[False]*SIZE for _ in range(SIZE)]
for r, c in FORBIDDEN:
    _forbidden[r][c] = True


def compute_per_piece(pieces_set):
    """Return list of (moves, (r,c)) for each piece. Fast implementation."""
    # Build sorted row/col arrays
    board = [[False]*SIZE for _ in range(SIZE)]
    for r, c in pieces_set:
        board[r][c] = True

    result = []
    for r, c in pieces_set:
        moves = 0
        # Left
        nc = c - 1
        while nc >= 0:
            if board[r][nc]: break
            if not _forbidden[r][nc]: moves += 1
            nc -= 1
        # Right
        nc = c + 1
        while nc < SIZE:
            if board[r][nc]: break
            if not _forbidden[r][nc]: moves += 1
            nc += 1
        # Up
        nr = r - 1
        while nr >= 0:
            if board[nr][c]: break
            if not _forbidden[nr][c]: moves += 1
            nr -= 1
        # Down
        nr = r + 1
        while nr < SIZE:
            if board[nr][c]: break
            if not _forbidden[nr][c]: moves += 1
            nr += 1
        result.append((moves, (r, c)))
    return result


def score(pieces_set):
    """Total moves of the best MAX_MOVERS pieces."""
    pp = compute_per_piece(pieces_set)
    if len(pp) <= MAX_MOVERS:
        return sum(m for m, _ in pp)
    pp.sort(reverse=True)
    return sum(m for m, _ in pp[:MAX_MOVERS])


def score_fast(pieces_set, board):
    """Fast score using prebuilt board."""
    result = []
    for r, c in pieces_set:
        moves = 0
        nc = c - 1
        while nc >= 0:
            if board[r][nc]: break
            if not _forbidden[r][nc]: moves += 1
            nc -= 1
        nc = c + 1
        while nc < SIZE:
            if board[r][nc]: break
            if not _forbidden[r][nc]: moves += 1
            nc += 1
        nr = r - 1
        while nr >= 0:
            if board[nr][c]: break
            if not _forbidden[nr][c]: moves += 1
            nr -= 1
        nr = r + 1
        while nr < SIZE:
            if board[nr][c]: break
            if not _forbidden[nr][c]: moves += 1
            nr += 1
        result.append(moves)
    if len(result) <= MAX_MOVERS:
        return sum(result)
    result.sort(reverse=True)
    return sum(result[:MAX_MOVERS])


def hill_climb(pieces, max_iters=150):
    """Move one piece at a time to maximize score."""
    pieces = set(pieces)
    board = [[False]*SIZE for _ in range(SIZE)]
    for r, c in pieces:
        board[r][c] = True
    best = score_fast(pieces, board)
    improved = True
    iters = 0
    while improved and iters < max_iters:
        improved = False
        iters += 1
        for p in list(pieces):
            pr, pc = p
            pieces.remove(p)
            board[pr][pc] = False
            best_pos = p
            best_val = score_fast(pieces | {p}, board)  # with p back
            board[pr][pc] = True  # temporarily restore for baseline
            board[pr][pc] = False  # remove again for testing

            for r, c in VALID_POS:
                if not board[r][c]:
                    board[r][c] = True
                    pieces.add((r, c))
                    val = score_fast(pieces, board)
                    pieces.remove((r, c))
                    board[r][c] = False
                    if val > best_val:
                        best_val = val
                        best_pos = (r, c)
                        improved = True
            pieces.add(best_pos)
            board[best_pos[0]][best_pos[1]] = True
        best = score_fast(pieces, board)
    return pieces, best


def greedy_add(n):
    pieces = set()
    board = [[False]*SIZE for _ in range(SIZE)]
    for _ in range(n):
        bp, bv = None, -1
        for r, c in VALID_POS:
            if not board[r][c]:
                board[r][c] = True
                pieces.add((r, c))
                v = score_fast(pieces, board)
                pieces.remove((r, c))
                board[r][c] = False
                if v > bv:
                    bv = v
                    bp = (r, c)
        pieces.add(bp)
        board[bp[0]][bp[1]] = True
    return pieces


def border_set():
    border = set()
    for i in range(SIZE):
        for pos in [(0, i), (SIZE-1, i), (i, 0), (i, SIZE-1)]:
            if pos not in FORBIDDEN:
                border.add(pos)
    return border


def random_placement(n):
    return set(random.sample(VALID_POS, n))


def find_max_for_n(n, num_random_trials=40):
    best_val = 0
    best_pieces = None

    if n > len(VALID_POS):
        return 0, set()

    # Strategy 1: Greedy
    pieces = greedy_add(n)
    pieces, val = hill_climb(pieces)
    if val > best_val:
        best_val = val
        best_pieces = set(pieces)

    # Strategy 2: Non-attacking rooks base
    rooks = set()
    used_cols = set()
    for i in range(SIZE):
        if (i, i) not in FORBIDDEN:
            rooks.add((i, i))
            used_cols.add(i)
    for r in range(SIZE):
        if not any(p[0] == r for p in rooks):
            for c in range(SIZE):
                if c not in used_cols and (r, c) not in FORBIDDEN:
                    rooks.add((r, c))
                    used_cols.add(c)
                    break
    if n <= len(rooks):
        pieces = set(list(rooks)[:n])
    else:
        pieces = set(rooks)
        board = [[False]*SIZE for _ in range(SIZE)]
        for r, c in pieces: board[r][c] = True
        while len(pieces) < n:
            bp, bv = None, -1
            for r, c in VALID_POS:
                if not board[r][c]:
                    board[r][c] = True
                    pieces.add((r, c))
                    v = score_fast(pieces, board)
                    pieces.remove((r, c))
                    board[r][c] = False
                    if v > bv:
                        bv = v
                        bp = (r, c)
            pieces.add(bp)
            board[bp[0]][bp[1]] = True
    pieces, val = hill_climb(pieces)
    if val > best_val:
        best_val = val
        best_pieces = set(pieces)

    # Strategy 3: Border-based
    border = border_set()
    if n <= len(border):
        pieces = set(border)
        board = [[False]*SIZE for _ in range(SIZE)]
        for r, c in pieces: board[r][c] = True
        while len(pieces) > n:
            brp, brv = None, -1
            for p in list(pieces):
                pr, pc = p
                pieces.remove(p)
                board[pr][pc] = False
                v = score_fast(pieces, board)
                pieces.add(p)
                board[pr][pc] = True
                if v > brv:
                    brv = v
                    brp = p
            pieces.remove(brp)
            board[brp[0]][brp[1]] = False
    else:
        pieces = set(border)
        board = [[False]*SIZE for _ in range(SIZE)]
        for r, c in pieces: board[r][c] = True
        while len(pieces) < n:
            bp, bv = None, -1
            for r, c in VALID_POS:
                if not board[r][c]:
                    board[r][c] = True
                    pieces.add((r, c))
                    v = score_fast(pieces, board)
                    pieces.remove((r, c))
                    board[r][c] = False
                    if v > bv:
                        bv = v
                        bp = (r, c)
            pieces.add(bp)
            board[bp[0]][bp[1]] = True
    pieces, val = hill_climb(pieces)
    if val > best_val:
        best_val = val
        best_pieces = set(pieces)

    # Strategy 4: Random restarts
    for _ in range(num_random_trials):
        pieces = random_placement(n)
        pieces, val = hill_climb(pieces)
        if len(pieces) == n and val > best_val:
            best_val = val
            best_pieces = set(pieces)

    return best_val, best_pieces


def print_board(pieces, movers=None):
    pp = dict((pos, m) for m, pos in compute_per_piece(pieces))
    print("    " + " ".join(f"{c:2d}" for c in range(SIZE)))
    for r in range(SIZE):
        row_str = f"{r:2d}  "
        for c in range(SIZE):
            if (r, c) in pieces:
                m = pp[(r,c)]
                if movers and (r, c) not in movers:
                    row_str += f"({m:1d})"
                else:
                    row_str += f"{m:2d} "
            elif (r, c) in FORBIDDEN:
                row_str += " # "
            else:
                row_str += " . "
        print(row_str)


def main():
    random.seed(42)
    overall_max = 0
    overall_n = 0
    overall_pieces = None

    print(f"Constraints: max {MAX_MOVERS} movers, no corners/center, center passable\n")
    print(f"{'n':>3} | {'movers':>6} | {'max_moves':>9}")
    print("-" * 30)

    for n in range(2, 39):
        trials = 30 if n < 18 else 60
        val, pieces = find_max_for_n(n, num_random_trials=trials)
        n_movers = min(n, MAX_MOVERS)
        if val > overall_max:
            overall_max = val
            overall_n = n
            overall_pieces = pieces
        print(f"{n:3d} | {n_movers:6d} | {val:9d}")

    print(f"\n{'='*50}")
    print(f"MAXIMUM: {overall_max} moves with {overall_n} total pieces")
    print(f"{'='*50}\n")

    # Determine movers vs obstacles
    pp = compute_per_piece(overall_pieces)
    pp.sort(reverse=True)
    movers = {pos for _, pos in pp[:MAX_MOVERS]}
    obstacles = overall_pieces - movers

    print(f"Board (numbers = moves/piece, (n) = obstacle, # = forbidden):\n")
    print_board(overall_pieces, movers)

    vals = [m for m, _ in pp]
    mover_vals = vals[:min(MAX_MOVERS, len(vals))]
    print(f"\nTotal: {sum(mover_vals)} moves from {len(movers)} movers" +
          (f" + {len(obstacles)} obstacles" if obstacles else ""))
    print(f"Per mover: min={min(mover_vals)}, max={max(mover_vals)}, avg={sum(mover_vals)/len(mover_vals):.1f}")

    print(f"\nVisual (M=mover, O=obstacle, #=forbidden, .=empty):\n")
    for r in range(SIZE):
        row = ""
        for c in range(SIZE):
            if (r, c) in movers:
                row += "M "
            elif (r, c) in obstacles:
                row += "O "
            elif (r, c) in FORBIDDEN:
                row += "# "
            else:
                row += ". "
        print(row)

    print(f"\nComparison:")
    print(f"  Unrestricted (all move):           332 (n=32)")
    print(f"  24-mover cap only:                 312 (n=24)")
    print(f"  24-mover + forbidden + passable:   {overall_max} (n={overall_n})")


if __name__ == "__main__":
    main()
