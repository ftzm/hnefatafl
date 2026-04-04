"""
Focused search for the optimal 24-piece arrangement on 11x11 board.
All 24 pieces are movers. Uses more aggressive search strategies.
"""
import random
from itertools import combinations

SIZE = 11

def compute_moves(pieces_set):
    """Compute total moves for all pieces."""
    row_pieces = [[] for _ in range(SIZE)]
    col_pieces = [[] for _ in range(SIZE)]
    for r, c in pieces_set:
        row_pieces[r].append(c)
        col_pieces[c].append(r)
    for i in range(SIZE):
        row_pieces[i].sort()
        col_pieces[i].sort()

    total = 0
    for r, c in pieces_set:
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


def per_piece_moves(pieces_set):
    """Return dict of {(r,c): moves} for each piece."""
    row_pieces = [[] for _ in range(SIZE)]
    col_pieces = [[] for _ in range(SIZE)]
    for r, c in pieces_set:
        row_pieces[r].append(c)
        col_pieces[c].append(r)
    for i in range(SIZE):
        row_pieces[i].sort()
        col_pieces[i].sort()

    result = {}
    for r, c in pieces_set:
        cols = row_pieces[r]
        idx = cols.index(c)
        left = c - (cols[idx-1] + 1) if idx > 0 else c
        right = (cols[idx+1] - 1) - c if idx < len(cols) - 1 else (SIZE - 1) - c
        rows = col_pieces[c]
        idy = rows.index(r)
        up = r - (rows[idy-1] + 1) if idy > 0 else r
        down = (rows[idy+1] - 1) - r if idy < len(rows) - 1 else (SIZE - 1) - r
        result[(r, c)] = left + right + up + down
    return result


def hill_climb_single(pieces, max_iters=200):
    """Move one piece at a time to maximize total moves."""
    pieces = set(pieces)
    best = compute_moves(pieces)
    improved = True
    iters = 0
    while improved and iters < max_iters:
        improved = False
        iters += 1
        for p in list(pieces):
            pieces.remove(p)
            best_pos = p
            best_val = compute_moves(pieces | {p})
            for r in range(SIZE):
                for c in range(SIZE):
                    if (r, c) not in pieces:
                        val = compute_moves(pieces | {(r, c)})
                        if val > best_val:
                            best_val = val
                            best_pos = (r, c)
                            improved = True
            pieces.add(best_pos)
        best = compute_moves(pieces)
    return pieces, best


def hill_climb_swap2(pieces, max_iters=100):
    """Try swapping 2 pieces simultaneously for bigger jumps."""
    pieces = set(pieces)
    best = compute_moves(pieces)
    improved = True
    iters = 0
    while improved and iters < max_iters:
        improved = False
        iters += 1
        piece_list = list(pieces)
        for i in range(len(piece_list)):
            for j in range(i+1, len(piece_list)):
                p1, p2 = piece_list[i], piece_list[j]
                pieces.discard(p1)
                pieces.discard(p2)
                # Try all pairs of empty positions
                empty = [(r, c) for r in range(SIZE) for c in range(SIZE) if (r, c) not in pieces]
                best_pair = (p1, p2)
                best_val = compute_moves(pieces | {p1, p2})
                # Sample random pairs of empties (too many to enumerate all)
                sample_size = min(200, len(empty))
                sampled = random.sample(empty, sample_size)
                for a in range(len(sampled)):
                    for b in range(a+1, len(sampled)):
                        val = compute_moves(pieces | {sampled[a], sampled[b]})
                        if val > best_val:
                            best_val = val
                            best_pair = (sampled[a], sampled[b])
                            improved = True
                pieces.add(best_pair[0])
                pieces.add(best_pair[1])
                piece_list = list(pieces)
                break  # restart after any swap
            if improved:
                break
        best = compute_moves(pieces)
    return pieces, best


def print_board(pieces, title=""):
    if title:
        print(title)
    ppm = per_piece_moves(pieces)
    for r in range(SIZE):
        row_str = ""
        row_detail = ""
        for c in range(SIZE):
            if (r, c) in pieces:
                m = ppm[(r,c)]
                row_str += f"{m:2d} "
            else:
                row_str += " . "
        print(row_str)
    print(f"Total: {compute_moves(pieces)} moves, {len(pieces)} pieces")
    print(f"Per-piece: min={min(ppm.values())}, max={max(ppm.values())}, "
          f"avg={sum(ppm.values())/len(ppm):.1f}")
    print()


def main():
    N = 24
    random.seed(42)
    global_best = 0
    global_pieces = None

    # Strategy 1: Greedy build
    print("Strategy 1: Greedy add...")
    pieces = set()
    for _ in range(N):
        bp, bv = None, -1
        for r in range(SIZE):
            for c in range(SIZE):
                if (r, c) not in pieces:
                    v = compute_moves(pieces | {(r, c)})
                    if v > bv:
                        bv = v
                        bp = (r, c)
        pieces.add(bp)
    pieces, val = hill_climb_single(pieces)
    print(f"  Result: {val}")
    if val > global_best:
        global_best = val
        global_pieces = set(pieces)

    # Strategy 2: Border-based, remove worst 16
    print("Strategy 2: Border minus 16...")
    border = set()
    for i in range(SIZE):
        border.add((0, i))
        border.add((SIZE-1, i))
        border.add((i, 0))
        border.add((i, SIZE-1))
    pieces = set(border)
    while len(pieces) > N:
        brp, brv = None, -1
        for p in list(pieces):
            v = compute_moves(pieces - {p})
            if v > brv:
                brv = v
                brp = p
        pieces.remove(brp)
    pieces, val = hill_climb_single(pieces)
    print(f"  Result: {val}")
    if val > global_best:
        global_best = val
        global_pieces = set(pieces)

    # Strategy 3: Non-attacking rooks + greedy add
    print("Strategy 3: 11 non-attacking rooks + 13 greedy...")
    pieces = {(i, i) for i in range(SIZE)}
    for _ in range(N - SIZE):
        bp, bv = None, -1
        for r in range(SIZE):
            for c in range(SIZE):
                if (r, c) not in pieces:
                    v = compute_moves(pieces | {(r, c)})
                    if v > bv:
                        bv = v
                        bp = (r, c)
        pieces.add(bp)
    pieces, val = hill_climb_single(pieces)
    print(f"  Result: {val}")
    if val > global_best:
        global_best = val
        global_pieces = set(pieces)

    # Strategy 4: Symmetric constructions
    print("Strategy 4: Symmetric border-like constructions...")
    # 2-per-row, 2-per-col + 2 extras
    for offset in range(1, 6):
        pieces = set()
        for i in range(SIZE):
            pieces.add((i, i))
            pieces.add((i, (i + offset) % SIZE))
        # Remove 0 (we have 22) and add 2 more greedily
        # Actually we might have collisions
        if len(pieces) < 22:
            continue
        # Trim to 22 then add 2
        while len(pieces) > 22:
            worst = min(pieces, key=lambda p: compute_moves(pieces) - compute_moves(pieces - {p}))
            pieces.remove(worst)
        for _ in range(N - len(pieces)):
            bp, bv = None, -1
            for r in range(SIZE):
                for c in range(SIZE):
                    if (r, c) not in pieces:
                        v = compute_moves(pieces | {(r, c)})
                        if v > bv:
                            bv = v
                            bp = (r, c)
            pieces.add(bp)
        pieces, val = hill_climb_single(pieces)
        print(f"  offset={offset}: {val}")
        if val > global_best:
            global_best = val
            global_pieces = set(pieces)

    # Strategy 5: Many random restarts + hill climb
    print("Strategy 5: Random restarts (100 trials)...")
    for trial in range(100):
        all_pos = [(r, c) for r in range(SIZE) for c in range(SIZE)]
        pieces = set(random.sample(all_pos, N))
        pieces, val = hill_climb_single(pieces)
        if val > global_best:
            global_best = val
            global_pieces = set(pieces)
            print(f"  Trial {trial}: NEW BEST = {val}")

    # Strategy 6: Swap-2 refinement on global best
    print(f"\nStrategy 6: Swap-2 refinement from best ({global_best})...")
    pieces, val = hill_climb_swap2(set(global_pieces))
    pieces, val = hill_climb_single(pieces)
    print(f"  Result: {val}")
    if val > global_best:
        global_best = val
        global_pieces = set(pieces)

    # Final result
    print(f"\n{'='*50}")
    print(f"BEST FOUND: {global_best} moves with {N} pieces")
    print(f"{'='*50}\n")
    print_board(global_pieces, "Board (numbers = moves per piece):")

    # Print the visual board
    print("Visual:")
    for r in range(SIZE):
        row = ""
        for c in range(SIZE):
            row += "X " if (r, c) in global_pieces else ". "
        print(row)


if __name__ == "__main__":
    main()
