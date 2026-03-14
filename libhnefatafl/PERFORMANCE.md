# Non-Obvious Performance Bottlenecks in libhnefatafl

Analysis of performance factors in the search engine that are not immediately
apparent from reading the C source code. Each section includes empirical
evidence from generated assembly and benchmark data.

## Methodology

- Assembly generated with `objdump -d -M intel` on `.o` files built with
  `-mbmi -mbmi2 -mlzcnt -mavx -mavx2 -O3 -g`
- Benchmarks run via `make bench_<name>` (ubench framework), 2-3 runs each
- `perf` was not available in this environment; cache/branch miss analysis
  is inferred from instruction-level examination

---

## Optimization Results Summary

Four potential bottlenecks were identified, analyzed, and empirically tested.
Each was benchmarked before/after with the criterion: keep if improved, revert
if not.

| # | Optimization | Result | Action |
|---|-------------|--------|--------|
| 1 | Modulo-by-11 → FILE() lookup in capture.c | Neutral (within noise) | **Kept** — cleaner code, zero risk |
| 2 | Position set: power-of-2 sizing with bitmask | **+30-46% on microbench**, ~1% search | **Kept** |
| 3 | I-cache: extract LEFTWARD into noinline functions | **-5-15% search regression** | **Reverted** |
| 4 | Dependency chain: origin-first iteration in LEFTWARD | **-10-12% moves_to regression** | **Reverted** |

---

## 1. Modulo-by-11 → FILE() Lookup (KEPT)

### Change

Replaced `dest % 11` with `FILE(dest)` in `capture.c` (2 occurrences).
`FILE()` uses the existing `file_table[121]` lookup from `layer.h`.

### Assembly Difference

Before (8 instructions, ~7 cycles):
```asm
movsxd rax, r8d               ; sign-extend dest
imul   rax, rax, 0x2e8ba2e9   ; magic number multiply (3 cycles)
sar    ecx, 0x1f              ; sign bit
sar    rax, 0x21              ; arithmetic shift
sub    eax, ecx               ; quotient = dest / 11
lea    ecx, [rax+rax*4]       ; quotient * 5
lea    ecx, [rax+rcx*2]       ; quotient * 11
sub    eax, ecx               ; dest - quotient*11 = dest % 11
```

After (1 instruction, ~1 cycle + L1 hit):
```asm
movzx  eax, byte [file_table + rdi]  ; table lookup
```

### Benchmark Results

No measurable difference in capture or search benchmarks — the modulo cost
is a small fraction of the total capture function cost. Kept for code clarity
(reuses existing infrastructure) and because it's strictly non-negative.

---

## 2. Position Set: Power-of-2 Bitmask (KEPT)

### Change

- Rounded set size up to next power-of-2 (with 2× overallocation)
- Replaced `fastrange64(position, set->size)` (128-bit multiply) with
  `position & set->mask` (single AND)
- Stored `mask = size - 1` instead of `size` to avoid repeated subtraction
- Replaced conditional wraparound with `(index + 1) & set->mask`

### Key Discovery

The search uses `create_position_set(100)` — the set holds positions for
a single line of play, not all search nodes. At ~100 entries × 8 bytes,
the entire set fits in ~1.5 cache lines. Cache pollution was a non-issue.

### Benchmark Results

**Microbenchmarks (position_set):**

| Operation | Before | After | Change |
|-----------|--------|-------|--------|
| Insert (90% load) | 113ns | 60ns | **-47%** |
| Lookup hit (90%) | 101ns | 69ns | **-32%** |
| Lookup miss (90%) | 165ns | 91ns | **-45%** |
| Insert (50% load) | 78ns | 53ns | **-32%** |

**Search benchmarks:**

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| depth_4 | 96ms | 94ms | ~-2% |
| depth_5 | 779ms | 764ms | ~-2% |

The 128-bit multiply was genuinely expensive relative to the small function.
Search improvement is modest because position set operations are a small
fraction of total search time.

---

## 3. I-Cache Pressure: Function Extraction (REVERTED)

### Approach

Replaced 8 `EXTRACT_FROM_LAYERS_COMMON` macro instantiations in
`moves_from_layers` with 2 `__attribute__((noinline))` functions:
- `extract_half_leftward` (9 parameters)
- `extract_half_rightward` (9 parameters)

Each function took a `rotated` boolean to select between normal/rotated
bookkeeping at runtime.

### Results

`moves_from_layers` shrank from 3,368 → 1,867 bytes (-45%), but:

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| search depth_4 | 94ms | 99-108ms | **+5-15% SLOWER** |
| search depth_5 | 764ms | 800-878ms | **+5-15% SLOWER** |

### Analysis

The function call overhead (9 parameters, many via stack; runtime `rotated`
branch) outweighed the I-cache savings. The compiler's per-instance
optimizations (constant propagation of `_i` and `_r`, dead code elimination
of unused rotated paths) were more valuable than the code size reduction.

**Reverted.**

---

## 4. Dependency Chain: Origin-First Iteration (REVERTED)

### Approach

Restructured the `LEFTWARD` macro to iterate movers (origins) in the outer
loop and destinations in the inner loop, mirroring the existing `RIGHTWARD1`
pattern. This eliminates the `blsmsk → and → lzcnt → sub` dependency chain
(7 cycles) from the inner loop — origin is computed once per mover instead
of re-derived for each destination.

Before (destination-first):
```c
while (dests) {
    dest_bit = _blsi_u64(dests);
    dest = _tzcnt_u64(dest_bit);
    orig = 63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & leftward_occ);  // 7-cycle chain
    // ... bookkeep ...
    dests -= dest_bit;
}
```

After (origin-first):
```c
while (origs) {
    orig = 63 - _lzcnt_u64(origs);       // computed once per mover
    orig_bit = (u64)1 << orig;
    dests = move_mask & ~(orig_bit | (orig_bit - 1));  // partition dests
    while (dests) {
        dest_bit = _blsi_u64(dests);
        dest = _tzcnt_u64(dest_bit);     // no origin chain needed
        // ... bookkeep ...
        dests -= dest_bit;
    }
    move_mask &= orig_bit | (orig_bit - 1);  // cleanup
}
```

### Results

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| moves_to_black | 0.468us | 0.524us | **+12% SLOWER** |
| moves_to_white | 0.291us | 0.321us | **+10% SLOWER** |
| search depth_4 | 94ms | 93ms | neutral |
| search depth_5 | 764ms | 763ms | neutral |

### Analysis

The overhead of the origin-first approach outweighs the dependency chain
savings:
- `63 - _lzcnt_u64(origs)` + shift to get highest bit is slower than
  `_blsi_u64` for lowest bit (used in the original)
- Extra masking per mover (`~(orig_bit | (orig_bit - 1))`,
  `move_mask &= ...`) adds per-origin overhead
- Movers with no destinations still incur outer-loop cost
- The CPU was already pipelining the original code effectively — the 7-cycle
  chain overlaps with bookkeeping stores from the previous iteration

This confirms the code's own TODO comment (`move.c:116-121`): "I think I've
tried this with little speed difference."

**Reverted.**

---

## Lessons Learned

1. **Micro-architectural bottlenecks don't always match textbook analysis.**
   The dependency chain in LEFTWARD was real (7 cycles), but out-of-order
   execution and store-buffer effects meant the CPU hid most of the latency.

2. **Function call overhead in hot loops is severe.** Even with `noinline`,
   the 9-parameter function extraction destroyed performance. The compiler's
   constant propagation through macros was essential.

3. **Small data structures benefit most from algorithmic improvements.**
   The position set was tiny (~1KB), so cache pollution was irrelevant.
   But replacing the 128-bit multiply with a bitmask AND was a clear win
   because it reduced per-call overhead on a frequently-called function.

4. **Always benchmark in the real workload.** Microbenchmarks showed large
   improvements for the position set, but search improvement was modest.
   Conversely, the I-cache reduction showed great code size metrics but
   hurt real search performance.
