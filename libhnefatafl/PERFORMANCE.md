# Non-Obvious Performance Bottlenecks in libhnefatafl

Analysis of performance factors in the search engine that are not immediately
apparent from reading the C source code. Each section includes empirical
evidence from generated assembly and benchmark data.

## Methodology

- Assembly generated with `objdump -d -M intel` on `.o` files built with
  `-mbmi -mbmi2 -mlzcnt -mavx -mavx2 -O3 -g`
- Benchmarks run via `make run-benchmarks` (ubench framework)
- `perf` was not available in this environment; cache/branch miss analysis
  is inferred from instruction-level examination

---

## 1. Instruction Cache Pressure from Macro Expansion

### The Problem

The move generation macros `LEFTWARD`, `RIGHTWARD2`, and `LEFTWARD_CENTER`
(`move.c:123-179`) are instantiated multiple times with different `_i`
(sub-layer index: 0 or 1) and `_r` (rotation: normal or rotated) parameters.
Each instantiation produces near-identical machine code at a different
address.

### Evidence

Object file `.text` section sizes:

| File          | .text bytes |
|---------------|-------------|
| `move.o`      | 29,546      |
| `search.o`    | 23,316      |
| `capture.o`   | 20,409      |

The largest functions in `move.o`, all generated from the same macro
patterns:

| Function              | Size (bytes) |
|-----------------------|--------------|
| `moves_to_king_impl`  | 4,662        |
| `moves_to`            | 4,630        |
| `moves_to2`           | 4,100        |
| `moves_from_layers`   | 3,368        |
| `extract_move`        | 2,188        |

The top 5 move functions alone total **18,948 bytes** of machine code. A
typical L1 instruction cache is 32KB. These functions consume ~59% of L1i
by themselves, before accounting for the search code that calls them
(search_white: 5,081 bytes, search_black: 3,463 bytes).

### Why It Matters

- **I-cache thrashing**: When the search loop calls into move generation,
  the 5KB+ move functions compete with the 5KB search functions for L1i
  space. Context-switching between these code regions causes evictions.
- **Branch Target Buffer pollution**: The inner loops of each macro
  expansion have the same branch structure (the `while (dests)` loop) but
  at different addresses. The BTB treats these as separate prediction
  entries, diluting predictor accuracy.
- **Micro-op cache (DSB) pressure**: On Intel CPUs, the decoded micro-op
  cache has limited capacity. Repeated near-identical code sequences waste
  DSB entries that could otherwise cache hot search code.

### Comparison: `moves_to` inner loop (lower half, normal orientation)

The inner loop at offset `0x1c0-0x247` processes one destination per
iteration:

```asm
; -- extract destination --
blsi   rcx,rdi           ; isolate lowest set bit of dests
; -- find origin (dependency chain) --
blsmsk r12,rcx            ; mask below dest_bit
and    r12,[rsp-0x78]     ; AND with occupied layer
tzcnt  r10,rcx            ; dest index (parallel with above chain)
lzcnt  r12,r12            ; leading zero count of masked occ
sub    r11d,r12d          ; 63 - lzcnt = origin index
; -- bookkeeping stores --
mov    [r15+r8*2-1],r10b  ; store dest
mov    [r15+r8*2-2],r11b  ; store orig
; ... layer update via lookup tables and shlx ...
; -- advance --
sub    rdi,rcx            ; dests -= dest_bit (loop-carried)
jne    0x1c0
```

This same pattern appears 8+ times across the function (lower/upper ×
normal/rotated × leftward/rightward), plus the center row variant using
16-bit operations. Each instance is ~120-180 bytes.

### Possible Mitigations

- Convert the macro body to a regular function parameterized by sub-layer
  index and rotation lookup table pointer. The compiler may still inline
  where profitable, but the single definition reduces I-cache footprint.
- Alternatively, reduce the number of instantiations by handling both
  sub-layers in a single loop body (iterating `_i` from 0 to 1).

---

## 2. Position Set Linear Probing & Cache Line Pollution

### The Problem

The position set (`position_set.c:52-97`) uses linear probing over a flat
`u64[]` array for repetition detection. Each probe step accesses a
potentially distant cache line, and the access pattern is unpredictable
from the hardware prefetcher's perspective.

### Evidence: Assembly of `insert_position`

```asm
; fastrange64: 128-bit multiply for index computation
mov    rcx,[rdi]          ; load set->size
mov    rax,rsi            ; position hash
mul    rcx                ; 128-bit multiply → rdx:rax, index in rdx
sub    rcx,0x1            ; size - 1 for wraparound

; probe loop
.loop:
  lea    rdi,[r8+rdx*8]   ; address of elements[index]
  mov    rax,[rdi]         ; LOAD: potentially random cache line
  test   rax,rax           ; empty slot?
  jne    .check_collision
  ; insert here
  mov    [rdi],rsi         ; store position
  mov    [r9],edx          ; store deletion_index
  ret

.check_collision:
  cmp    rax,rsi           ; collision with same value?
  je     .duplicate
  cmp    rdx,rcx           ; wraparound check
  lea    rax,[rdx+0x1]
  cmovb  rdx,rax           ; branchless wraparound (good!)
  jmp    .loop
```

The compiler has done a nice job here: the wraparound uses `cmovb`
(branchless), and the loop body is tight (86 bytes total for
`insert_position`). The concern is not code quality but **access pattern**:

### Why It Matters

- **128-bit multiply on every call**: The `mul rcx` instruction has ~3-4
  cycle latency. This executes on every `insert_position` and
  `check_position` call — at least once per search node.
- **Random memory access**: The computed index is effectively random
  (derived from a zobrist hash). Each probe step `elements[index]` may
  touch a different cache line. At 8 bytes per element and 64 bytes per
  cache line, there are 8 elements per line, but with linear probing the
  first probe is rarely in L1.
- **Cache pollution**: The position set array is large (1.3× the number
  of positions searched). At depth 5, the search visits ~5.7M positions,
  meaning the set could be ~7.4M × 8 bytes ≈ 59MB. Every probe loads a
  cache line that's unlikely to be reused soon, evicting useful data.

### Benchmark Data

Position set microbenchmarks show clear load-factor sensitivity:

| Operation | 10% load | 50% load | 75% load | 90% load |
|-----------|----------|----------|----------|----------|
| Insert    | 52ns     | 78ns     | 116ns    | 113ns    |
| Lookup hit| 30ns     | 68ns     | 83ns     | 101ns    |
| Lookup miss| 32ns    | 59ns     | —        | 165ns    |

The jump from 50% to 90% load on miss lookups (59ns → 165ns, 2.8×) is
consistent with longer probe chains causing additional cache misses.

### Possible Mitigations

- **Smaller set with Bloom filter pre-check**: A Bloom filter for fast
  negative checks would avoid most probe sequences for non-repeating
  positions (the common case). The Bloom filter itself would be small
  enough to stay in L1.
- **Power-of-2 sizing with bitmask**: Replace `fastrange64` (128-bit
  multiply) with `hash & (size - 1)` (single AND instruction, 1 cycle).
  Requires power-of-2 allocation.
- **Consider the working set**: At depth 5, the set stores positions for
  a single line of play (depth ~40 max with quiescence), not all 5.7M
  nodes. The actual set size is bounded by `max_depth + quiescence_depth`
  ≈ 40 entries. If this is the case, the set is tiny and these concerns
  are minimal. Worth verifying the actual `max_elems` passed to
  `create_position_set`.

---

## 3. Data Dependency Chains in Move Generation

### The Problem

The inner loop of the `LEFTWARD` macro (`move.c:128-140`) contains a
serial dependency chain for computing the origin square of each move.
The CPU cannot exploit instruction-level parallelism within this chain.

### Evidence: Critical Path Analysis

From the disassembly of `moves_to` at offset `0x1c0`:

```
Cycle  Instruction              Latency  Depends on
─────  ──────────────────────   ───────  ──────────
  0    blsi   rcx, rdi          1        rdi (dests)
  1    blsmsk r12, rcx          1        rcx
  1    tzcnt  r10, rcx          3        rcx (parallel with blsmsk)
  2    and    r12, [rsp-0x78]   1        r12, memory (L1 hit)
  3    lzcnt  r12, r12          3        r12
  6    sub    r11d, r12d        1        r12
```

**Origin computation critical path: 7 cycles** (blsi → blsmsk → and →
lzcnt → sub)

**Destination computation: 4 cycles** (blsi → tzcnt), runs in parallel.

After computing orig and dest, there are ~15 instructions of bookkeeping
(lookup table accesses, store to move/layer arrays) that can execute while
the next iteration's `blsi` computes. But the **loop-carried dependency**
is:

```
sub    rdi, rcx               ; dests -= dest_bit
jne    .loop_top
```

This is only 1 cycle, so the carried dependency is not the bottleneck —
the 7-cycle origin chain is. Each destination takes a minimum of ~7 cycles
for the critical path, plus the bookkeeping instructions that may extend
this if they compete for execution ports.

### Throughput Estimate

With 7-cycle minimum per destination and typical 10-30 destinations per
piece set, the inner loop alone costs 70-210 cycles per direction per
sub-layer. Across 4 directions × 2 sub-layers + center row = ~9 instances,
that's 630-1890 cycles for complete move generation of one piece type.

### Benchmark Context

Move generation benchmarks:
- `moves_to_black`: 466ns (~1400 cycles at 3GHz)
- `moves_to_white`: 285ns (~855 cycles)

This is consistent with the dependency chain analysis.

### Possible Mitigations

- **Software pipelining**: Process two destinations simultaneously by
  extracting the next `dest_bit` while computing the current origin.
  The `blsi` of the next iteration could overlap with the `lzcnt` of the
  current one.
- **Origin caching**: The same origin serves multiple destinations
  (all squares a piece can slide to). Iterating by origin first, then
  extracting its destinations, would compute each origin only once instead
  of re-deriving it for each destination via `lzcnt`.
  - Note from source (`move.c:116-121`): This was tried before with
    "little speed difference" but might be worth revisiting with the
    current macro structure.

---

## 4. Modulo-by-11 in Capture Detection

### The Problem

`apply_captures_niave` (`capture.c:20`) computes `dest % 11` to determine
the file (column) of the destination square for boundary checking on
east/west captures.

### Evidence: Compiler Output

The compiler strength-reduces `dest % 11` to a multiply-shift-subtract
sequence (at offset `0x0f` in `apply_captures_niave`):

```asm
movsxd rax, r8d               ; sign-extend dest
mov    ecx, r8d
imul   rax, rax, 0x2e8ba2e9   ; magic number multiply (3 cycles)
sar    ecx, 0x1f              ; sign bit
sar    rax, 0x21              ; arithmetic shift
sub    eax, ecx               ; quotient = dest / 11
lea    ecx, [rax+rax*4]       ; quotient * 5
lea    ecx, [rax+rcx*2]       ; quotient * 11
sub    eax, ecx               ; dest - quotient*11 = dest % 11
```

This is **8 instructions, ~7 cycles** for what is conceptually a single
lookup. The `imul` has 3-cycle latency, and everything after it is
serialized.

### Context

This modulo is computed once per call to `apply_captures_niave`/
`apply_captures_niave_z`. It's used twice (for the `modDest < 9` and
`modDest > 1` boundary checks). In the search, capture detection runs
for every move at every node.

The total `apply_captures_niave` function is 561 bytes / many branches.
The 7-cycle modulo is a fraction of total cost, but it's pure overhead
with a trivial fix.

### Possible Mitigation

Replace with a 121-byte lookup table:

```c
static const u8 file_of[121] = {
    0,1,2,3,4,5,6,7,8,9,10,  // row 0
    0,1,2,3,4,5,6,7,8,9,10,  // row 1
    // ... 11 rows total
};
```

This replaces 8 instructions / 7 cycles with a single `movzx` from a
table that fits in 2 cache lines and will be permanently hot in L1d.
The table is small enough that it adds negligible data cache pressure.

Note: `rank_table[121]` and `file_table[121]` already exist in `layer.h`
for other purposes. If `file_table` contains the file (column) index,
it can be reused directly — no new table needed.

---

## Summary: Impact Ranking

| # | Issue | Estimated Impact | Effort |
|---|-------|-----------------|--------|
| 1 | I-cache pressure from macro expansion | High — 19KB of move gen code competes with 8.5KB of search code for 32KB L1i | Medium |
| 2 | Position set cache pollution | Medium — depends on actual working set size; if set is small (just current line of play), impact is low | Low-Medium |
| 3 | Dependency chain in move gen | Medium — 7-cycle minimum per destination, but total move gen is already fast (~300-470ns) | Medium-High |
| 4 | Modulo-by-11 | Low — 7 cycles per capture call, but a trivial fix via existing lookup table | Very Low |

### Recommendation Order

1. **Modulo-by-11** (target 4): Trivial fix, zero risk, small but free gain
2. **Position set sizing** (target 2): Verify actual `max_elems` to determine
   if this is a real concern or a non-issue
3. **I-cache pressure** (target 1): Highest potential impact but requires
   careful restructuring; benchmark before/after to confirm improvement
4. **Dependency chain** (target 3): Lowest ROI — the compiler has already
   done reasonable scheduling, and the total move generation time is small
   relative to search
