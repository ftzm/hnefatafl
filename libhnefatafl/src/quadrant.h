#pragma once

#include "x86intrin.h" // IWYU pragma: export
// x86intrin must precede layer.h for MAP_INDICES intrinsics
#include "layer.h"
#include "types.h"

#define QUADRANT_NW 0
#define QUADRANT_NE 1
#define QUADRANT_SW 2
#define QUADRANT_SE 3
#define QUADRANT_NONE 4

typedef struct quadrant_counts {
  u8 black[4]; // NW, NE, SW, SE
  u8 white[4]; // white pawns: NW, NE, SW, SE
  u8 king;     // quadrant of king (0-3), or QUADRANT_NONE
} quadrant_counts;

extern const u8 quadrant_table[121];

quadrant_counts init_quadrant_counts(const board *b);

static inline void quadrant_move(u8 *counts, int orig, int dest) {
  u8 oq = quadrant_table[orig];
  u8 dq = quadrant_table[dest];
  if (oq != dq) {
    if (oq < 4)
      counts[oq]--;
    if (dq < 4)
      counts[dq]++;
  }
}

static inline void quadrant_captures(u8 *counts, layer captures) {
  MAP_INDICES(captures, {
    u8 q = quadrant_table[i];
    if (q < 4)
      counts[q]--;
  });
}

static inline quadrant_counts
quadrant_update_black_move(quadrant_counts qc, int orig, int dest) {
  quadrant_move(qc.black, orig, dest);
  return qc;
}

static inline quadrant_counts
quadrant_update_white_move(quadrant_counts qc, int orig, int dest) {
  quadrant_move(qc.white, orig, dest);
  return qc;
}

static inline quadrant_counts
quadrant_update_king_move(quadrant_counts qc, int orig, int dest) {
  (void)orig;
  qc.king = quadrant_table[dest];
  return qc;
}

static inline quadrant_counts
quadrant_update_black_capture(quadrant_counts qc, layer captures) {
  quadrant_captures(qc.white, captures);
  return qc;
}

static inline quadrant_counts
quadrant_update_white_capture(quadrant_counts qc, layer captures) {
  quadrant_captures(qc.black, captures);
  return qc;
}
