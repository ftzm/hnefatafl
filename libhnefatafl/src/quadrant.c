#include "quadrant.h"
#include "x86intrin.h" // IWYU pragma: export

// Quadrant assignment for each board position (0-120).
// Board is 11x11. rank = pos / 11, file = pos % 11.
// Middle rank (5) and middle file (5) are QUADRANT_NONE.
//
// NW(0): rank>5, file>5   NE(1): rank>5, file<5
// SW(2): rank<5, file>5   SE(3): rank<5, file<5
//
// clang-format off
const u8 quadrant_table[121] = {
    // rank 0 (positions 0-10)
    3, 3, 3, 3, 3,  4,  2, 2, 2, 2, 2,
    // rank 1 (positions 11-21)
    3, 3, 3, 3, 3,  4,  2, 2, 2, 2, 2,
    // rank 2 (positions 22-32)
    3, 3, 3, 3, 3,  4,  2, 2, 2, 2, 2,
    // rank 3 (positions 33-43)
    3, 3, 3, 3, 3,  4,  2, 2, 2, 2, 2,
    // rank 4 (positions 44-54)
    3, 3, 3, 3, 3,  4,  2, 2, 2, 2, 2,
    // rank 5 (positions 55-65) - middle rank
    4, 4, 4, 4, 4,  4,  4, 4, 4, 4, 4,
    // rank 6 (positions 66-76)
    1, 1, 1, 1, 1,  4,  0, 0, 0, 0, 0,
    // rank 7 (positions 77-87)
    1, 1, 1, 1, 1,  4,  0, 0, 0, 0, 0,
    // rank 8 (positions 88-98)
    1, 1, 1, 1, 1,  4,  0, 0, 0, 0, 0,
    // rank 9 (positions 99-109)
    1, 1, 1, 1, 1,  4,  0, 0, 0, 0, 0,
    // rank 10 (positions 110-120)
    1, 1, 1, 1, 1,  4,  0, 0, 0, 0, 0,
};
// clang-format on

quadrant_counts init_quadrant_counts(const board *b) {
  quadrant_counts qc = {{0}, {0}, QUADRANT_NONE};
  layer black = b->black;
  MAP_INDICES(black, {
    u8 q = quadrant_table[i];
    if (q < 4)
      qc.black[q]++;
  });
  layer white = b->white;
  MAP_INDICES(white, {
    u8 q = quadrant_table[i];
    if (q < 4)
      qc.white[q]++;
  });
  int king_pos = LOWEST_INDEX(b->king);
  qc.king = quadrant_table[king_pos];
  return qc;
}
