#include "u128.h"

/*
inline u128 u128_or(const u128 x, const u128 y) {
  struct u128 output = {x.hi | y.hi, x.lo | y.lo};
  return output;
}

inline u128 u128_and(const u128 x, const u128 y) {
  struct u128 output = {x.hi & y.hi, x.lo & y.lo};
  return output;
}

inline u128 u128_xor(const u128 x, const u128 y) {
  struct u128 output = {x.hi ^ y.hi, x.lo ^ y.lo};
  return output;
}

inline uint64_t u128_check_bit(const u128 board, const int bit) {
  if (bit<64) {
    return board.lo & ((uint64_t) 1 << bit);
  } else {
    return board.hi & ((uint64_t) 1 << (bit - 64));
  }
}

inline void u128_set_bit(u128 *board, const int bit) {
  if (bit<64) {
    board->lo = board->lo | ((uint64_t) 1 << bit);
  } else {
    board->hi = board->hi | ((uint64_t)1 << (bit - 64));
  }
}

inline void u128_clear_bit(u128 *board, const int bit) {
  if (bit<64) {
    board->lo = board->lo & ~((uint64_t) 1 << bit);
  } else {
    board->hi = board->hi & ~((uint64_t) 1 << (bit - 64));
  }
}

inline int u128_popcount(u128 *n) {
  return __builtin_popcountll(n->hi)
    + __builtin_popcountll(n->lo);
}

*/
