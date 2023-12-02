#pragma once
#include <stdint.h>

typedef struct u128{
  uint64_t hi;
  uint64_t lo;
} u128;

typedef __uint128_t u1282;

/*
u128 u128_or(const u128 x, const u128 y);

u128 u128_and(const u128 x, const u128 y);

u128 u128_xor(const u128 x, const u128 y);

uint64_t u128_check_bit(const u128 board, const int bit);

void u128_set_bit(u128 *board, const int bit);

void u128_clear_bit(u128 *board, const int bit);

int u128_popcount(u128 *n);
*/

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

inline int u128_popcount(const u128 n) {
  return __builtin_popcountll(n.hi)
    + __builtin_popcountll(n.lo);
}

inline int u128_ctz(const u128 *n) {
  if (n->lo == 0) {
    return __builtin_ctzll(n->hi) + 64;
  } else {
    return __builtin_ctzll(n->lo);
  }
}

static u128 u128_zero = {0, 0};
