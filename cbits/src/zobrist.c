#include "zobrist.h"
#include "board.h"
#include "stdbool.h"
#include "stdint.h"
#include "x86intrin.h"

/*
pseudo random number generation
https://jonkagstrom.com/mx3/mx3_rev2.html

These have really nice distribution over the whole integer, making it
appropriate for use with e.g. fastrange
*/
u64 mix(u64 x) {
  x ^= x >> 32;
  x *= 0xbea225f9eb34556d;
  x ^= x >> 29;
  x *= 0xbea225f9eb34556d;
  x ^= x >> 32;
  x *= 0xbea225f9eb34556d;
  x ^= x >> 29;
  return x;
}

u64 black_hashes[121];
u64 white_hashes[121];
u64 king_hashes[121];
u64 is_black_hash;

void init_hashes() {
  int seed = 0;
  for (int i = 0; i < 121; i++) {
    black_hashes[i] = mix(seed++);
  }
  for (int i = 0; i < 121; i++) {
    white_hashes[i] = mix(seed++);
  }
  for (int i = 0; i < 121; i++) {
    king_hashes[i] = mix(seed++);
  }
  is_black_hash = mix(seed);
}

u64 hash_for_board(board b, bool is_black_turn) {
  u64 hash = 0;
  for (int i = 0; i < 121; i++) {
    if (CHECK_INDEX(b.black, i)) {
      hash ^= black_hashes[i];
    } else if (CHECK_INDEX(b.white, i)) {
      hash ^= white_hashes[i];
    } else if (CHECK_INDEX(b.king, i)) {
      hash ^= king_hashes[i];
    }
  }
  if (is_black_turn) {
    hash ^= is_black_hash;
  }
  return hash;
}

u64 next_hash_black(const u64 z, u8 orig, u8 dest) {
  return z ^ black_hashes[orig] ^ black_hashes[dest] ^ is_black_hash;
}

u64 next_hash_white(const u64 z, u8 orig, u8 dest) {
  return z ^ white_hashes[orig] ^ white_hashes[dest] ^ is_black_hash;
}

u64 next_hash_king(const u64 z, u8 orig, u8 dest) {
  return z ^ king_hashes[orig] ^ king_hashes[dest] ^ is_black_hash;
}

// TODO: maybe integrate this into capture handling
u64 next_black_captures(const u64 input_z, layer captures) {
  u64 z = input_z;
  MAP_INDICES(captures, z ^= black_hashes[i]);
  return z;
}

// TODO: maybe integrate this into capture handling
u64 next_white_captures(const u64 input_z, layer captures) {
  u64 z = input_z;
  MAP_INDICES(captures, z ^= white_hashes[i]);
  return z;
}
