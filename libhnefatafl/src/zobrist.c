#include "zobrist.h"
#include "board.h"
#include "stdbool.h"
#include "x86intrin.h"
#include "zobrist_constants.h"

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
