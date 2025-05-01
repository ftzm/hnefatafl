#include "stdint.h"
#include "stdbool.h"
#include "board.h"
#include "x86intrin.h"
#include "zobrist.h"

/*
pseudo random number generation
https://jonkagstrom.com/mx3/mx3_rev2.html

These have really nice distribution over the whole integer, making it
appropriate for use with e.g. fastrange
*/
uint64_t mix(uint64_t x) {
	x ^= x >> 32;
	x *= 0xbea225f9eb34556d;
	x ^= x >> 29;
	x *= 0xbea225f9eb34556d;
	x ^= x >> 32;
	x *= 0xbea225f9eb34556d;
	x ^= x >> 29;    	
	return x;
}

uint64_t black_hashes[121];
uint64_t white_hashes[121];
uint64_t king_hashes[121];
uint64_t is_black_hash;

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

uint64_t hash_for_board(board b, bool is_black_turn) {
  uint64_t hash = 0;
  for (int i = 0; i < 121; i++) {
    if (check_index(b.black, i)) {
      hash ^= black_hashes[i];
    } else if (check_index(b.white, i)) {
      hash ^= white_hashes[i];
    } else if (check_index(b.king, i)) {
      hash ^= king_hashes[i];
    }
  }
  if (is_black_turn) {
    hash ^= is_black_hash;
  }
  return hash;
}

uint64_t next_hash_black(const uint64_t z, uint8_t orig, uint8_t dest) {
  return z ^ black_hashes[orig] ^ black_hashes[dest] ^ is_black_hash;
}

uint64_t next_hash_white(const uint64_t z, uint8_t orig, uint8_t dest) {
  return z ^ white_hashes[orig] ^ white_hashes[dest] ^ is_black_hash;
}

uint64_t next_hash_king(const uint64_t z, uint8_t orig, uint8_t dest) {
  return z ^ king_hashes[orig] ^ king_hashes[dest] ^ is_black_hash;
}

// TODO: maybe integrate this into capture handling
uint64_t next_black_captures(const uint64_t input_z, layer captures) {
  uint64_t z = input_z;
  while (captures._[0]) {
    z ^= black_hashes[_tzcnt_u64(captures._[0])];
    captures._[0] = _blsr_u64(captures._[0]);
  }
  while (captures._[1]) {
    z ^= black_hashes[64 + _tzcnt_u64(captures._[1])];
    captures._[1] = _blsr_u64(captures._[1]);
  }
  return z;
}

// TODO: maybe integrate this into capture handling
uint64_t next_white_captures(const uint64_t input_z, layer captures) {
  uint64_t z = input_z;
  while (captures._[0]) {
    z ^= white_hashes[_tzcnt_u64(captures._[0])];
    captures._[0] = _blsr_u64(captures._[0]);
  }
  while (captures._[1]) {
    z ^= white_hashes[64 + _tzcnt_u64(captures._[1])];
    captures._[1] = _blsr_u64(captures._[1]);
  }
  return z;
}
