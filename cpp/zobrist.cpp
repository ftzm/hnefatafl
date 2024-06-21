#pragma once

#include <cstdint>
#include <random>
#include "board.cpp"

uint64_t random_uint64() {
  std::random_device rand_dev;
  std::mt19937 generator(rand_dev());
  std::uniform_int_distribution<uint64_t> distr(
      std::numeric_limits<uint64_t>::min(),
      std::numeric_limits<uint64_t>::max());
  return distr(generator);
}

uint64_t black_hashes[121];
uint64_t white_hashes[121];
uint64_t king_hashes[121];
uint64_t is_black_hash;

void init_hashes() {
  for (int i = 0; i < 121; i++) {
    black_hashes[i] = random_uint64();
  }
  for (int i = 0; i < 121; i++) {
    white_hashes[i] = random_uint64();
  }
  for (int i = 0; i < 121; i++) {
    king_hashes[i] = random_uint64();
  }
  is_black_hash = random_uint64();
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
  /*
  */
  return hash;
}

uint64_t next_hash_black(const uint64_t z, const move m) {
  return z ^ black_hashes[m.orig] ^ black_hashes[m.dest] ^ is_black_hash;
  // return z ^ black_hashes[m.orig] ^ black_hashes[m.dest];
}

uint64_t next_hash_white(const uint64_t z, const move m) {
  return z ^ white_hashes[m.orig] ^ white_hashes[m.dest] ^ is_black_hash;
  // return z ^ white_hashes[m.orig] ^ white_hashes[m.dest];
}

uint64_t next_hash_king(const uint64_t z, const move m) {
  return z ^ king_hashes[m.orig] ^ king_hashes[m.dest] ^ is_black_hash;
  // return z ^ king_hashes[m.orig] ^ king_hashes[m.dest];
}
