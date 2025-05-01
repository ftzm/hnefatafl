#pragma once

#include "stdint.h"
#include "stdbool.h"
#include "board.h"

uint64_t mix(uint64_t x);

extern uint64_t black_hashes[121];
extern uint64_t white_hashes[121];
extern uint64_t king_hashes[121];
extern uint64_t is_black_hash;

void init_hashes();

uint64_t hash_for_board(board b, bool is_black_turn);

uint64_t next_hash_black(const uint64_t z, uint8_t orig, uint8_t dest);

uint64_t next_hash_white(const uint64_t z, uint8_t orig, uint8_t dest);

uint64_t next_hash_king(const uint64_t z, uint8_t orig, uint8_t dest);

uint64_t next_black_captures(const uint64_t input_z, layer captures);

uint64_t next_white_captures(const uint64_t input_z, layer captures);
