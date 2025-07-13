#pragma once

#include "stdint.h"
#include "stdbool.h"
#include "util.h"
#include "board.h"

u64 mix(u64 x);

extern u64 black_hashes[121];
extern u64 white_hashes[121];
extern u64 king_hashes[121];
extern u64 is_black_hash;

void init_hashes();

u64 hash_for_board(board b, bool is_black_turn);

u64 next_hash_black(const u64 z, u8 orig, u8 dest);

u64 next_hash_white(const u64 z, u8 orig, u8 dest);

u64 next_hash_king(const u64 z, u8 orig, u8 dest);

u64 next_black_captures(const u64 input_z, layer captures);

u64 next_white_captures(const u64 input_z, layer captures);
