#pragma once

#include "board.h"
#include "stdbool.h"
#include "util.h"

u64 hash_for_board(board b, bool is_black_turn);

u64 next_hash_black(const u64 z, u8 orig, u8 dest);

u64 next_hash_white(const u64 z, u8 orig, u8 dest);

u64 next_hash_king(const u64 z, u8 orig, u8 dest);

u64 next_black_captures(const u64 input_z, layer captures);

u64 next_white_captures(const u64 input_z, layer captures);
