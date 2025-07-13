#pragma once

#include "board.h"
#include "sys/types.h"

extern layer surround_masks[120];
extern layer surround_masks_r[120];

u8 apply_captures_niave(const layer friends, layer *foes, layer *foes_r, int dest);

void apply_captures_z_black(board *b, u64 *z, u8 dest);
void apply_captures_z_white(board *b, u64 *z, u8 dest);

void shield_wall_black(board *b, uint pos);
void shield_wall_white(board *b, uint pos);

void shield_wall_black_gen(board *b, uint pos);
void shield_wall_white_gen(board *b, uint pos);

void gen_foe_masks();
void gen_ally_masks();
void gen_surround_masks();
