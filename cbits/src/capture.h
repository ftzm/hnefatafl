#pragma once

#include "board.h"
#include "sys/types.h"

uint8_t apply_captures_niave(const layer friends, layer *foes, layer *foes_r, int dest);

void shield_wall_black(board *b, uint pos);
void shield_wall_white(board *b, uint pos);

void shield_wall_black_gen(board *b, uint pos);
void shield_wall_white_gen(board *b, uint pos);

void gen_foe_masks();
void gen_ally_masks();
void gen_surround_masks();
