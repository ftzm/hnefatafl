#pragma once

#include "board.h"
#include "sys/types.h"

void shield_wall_black(board *b, uint pos);
void shield_wall_white(board *b, uint pos);

void shield_wall_black_gen(board *b, uint pos);
void shield_wall_white_gen(board *b, uint pos);
