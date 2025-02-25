#pragma once

#include "board.h"

typedef struct move {
  uint8_t orig;
  uint8_t dest;
} move;

void get_team_moves_black(
    const board current,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    board *boards);

void get_team_moves_white(
    const board current,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    board *boards);

void init_move_globals();
