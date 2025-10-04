#pragma once

#include "util.h"

typedef struct layer {
  u64 _[2];
} layer;

typedef struct board {
  layer black;
  layer black_r;
  layer white;
  layer white_r;
  layer king;
  layer king_r;
} board;

typedef enum dir {
  north,
  south,
  east,
  west,
} dir;

typedef struct move {
  u8 orig;
  u8 dest;
} move;

typedef enum game_status {
  ongoing,
  king_captured,
  white_surrounded,
  no_white_moves,
  king_escaped,
  exit_fort,
  no_black_moves,
} game_status;