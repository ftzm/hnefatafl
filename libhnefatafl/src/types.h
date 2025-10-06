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
  status_ongoing,
  status_king_captured,
  status_white_surrounded,
  status_no_white_moves,
  status_king_escaped,
  status_exit_fort,
  status_no_black_moves,
} game_status;
