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

typedef enum piece_type {
  empty,
  black_pawn,
  white_pawn,
  king,
} piece_type;

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

typedef enum move_error {
  move_error_no_error = 0,
  move_error_no_piece_at_origin = 1,
  move_error_wrong_piece_for_turn = 2,
  move_error_invalid_destination = 3,
  move_error_not_orthogonal = 4,
  move_error_path_blocked = 5,
  move_error_threefold_repetition = 6,
} move_error;

typedef struct move_validation_result {
  move_error error;
  int move_index;
} move_validation_result;
