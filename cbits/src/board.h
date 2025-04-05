#pragma once

#include "layer.h"

typedef struct board {
  layer black;
  layer black_r;
  layer white;
  layer white_r;
  // king can maybe also just be a char
  layer king;
  layer king_r;
} board;

extern const char* start_board_string;

int boards_equal(board a, board b);

layer board_occ(board b);

layer board_occ_r(board b);

layer king_board_occ(board b);

layer king_board_occ_r(board b);
