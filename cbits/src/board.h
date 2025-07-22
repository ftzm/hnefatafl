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

extern const char *start_board_string;

int boards_equal(board a, board b);

layer board_occ(board b);

layer board_occ_r(board b);

layer king_board_occ(board b);

layer king_board_occ_r(board b);

int black_pawn_count(const board *b);

int white_pawn_count(const board *b);

inline board apply_black_move(board b, layer l, layer l_r) {
  LAYER_XOR_ASSG(b.black, l);
  LAYER_XOR_ASSG(b.black_r, l_r);
  return b;
}

inline board apply_white_move(board b, layer l, layer l_r) {
  LAYER_XOR_ASSG(b.white, l);
  LAYER_XOR_ASSG(b.white_r, l_r);
  return b;
}

inline board apply_king_move(board b, layer l, layer l_r) {
  LAYER_XOR_ASSG(b.king, l);
  LAYER_XOR_ASSG(b.king_r, l_r);
  return b;
}
