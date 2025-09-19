#pragma once

#include "constants.h"
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

board rotate_board_right(board b);

/* create a layer representing empty squares both white and black pawns are
 * allowed to enter */
static inline layer pawn_destinations(board b) {
  return LAYER_NOT(LAYER_OR(throne, board_occ(b)));
}

/* create a layer representing empty squares both white and black pawns are
 * allowed to enter on a rotated board */
static inline layer pawn_destinations_r(board b) {
  return LAYER_NOT(LAYER_OR(throne, board_occ_r(b)));
}

/* create a layer representing empty squares the king is allowed to enter */
static inline layer king_destinations(board b) { return LAYER_NOT(king_board_occ(b)); }

/* create a layer representing empty squares the king is allowed to enter on a
 * rotated board*/
static inline layer king_destinations_r(board b) {
  return LAYER_NOT(king_board_occ_r(b));
}

/* find destinations where allies can move to capture foes */
layer find_capture_destinations(
    const layer allies,
    const layer foes,
    const layer occ);
