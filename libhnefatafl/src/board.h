#pragma once

#include "constants.h"
#include "layer.h"

static const board start_board = {
    .black = start_black,
    .black_r = start_black_r,
    .white = start_white,
    .white_r = start_white_r,
    .king = start_king,
    .king_r = start_king_r};

int boards_equal(board a, board b);

layer board_occ(board b);

layer board_occ_r(board b);

layer king_board_occ(board b);

layer king_board_occ_r(board b);

int black_pawn_count(const board *b);

int white_pawn_count(const board *b);

static inline board apply_black_move(board b, layer l, layer l_r) {
  LAYER_XOR_ASSG(b.black, l);
  LAYER_XOR_ASSG(b.black_r, l_r);
  return b;
}

static inline board apply_white_move(board b, layer l, layer l_r) {
  LAYER_XOR_ASSG(b.white, l);
  LAYER_XOR_ASSG(b.white_r, l_r);
  return b;
}

static inline board apply_king_move(board b, layer l, layer l_r) {
  LAYER_XOR_ASSG(b.king, l);
  LAYER_XOR_ASSG(b.king_r, l_r);
  return b;
}

static inline board apply_black_move_m(board b, u8 orig, u8 dest) {
  CLEAR_INDEX(b.black, orig);
  SET_INDEX(b.black, dest);
  CLEAR_INDEX(b.black_r, rotate_right[orig]);
  SET_INDEX(b.black_r, rotate_right[dest]);
  return b;
}

static inline board apply_white_move_m(board b, u8 orig, u8 dest) {
  CLEAR_INDEX(b.white, orig);
  SET_INDEX(b.white, dest);
  CLEAR_INDEX(b.white_r, rotate_right[orig]);
  SET_INDEX(b.white_r, rotate_right[dest]);
  return b;
}

static inline board apply_king_move_m(board b, u8 orig, u8 dest) {
  CLEAR_INDEX(b.king, orig);
  SET_INDEX(b.king, dest);
  CLEAR_INDEX(b.king_r, rotate_right[orig]);
  SET_INDEX(b.king_r, rotate_right[dest]);
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
static inline layer king_destinations(board b) {
  return LAYER_NOT(king_board_occ(b));
}

/* create a layer representing empty squares the king is allowed to enter on a
 * rotated board*/
static inline layer king_destinations_r(board b) {
  return LAYER_NOT(king_board_occ_r(b));
}
