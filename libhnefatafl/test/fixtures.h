#pragma once

#include "assert.h"
#include "board.h"
#include "constants.h"
#include "theft.h"

// Find the next unoccupied index starting from 'start', wrapping around.
// Returns the free index, or -1 if the board is completely full.
static inline int find_free_index(layer occ, u64 start) {
  for (u64 i = 0; i < 120; i++) {
    u64 idx = (start + i) % 120;
    if (!CHECK_INDEX(occ, idx)) {
      return (int)idx;
    }
  }
  return -1;
}

// ---------------------------------------------------------------------------
// Board validation functions
// ---------------------------------------------------------------------------

// Black: 1-24, White: 1-12, King: exactly 1
static inline int board_has_valid_piece_counts(const board *b) {
  int bc = LAYER_POPCOUNT(b->black);
  int wc = LAYER_POPCOUNT(b->white);
  int kc = LAYER_POPCOUNT(b->king);
  return bc >= 1 && bc <= 24 && wc >= 1 && wc <= 12 && kc == 1;
}

// No two piece layers share any set bit
static inline int board_has_no_overlapping_pieces(const board *b) {
  return IS_EMPTY(LAYER_AND(b->black, b->white))
         && IS_EMPTY(LAYER_AND(b->black, b->king))
         && IS_EMPTY(LAYER_AND(b->white, b->king));
}

// No pieces on the four corner squares
static inline int board_has_no_pieces_on_corners(const board *b) {
  layer all_pieces = LAYER_OR(LAYER_OR(b->black, b->white), b->king);
  return IS_EMPTY(LAYER_AND(all_pieces, corners));
}

// Only the king may occupy the throne (index 60)
static inline int board_has_no_pawns_on_throne(const board *b) {
  return !CHECK_INDEX(b->black, 60) && !CHECK_INDEX(b->white, 60);
}

// Rotated layers are consistent with their non-rotated counterparts
static inline int board_has_consistent_rotations(const board *b) {
  layer expected_black_r = rotate_layer_right(b->black);
  layer expected_white_r = rotate_layer_right(b->white);
  layer expected_king_r = rotate_layer_right(b->king);
  return LAYERS_EQUAL(b->black_r, expected_black_r)
         && LAYERS_EQUAL(b->white_r, expected_white_r)
         && LAYERS_EQUAL(b->king_r, expected_king_r);
}

// All-in-one validity check
static inline int board_is_valid(const board *b) {
  return board_has_valid_piece_counts(b)
         && board_has_no_overlapping_pieces(b)
         && board_has_no_pieces_on_corners(b)
         && board_has_no_pawns_on_throne(b)
         && board_has_consistent_rotations(b);
}

static board theft_create_board(struct theft *t) {
  layer occ = corners;
  // set throne in occ
  OP_LAYER_BIT(occ, 60, |=);

  layer black = EMPTY_LAYER;
  u64 black_count = theft_random_choice(t, 24) + 1;
  while (black_count) {
    u64 start = theft_random_choice(t, 120);
    int index = find_free_index(occ, start);
    if (index < 0) break;
    OP_LAYER_BIT(black, index, |=);
    OP_LAYER_BIT(occ, index, |=);
    black_count--;
  }
  layer black_r = rotate_layer_right(black);

  layer white = EMPTY_LAYER;
  u64 white_count = theft_random_choice(t, 12) + 1;
  while (white_count) {
    u64 start = theft_random_choice(t, 120);
    int index = find_free_index(occ, start);
    if (index < 0) break;
    OP_LAYER_BIT(white, index, |=);
    OP_LAYER_BIT(occ, index, |=);
    white_count--;
  }
  layer white_r = rotate_layer_right(white);

  // unset throne in occ so king can be placed there
  CLEAR_INDEX(occ, 60);
  layer king = EMPTY_LAYER;
  u64 start = theft_random_choice(t, 120);
  int index = find_free_index(occ, start);
  if (index < 0) {
    printf("failed to generate king position\n");
    exit(1);
  }
  OP_LAYER_BIT(king, index, |=);
  layer king_r = rotate_layer_right(king);

  board result = {black, black_r, white, white_r, king, king_r};
  assert(board_is_valid(&result));
  return result;
}
