#pragma once

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

static inline board theft_create_board(struct theft *t) {
  layer occ = corners;
  // set throne in occ
  OP_LAYER_BIT(occ, 60, |=);

  layer black = EMPTY_LAYER;
  u64 black_count = theft_random_choice(t, 25) + 1;
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

  // unset throne in occ
  OP_LAYER_BIT(occ, 60, |=);
  layer king = EMPTY_LAYER;
  u64 start = theft_random_choice(t, 120);
  int index = find_free_index(occ, start);
  if (index < 0) {
    printf("failed to generate king position\n");
    exit(1);
  }
  OP_LAYER_BIT(king, index, |=);
  layer king_r = rotate_layer_right(king);

  return (board){black, black_r, white, white_r, king, king_r};
}
