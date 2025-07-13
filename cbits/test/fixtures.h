#pragma once

#include "theft.h"
#include "constants.h"
#include "board.h"

uint64_t
inline theft_random_choice_between(struct theft *t, uint64_t floor, uint64_t ceil) {
  return random() % ceil + floor;
}

inline uint64_t my_random_choice(struct theft *t, int limit) {
  return random() % limit;
}

inline board theft_create_board(struct theft *t) {
  layer occ = corners;
  // set throne in occ
  OP_LAYER_BIT(occ, 60, |=);

  layer black = EMPTY_LAYER;
  uint64_t black_count = theft_random_choice_between(t, 1, 25);
  // uint64_t black_count = 1;
  while (black_count) {
    uint64_t index = my_random_choice(t, 120);
    // printf("black index: %ld\n", index);
    if (CHECK_INDEX(occ, index)) {
      continue;
    }
    OP_LAYER_BIT(black, index, |=);
    OP_LAYER_BIT(occ, index, |=);
    black_count--;
  }
  layer black_r = rotate_layer_right(black);

  layer white = EMPTY_LAYER;
  uint64_t white_count = theft_random_choice_between(t, 1, 12);
  // uint64_t white_count = 1;
  while (white_count) {
    uint64_t index = my_random_choice(t, 120);
    // printf("white index: %ld\n", index);
    if (CHECK_INDEX(occ, index)) {
      continue;
    }
    OP_LAYER_BIT(white, index, |=);
    OP_LAYER_BIT(occ, index, |=);
    white_count--;
  }
  layer white_r = rotate_layer_right(white);

  // unset throne in occ
  OP_LAYER_BIT(occ, 60, |=);
  layer king = EMPTY_LAYER;
  int attempts = 100;
  while (attempts) {
    // printf("king attempts: %d\n", attempts);
    uint64_t index = my_random_choice(t, 120);
    // printf("king index: %ld\n", index);
    if (CHECK_INDEX(occ, index)) {
      attempts--;
      continue;
    }
    OP_LAYER_BIT(king, index, |=);
    // printf("prebreak\n");
    break;
  }
  if (!attempts) {
    printf("failed to generate king position\n");
    exit(1);
  }
  layer king_r = rotate_layer_right(king);

  return (board){black, black_r, white, white_r, king, king_r};
}
