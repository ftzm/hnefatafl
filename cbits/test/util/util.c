#include "assert.h"
#include "board.h"
#include "layer.h"
#include "stdio.h"
#include "stdlib.h"
#include "theft.h"
#include "theft_types.h"
#include "x86intrin.h"
#include "constants.h"


uint64_t my_random_choice(struct theft *t, int limit) {
  return random() % limit;
}

uint64_t
theft_random_choice_between(struct theft *t, uint64_t floor, uint64_t ceil) {
  return random() % ceil + floor;
}

board theft_create_board(struct theft *t) {
  layer occ = corners;
  // set throne in occ
  op_layer_bit(occ, 60, |=);

  layer black = EMPTY_LAYER;
  uint64_t black_count = theft_random_choice_between(t, 1, 25);
  // uint64_t black_count = 1;
  while (black_count) {
    uint64_t index = my_random_choice(t, 120);
    // printf("black index: %ld\n", index);
    if (check_index(occ, index)) {
      continue;
    }
    op_layer_bit(black, index, |=);
    op_layer_bit(occ, index, |=);
    black_count--;
  }
  layer black_r = rotate_layer_right(black);

  layer white = EMPTY_LAYER;
  uint64_t white_count = theft_random_choice_between(t, 1, 12);
  // uint64_t white_count = 1;
  while (white_count) {
    uint64_t index = my_random_choice(t, 120);
    // printf("white index: %ld\n", index);
    if (check_index(occ, index)) {
      continue;
    }
    op_layer_bit(white, index, |=);
    op_layer_bit(occ, index, |=);
    white_count--;
  }
  layer white_r = rotate_layer_right(white);

  // unset throne in occ
  op_layer_bit(occ, 60, |=);
  layer king = EMPTY_LAYER;
  int attempts = 100;
  while (attempts) {
    // printf("king attempts: %d\n", attempts);
    uint64_t index = my_random_choice(t, 120);
    // printf("king index: %ld\n", index);
    if (check_index(occ, index)) {
      attempts--;
      continue;
    }
    op_layer_bit(king, index, |=);
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
