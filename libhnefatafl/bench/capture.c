#include "board.h"
#include "capture.h"
#include "io.h"
#include "ubench.h"

int capture_indices[] = {
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  111,
  112,
  113,
  114,
  115,
  116,
  117,
  118,
  119,
  11,
  22,
  33,
  44,
  55,
  66,
  77,
  88,
  99,
  21,
  32,
  43,
  54,
  65,
  76,
  87,
  98,
  109
};

UBENCH_EX(capture, shield_wall_white) {
  const board start_board = read_board(start_board_string);
  UBENCH_DO_BENCHMARK() {
    for (int j = 0; j < 36; j++) {
      board b = start_board;
      shield_wall_white(&b, capture_indices[j]);
      UBENCH_DO_NOTHING(&b);
    }
  }
}

UBENCH_EX(capture, shield_wall_black) {
  const board start_board = read_board(start_board_string);
  UBENCH_DO_BENCHMARK() {
    for (int j = 0; j < 36; j++) {
      board b = start_board;
      shield_wall_black(&b, capture_indices[j]);
      UBENCH_DO_NOTHING(&b);
    }
  }
}

UBENCH_EX(capture, shield_wall_white_gen) {
  const board start_board = read_board(start_board_string);
  UBENCH_DO_BENCHMARK() {
    for (int j = 0; j < 36; j++) {
      board b = start_board;
      shield_wall_white_gen(&b, capture_indices[j]);
      UBENCH_DO_NOTHING(&b);
    }
  }
}

UBENCH_EX(capture, shield_wall_black_gen) {
  const board start_board = read_board(start_board_string);
  UBENCH_DO_BENCHMARK() {
    for (int j = 0; j < 36; j++) {
      board b = start_board;
      shield_wall_black_gen(&b, capture_indices[j]);
      UBENCH_DO_NOTHING(&b);
    }
  }
}

UBENCH_STATE();

int main() {
  return ubench_main(0, NULL);
}