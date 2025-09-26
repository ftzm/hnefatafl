#include "board.h"
#include "io.h"
#include "capture.h"
#include "stdio.h"

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

int main() {
  board b = read_board(start_board_string);
  for (int i = 0; i < 10000000; i++) {
    for (int j = 0; j < 36; j++) {
      board b2 = b;
      shield_wall_white(&b2, capture_indices[j]);
    }
  }
  printf("benched\n");
}
