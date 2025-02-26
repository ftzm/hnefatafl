#include "assert.h"
#include "board.h"
#include "layer.h"
#include "move.h"
#include "io.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "stdbool.h"

bool board_rotation_correct(board b) {
  bool res = true;

  layer black_unrotated = rotate_layer_left(b.black_r);
  layer white_unrotated = rotate_layer_left(b.white_r);
  layer king_unrotated = rotate_layer_left(b.king_r);

  if (!(LAYERS_EQUAL(b.black, black_unrotated))) {
    layer diff = layer_xor(b.black, black_unrotated);
    print_layer(diff);
    res = false;
  }

  if (!(LAYERS_EQUAL(b.white, white_unrotated))) {
    layer diff = layer_xor(b.white, white_unrotated);
    print_layer(diff);
    res = false;
  }

  if (!(LAYERS_EQUAL(b.king, king_unrotated))) {
    layer diff = layer_xor(b.king, king_unrotated);
    print_layer(diff);
    res = false;
  }

  return res;
}

void test_start_board_moves() {
  const board start_board = read_board(start_board_string);

  board bs[235];
  move ms[235];
  int total = 0;
  uint8_t cap_counts[235] = {0};

  get_team_moves_black(start_board, &total, ms, cap_counts, bs);

  for (int i; i < total; i++) {
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.white, bs[i].white);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }

  total = 0;
  get_team_moves_white(start_board, &total, ms, cap_counts, bs);

  for (int i; i < total; i++) {
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.black, bs[i].black);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }
}

int main() {
  init_move_globals();
  test_start_board_moves();
}
