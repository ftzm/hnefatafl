#include "move.h"
#include "assert.h"
#include "board.h"
#include "io.h"
#include "layer.h"
#include "string.h"
#include "ubench.h"
#include "board.h"
#include "ubench.h"

const char *sanity_capture_king_string = " .  .  X  .  X  .  .  O  .  .  . "
                                         " .  X  .  X  .  .  .  .  .  .  . "
                                         " X  .  .  O  O  X  .  .  X  .  . "
                                         " .  .  .  .  .  #  X  .  .  X  . "
                                         " .  X  .  .  .  .  O  .  .  .  X "
                                         " X  .  .  O  O  .  O  .  .  X  X "
                                         " X  .  .  .  O  O  O  .  .  .  X "
                                         " X  .  .  .  .  O  .  .  .  .  X "
                                         " .  .  .  .  .  .  .  .  O  .  . "
                                         " .  .  .  .  .  X  .  .  .  .  . "
                                         " .  .  X  .  X  X  X  X  .  .  . ";

/*
UBENCH_EX(foo, gen) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

UBENCH_EX(foo, orig) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

/*
UBENCH_EX(foo2, gen) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

/*
UBENCH_EX(foo2, gen2) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black2(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

UBENCH_EX(foo3, gen3) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black3(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(move, mm_white) {
  const board b = read_board(sanity_capture_king_string);
  board bs[235];
  move ms[235];
  int total = 0;
  move_map mm;
  memset(mm, 0, sizeof(mm));
  move_map mm2;
  memset(mm2, 0, sizeof(mm));
  move_map mm3;
  memset(mm3, 0, sizeof(mm));
  build_mm(b.white, board_occ(b), mm);
  build_mm(b.white, board_occ(b), mm2);
  build_mm(b.white, board_occ(b), mm3);
  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(board_occ(b), throne_mask));
  free._[1] &= 144115188075855871;
  UBENCH_DO_BENCHMARK() {
    apply_southward_move(66, 11, mm2, mm2, mm3);
    UBENCH_DO_NOTHING(mm2);
    UBENCH_DO_NOTHING(mm3);
    gen_moves_from_mm(b, free, mm, ms, bs, &total);
    UBENCH_DO_NOTHING(ms);
  }
}

// needs to be at top level
UBENCH_STATE();


int main() {
  init_move_globals();
  return ubench_main(0, NULL);
}
