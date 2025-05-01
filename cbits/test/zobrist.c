#include "zobrist.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "move.h"
#include <stdint.h>

const char *sanity_capture_king_string = " .  .  X  .  X  .  O  .  .  .  . "
                                         " .  X  .  X  .  .  .  .  O  .  . "
                                         " .  .  .  O  O  X  .  .  X  .  . "
                                         " X  .  .  .  .  #  X  .  .  X  . "
                                         " .  X  .  .  .  .  O  .  .  .  X "
                                         " X  .  .  O  O  .  O  .  .  X  X "
                                         " X  .  .  .  O  O  O  .  .  .  X "
                                         " X  .  .  .  .  O  .  .  .  .  X "
                                         " .  .  .  .  .  .  .  .  O  .  . "
                                         " .  .  .  .  .  X  .  .  .  .  . "
                                         " .  .  X  .  X  X  X  X  .  .  . ";

TEST test_black_zobrist() {
  board b = read_board(sanity_capture_king_string);
  uint64_t start_zobrist = hash_for_board(b, true);

  moves_to_t r =
      moves_to_black(b, layer_neg(board_occ(b)), layer_neg(board_occ_r(b)));

  for (int i = 0; i < r.total; i++) {
    move m = r.ms[i];
    board b2 = b;
    op_layer_bit(b2.black, m.orig, ^=);
    op_layer_bit(b2.black, m.dest, ^=);

    uint64_t incremental_hash = next_hash_black(start_zobrist, m.orig, m.dest);
    apply_captures_z_black(&b2, &incremental_hash, m.dest);

    uint64_t re_hash = hash_for_board(b2, false);

    board_string_t b2_str =
        to_board_move_string(b2, m.orig, m.dest, layer_xor(b.white, b2.white));
    ASSERT_EQm(b2_str._, re_hash, incremental_hash);
  }

  return GREATEST_TEST_RES_PASS;
}

TEST test_white_zobrist() {
  board b = read_board(sanity_capture_king_string);
  uint64_t start_zobrist = hash_for_board(b, false);

  moves_to_t r =
      moves_to_white(b, layer_neg(board_occ(b)), layer_neg(board_occ_r(b)));

  for (int i = 0; i < r.total; i++) {
    move m = r.ms[i];
    board b2 = b;
    op_layer_bit(b2.white, m.orig, ^=);
    op_layer_bit(b2.white, m.dest, ^=);

    uint64_t incremental_hash = next_hash_white(start_zobrist, m.orig, m.dest);
    apply_captures_z_white(&b2, &incremental_hash, m.dest);

    uint64_t re_hash = hash_for_board(b2, true);

    board_string_t b2_str =
        to_board_move_string(b2, m.orig, m.dest, layer_xor(b.black, b2.black));
    ASSERT_EQm(b2_str._, re_hash, incremental_hash);
  }

  return GREATEST_TEST_RES_PASS;
}

TEST test_king_zobrist() {
  board b = read_board(sanity_capture_king_string);
  uint64_t start_zobrist = hash_for_board(b, false);

  moves_to_t r =
      moves_to_white(b, layer_neg(board_occ(b)), layer_neg(board_occ_r(b)));

  for (int i = 0; i < r.total; i++) {
    move m = r.ms[i];
    board b2 = b;
    op_layer_bit(b2.white, m.orig, ^=);
    op_layer_bit(b2.white, m.dest, ^=);

    uint64_t incremental_hash = next_hash_white(start_zobrist, m.orig, m.dest);
    apply_captures_z_white(&b2, &incremental_hash, m.dest);

    uint64_t re_hash = hash_for_board(b2, true);

    board_string_t b2_str =
        to_board_move_string(b2, m.orig, m.dest, layer_xor(b.black, b2.black));
    ASSERT_EQm(b2_str._, re_hash, incremental_hash);
  }

  return GREATEST_TEST_RES_PASS;
}

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {
  init_hashes();

  GREATEST_MAIN_BEGIN();

  RUN_TEST(test_black_zobrist);
  RUN_TEST(test_white_zobrist);
  RUN_TEST(test_king_zobrist);

  GREATEST_MAIN_END();
}
