#include "zobrist.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "move.h"

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
  u64 start_zobrist = hash_for_board(b, true);

  moves_to_t r =
      moves_to_black(b, LAYER_NEG(board_occ(b)), LAYER_NEG(board_occ_r(b)));

  for (int i = 0; i < r.total; i++) {
    move m = r.ms[i];
    board b2 = b;
    OP_LAYER_BIT(b2.black, m.orig, ^=);
    OP_LAYER_BIT(b2.black, m.dest, ^=);

    u64 incremental_hash = next_hash_black(start_zobrist, m.orig, m.dest);
    apply_captures_z_black(&b2, &incremental_hash, m.dest);

    u64 re_hash = hash_for_board(b2, false);

    board_string_t b2_str =
        to_board_move_string(b2, m.orig, m.dest, LAYER_XOR(b.white, b2.white));
    static char msg_buf[1000];
    strcpy(msg_buf, b2_str._);
    ASSERT_EQm(msg_buf, re_hash, incremental_hash);
  }

  PASS();
}

TEST test_white_zobrist() {
  board b = read_board(sanity_capture_king_string);
  u64 start_zobrist = hash_for_board(b, false);

  moves_to_t r =
      moves_to_white(b, LAYER_NEG(board_occ(b)), LAYER_NEG(board_occ_r(b)));

  for (int i = 0; i < r.total; i++) {
    move m = r.ms[i];
    board b2 = b;
    OP_LAYER_BIT(b2.white, m.orig, ^=);
    OP_LAYER_BIT(b2.white, m.dest, ^=);

    u64 incremental_hash = next_hash_white(start_zobrist, m.orig, m.dest);
    apply_captures_z_white(&b2, &incremental_hash, m.dest);

    u64 re_hash = hash_for_board(b2, true);

    board_string_t b2_str =
        to_board_move_string(b2, m.orig, m.dest, LAYER_XOR(b.black, b2.black));
    static char msg_buf[1000];
    strcpy(msg_buf, b2_str._);
    ASSERT_EQm(msg_buf, re_hash, incremental_hash);
  }

  PASS();
}

TEST test_king_zobrist() {
  board b = read_board(sanity_capture_king_string);
  u64 start_zobrist = hash_for_board(b, false);

  moves_to_t r =
      moves_to_white(b, LAYER_NEG(board_occ(b)), LAYER_NEG(board_occ_r(b)));

  for (int i = 0; i < r.total; i++) {
    move m = r.ms[i];
    board b2 = b;
    OP_LAYER_BIT(b2.white, m.orig, ^=);
    OP_LAYER_BIT(b2.white, m.dest, ^=);

    u64 incremental_hash = next_hash_white(start_zobrist, m.orig, m.dest);
    apply_captures_z_white(&b2, &incremental_hash, m.dest);

    u64 re_hash = hash_for_board(b2, true);

    board_string_t b2_str =
        to_board_move_string(b2, m.orig, m.dest, LAYER_XOR(b.black, b2.black));
    static char msg_buf[1000];
    strcpy(msg_buf, b2_str._);
    ASSERT_EQm(msg_buf, re_hash, incremental_hash);
  }

  PASS();
}

SUITE(zobrist_suite) {
  RUN_TEST(test_black_zobrist);
  RUN_TEST(test_white_zobrist);
  RUN_TEST(test_king_zobrist);
}
