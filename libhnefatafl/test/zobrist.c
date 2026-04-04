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

// Parameterized zobrist test macro: verifies incremental hash updates match
// full recalculation for every legal move of a given piece type.
#define ZOBRIST_TEST(name, piece_field, moves_fn, hash_fn, capture_fn,         \
                     opponent_field, start_black)                               \
  TEST name(void) {                                                            \
    board b = read_board(sanity_capture_king_string);                          \
    u64 start_zobrist = hash_for_board(b, start_black);                        \
                                                                               \
    moves_to_t r =                                                             \
        moves_fn(b, LAYER_NEG(board_occ(b)), LAYER_NEG(board_occ_r(b)));       \
                                                                               \
    for (int i = 0; i < r.total; i++) {                                        \
      move m = r.ms[i];                                                        \
      board b2 = b;                                                            \
      OP_LAYER_BIT(b2.piece_field, m.orig, ^=);                                \
      OP_LAYER_BIT(b2.piece_field, m.dest, ^=);                                \
                                                                               \
      u64 incremental_hash = hash_fn(start_zobrist, m.orig, m.dest);           \
      capture_fn(&b2, &incremental_hash, m.dest);                              \
                                                                               \
      u64 re_hash = hash_for_board(b2, !(start_black));                        \
                                                                               \
      board_string_t b2_str = to_board_move_string(                            \
          b2, m.orig, m.dest, LAYER_XOR(b.opponent_field, b2.opponent_field)); \
      static char msg_buf[1000];                                               \
      strcpy(msg_buf, b2_str._);                                               \
      ASSERT_EQm(msg_buf, re_hash, incremental_hash);                          \
    }                                                                          \
                                                                               \
    PASS();                                                                    \
  }

ZOBRIST_TEST(test_black_zobrist, black, moves_to_black, next_hash_black,
             apply_captures_z_black, white, true)
ZOBRIST_TEST(test_white_zobrist, white, moves_to_white, next_hash_white,
             apply_captures_z_white, black, false)
ZOBRIST_TEST(test_king_zobrist, king, moves_to_king, next_hash_king,
             apply_captures_z_king, black, false)

SUITE(zobrist_suite) {
  RUN_TEST(test_black_zobrist);
  RUN_TEST(test_white_zobrist);
  RUN_TEST(test_king_zobrist);
}
