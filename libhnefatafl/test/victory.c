#include "greatest.h"

#include "board.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "victory.h"

// Attacker direction bitmask for king_capture_check exhaustive test
#define DIR_NORTH 0x1
#define DIR_SOUTH 0x2
#define DIR_EAST  0x4
#define DIR_WEST  0x8

TEST test_king_capture_check(
    bool (*reference)(const board *b),
    bool (*optimized)(const board *b)) {
  for (int i = 0; i < 121; i++) {
    for (u8 attackers = 0; attackers < 16; attackers++) {
      board b = {.king = EMPTY_LAYER, .black = EMPTY_LAYER};
      SET_INDEX(b.king, i);

      if ((attackers & DIR_NORTH) && RANK(i) != 10)
        SET_INDEX(b.black, i + 11);
      if ((attackers & DIR_SOUTH) && RANK(i) != 0)
        SET_INDEX(b.black, i - 11);
      if ((attackers & DIR_EAST) && FILE(i) != 0)
        SET_INDEX(b.black, i - 1);
      if ((attackers & DIR_WEST) && FILE(i) != 10)
        SET_INDEX(b.black, i + 1);

      bool ref_res = reference(&b);
      bool opt_res = optimized(&b);
      if (ref_res != opt_res) {
        printf("king at %d, attackers=0x%x: reference=%d optimized=%d\n",
               i, attackers, ref_res, opt_res);
        print_board(b);
        FAIL();
      }
    }
  }
  PASS();
}

TEST test_surround(const char *board_str, bool should_surround) {
  board b = read_board(board_str);
  ASSERT_EQ(should_surround, surrounded(&b));
  PASS();
}

TEST test_exit_fort(const char *board_string, bool should_be_exit_fort) {
  board rotations[4];
  rotations[0] = read_board(board_string);
  for (int r = 1; r < 4; r++)
    rotations[r] = rotate_board_right(rotations[r - 1]);

  for (int r = 0; r < 4; r++) {
    if (exit_fort(&rotations[r]) != should_be_exit_fort) {
      printf("exit_fort mismatch at rotation %d:\n", r);
      print_board(rotations[r]);
      FAIL();
    }
  }

  PASS();
}

SUITE(victory_suite) {
  RUN_TESTp(
      test_king_capture_check,
      king_capture_check_ref,
      king_capture_check);

  RUN_TESTp(
      test_surround,
      ".  .  .  X  X  X  X  X  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  O  .  .  .  .  X"
      "X  .  .  .  O  O  O  .  .  .  X"
      "X  X  .  O  O  #  O  O  .  X  X"
      "X  .  .  .  O  O  O  .  .  .  X"
      "X  .  .  .  .  O  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  X  X  X  X  X  .  .  .",
      false);

  RUN_TESTp(
      test_surround,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  O  O  O  O  .  .  .  .  ."
      ".  .  .  .  O  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  #  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_surround,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  X  .  .  .  .  .  ."
      ".  .  X  .  .  X  .  .  .  .  ."
      ".  X  O  O  O  O  X  .  .  .  ."
      ".  .  X  .  O  #  X  .  .  .  ."
      ".  .  .  X  .  X  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_surround,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  X  .  .  O  .  .  ."
      ".  .  X  .  .  X  .  .  .  .  ."
      ".  X  O  O  O  O  X  .  .  .  ."
      ".  .  X  .  O  #  X  .  .  .  ."
      ".  .  .  X  .  X  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_exit_fort,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  O  .  X  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  O  .  .  .  O  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  X  O  O  .  ."
      ".  .  .  .  .  .  O  .  #  O  .",
      true);

  RUN_TESTp(
      test_exit_fort,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  O  .  X  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  O  .  .  .  O  .  .  ."
      ".  .  .  .  .  .  .  .  #  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  X  O  O  .  ."
      ".  .  .  .  .  .  O  .  .  O  .",
      false);

  RUN_TESTp(
      test_exit_fort,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  X  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  .  .  O  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  X  O  O  .  .  .  ."
      ".  .  .  .  O  .  #  O  .  .  .",
      true);

  RUN_TESTp(
      test_exit_fort,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  X  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  .  .  O  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  X  O  O  .  .  .  ."
      ".  .  .  .  O  .  #  .  .  .  .",
      false);

  RUN_TESTp(
      test_exit_fort,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  X  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  .  .  O  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  O  O  .  .  ."
      ".  .  .  .  X  O  .  O  X  .  ."
      ".  .  .  .  O  O  #  O  .  .  .",
      true);

  RUN_TESTp(
      test_exit_fort,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  X  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  O  .  .  .  O  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  O  O  .  .  ."
      ".  .  .  .  X  O  X  O  X  .  ."
      ".  .  .  .  O  O  #  O  .  .  .",
      false);
}
