#include "greatest.h"

#include "board.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "victory.h"

TEST test_king_capture_check(
    bool (*check1)(const board *b),
    bool (*check2)(const board *b)) {
  for (int i = 0; i < 121; i++) {
    for (u8 attackers = 0; attackers < 16; attackers++) {
      board b = {.king = EMPTY_LAYER, .black = EMPTY_LAYER};
      SET_INDEX(b.king, i);
      // north
      if (attackers & 1 && RANK(i) != 10) {
        int index = i + 11;
        SET_INDEX(b.black, index);
      }
      // south
      if (attackers & 0b10 && RANK(i) != 0) {
        int index = i - 11;
        SET_INDEX(b.black, index);
      }
      // east
      if (attackers & 0b100 && FILE(i) != 0) {
        int index = i - 1;
        SET_INDEX(b.black, index);
      }
      // west
      if (attackers & 0b1000 && FILE(i) != 10) {
        int index = i + 1;
        SET_INDEX(b.black, index);
      }
      bool check1_res = check1(&b);
      bool check2_res = check2(&b);
      // print_board(b);
      if (check1_res != check2_res) {
        printf("check1: %b\n", check1_res);
        printf("check2: %b\n", check2_res);
        print_board(b);
        FAIL();
      }
    }
  }
  PASS();
}

TEST test_surround(const char *b, bool should_surround) {
  board board = read_board(b);
  bool is_surrounded = surrounded(&board);
  if (is_surrounded != should_surround) {
    FAIL();
  }
  PASS();
}

TEST test_exit_fort(const char *board_string, bool should_be_exit_fort) {
  board b = read_board(board_string);
  board b_r = rotate_board_right(b);
  board b_r_r = rotate_board_right(b_r);
  board b_r_r_r = rotate_board_right(b_r_r);
  bool is_exit_fort;

  is_exit_fort = exit_fort(&b);
  if (is_exit_fort != should_be_exit_fort) {
    FAIL();
  }

  is_exit_fort = exit_fort(&b_r);
  if (is_exit_fort != should_be_exit_fort) {
    print_board(b);
    print_board(b_r);
    FAIL();
  }

  is_exit_fort = exit_fort(&b_r_r);
  if (is_exit_fort != should_be_exit_fort) {
    print_board(b);
    print_board(b_r_r);
    FAIL();
  }

  is_exit_fort = exit_fort(&b_r_r_r);
  if (is_exit_fort != should_be_exit_fort) {
    print_board(b_r_r_r);
    FAIL();
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
