#include "greatest.h"

#include "layer.h"
#include "board.h"
#include "stdbool.h"
#include "io.h"
#include "victory.h"

TEST test_king_capture_check(bool (*check1)(const board *b), bool (*check2)(const board *b)) {
  for (int i = 0; i < 121; i++) {
    for (u8 attackers = 0; attackers < 16; attackers++) {
      board b = {.king = EMPTY_LAYER, .black = EMPTY_LAYER};
      SET_INDEX(b.king, i);
      // north
      if (attackers & 1 && rank(i) != 10) {
        int index = i + 11;
        SET_INDEX(b.black, index);
      }
      // south
      if (attackers & 0b10 && rank(i) != 0) {
        int index = i - 11;
        SET_INDEX(b.black, index);
      }
      // east
      if (attackers & 0b100 && file(i) != 0) {
        int index = i - 1;
        SET_INDEX(b.black, index);
      }
      // west
      if (attackers & 0b1000 && file(i) != 10) {
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

SUITE(victory_suite) {
  RUN_TESTp(test_king_capture_check, king_capture_check_ref, king_capture_check);
}

