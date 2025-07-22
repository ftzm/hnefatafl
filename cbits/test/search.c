#include "search.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "stdarg.h"
#include "stdbool.h"

bool pvs_equal(pv_line *a, pv_line *b) {
  if (a->length != b->length) {
    return false;
  }
  for (int i = 0; i < a->length; i++) {
    if (!MOVES_EQUAL(a->moves[i], b->moves[i])) {
      return false;
    }
  }
  return true;
}

void apply_black_move_m(board *b, move m) {
  CLEAR_INDEX(b->black, m.orig);
  SET_INDEX(b->black, m.dest);
}

void apply_white_move_m(board *b, move m) {
  if (CHECK_INDEX(b->white, m.orig)) {
    CLEAR_INDEX(b->white, m.orig);
    SET_INDEX(b->white, m.dest);
  } else {
    CLEAR_INDEX(b->king, m.orig);
    SET_INDEX(b->king, m.dest);
  }
}

void print_pv(board b, pv_line *pv) {
  bool is_black_turn = pv->is_black_turn;
  if (!pv->length) {
    printf("--empty--\n");
  }
  for (int i = 0; i < pv->length; i++) {
    printf("move: %d\n", i);
    move m = pv->moves[i];
    layer captures;
    u64 dummy_zobrist;
    if (is_black_turn) {
      apply_black_move_m(&b, m);
      captures = apply_captures_z_black(&b, &dummy_zobrist, m.dest);
    } else {
      apply_white_move_m(&b, m);
      captures = apply_captures_z_white(&b, &dummy_zobrist, m.dest);
    }
    print_board_move(b, m.orig, m.dest, captures);
  }
}

TEST assert_pv(pv_line (*f)(board), bool is_black_turn, char *board_string,
               int length, ...) {
  board b = read_board(board_string);

  // Build expected PV
  move *moves = malloc(sizeof(move) * length);
  va_list valist;
  va_start(valist, length);
  for (int i = 0; i < length; i++) {
    moves[i] = va_arg(valist, move);
  }
  va_end(valist);
  pv_line expected_pv = {is_black_turn, moves, length};

  // Compute PV using provided function
  pv_line computed_pv = f(b);

  bool equal = true;
  if (!pvs_equal(&expected_pv, &computed_pv)) {
    equal = false;
  }

  print_pv(b, &expected_pv);
  print_pv(b, &computed_pv);

  if (equal) {
    destroy_pv_line(&expected_pv);
    destroy_pv_line(&computed_pv);
    PASS();
  } else {
    printf("expected PV:\n\n");
    print_pv(b, &expected_pv);
    printf("\ncomputed PV:\n\n");
    print_pv(b, &computed_pv);
    destroy_pv_line(&expected_pv);
    destroy_pv_line(&computed_pv);
    FAILm("PVs unequal");
  }
}

SUITE(quiesce_white_suite) {
  RUN_TESTp(assert_pv, quiesce_white_runner, false,
            "     +---------------------------------+"
            " 11  | .  .  X  .  O  X  .  .  .  .  . |"
            " 10  | .  X  .  .  .  .  .  .  .  .  . |"
            "  9  | X  .  .  .  .  .  #  .  .  .  . |"
            "  8  | .  .  .  .  .  .  .  .  .  .  . |"
            "  7  | .  .  .  .  .  .  .  .  .  .  . |"
            "  6  | .  .  .  .  .  .  .  .  .  .  . |"
            "  5  | .  .  .  .  .  .  .  .  .  .  . |"
            "  4  | .  .  .  .  .  .  .  .  .  .  . |"
            "  3  | X  .  .  .  .  .  .  .  .  .  X |"
            "  2  | .  X  .  .  .  .  .  .  .  X  . |"
            "  1  | .  .  X  .  .  .  .  .  X  .  . |"
            "     +---------------------------------+"
            "       a  b  c  d  e  f  g  h  i  j  k  ",
            1, MOVE(9, g, 11, g));
  RUN_TESTp(assert_pv, quiesce_white_runner, false,
            ".  .  X  .  .  .  #  .  .  .  ."
            ".  X  .  .  .  .  .  .  .  .  ."
            "X  .  .  .  .  .  .  .  .  .  ."
            ".  .  .  .  .  .  .  .  .  .  ."
            ".  .  .  .  .  .  .  .  .  .  ."
            ".  .  .  .  .  .  .  .  .  .  ."
            ".  .  .  .  .  .  .  .  .  .  ."
            ".  .  .  .  .  .  .  .  .  .  ."
            "X  .  .  .  .  .  .  .  .  .  X"
            ".  X  .  .  .  .  .  .  .  X  ."
            ".  .  X  .  .  .  .  .  X  .  .",
            0);
};
