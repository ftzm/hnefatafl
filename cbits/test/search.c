#include "search.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "limits.h"
#include "macro_util.h"
#include "score.h"
#include "stdarg.h"
#include "stdbool.h"

typedef enum result_score {
  VICTORY,
  LOSS,
  INCREASE,
  DECREASE,
  MIDDLING,
  ANY
} result_score;

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
    is_black_turn = !is_black_turn;
  }
}

/* we can signal and empty PV by providing a single move of k1k1 */
TEST assert_pv(
    pv_line (*f)(board),
    bool is_black_turn,
    char *board_string,
    result_score result_score,
    int length,
    ...) {
  board b = read_board(board_string);

  score_weights w = init_default_weights();
  score_state s = init_score_state(&w, &b);
  i32 static_eval;
  if (is_black_turn) {
    static_eval = black_score(&w, &s, &b);
  } else {
    static_eval = white_score(&w, &s, &b);
  }

  // Build expected PV
  move *moves = malloc(sizeof(move) * length);
  va_list valist;
  va_start(valist, length);
  for (int i = 0; i < length; i++) {
    move m = read_move(va_arg(valist, char *));
    if (length == 1 && m.orig == 0 && m.dest == 0) {
      length = 0;
    } else {
      moves[i] = m;
    }
  }
  va_end(valist);
  pv_line expected_pv = {is_black_turn, moves, length};

  // Compute PV using provided function
  pv_line computed_pv = f(b);

  bool equal = true;
  if (!pvs_equal(&expected_pv, &computed_pv)) {
    equal = false;
  }

  if (!equal) {
    printf("expected PV:\n\n");
    print_pv(b, &expected_pv);
    printf("\ncomputed PV:\n\n");
    print_pv(b, &computed_pv);
    destroy_pv_line(&expected_pv);
    destroy_pv_line(&computed_pv);
    FAILm("PVs unequal");
  } else {
    destroy_pv_line(&expected_pv);
    destroy_pv_line(&computed_pv);
  }

  if (result_score == VICTORY) {
    ASSERT_EQ_FMTm("returns victory score", MAX_SCORE, computed_pv.score, "%d");
  } else if (result_score == LOSS) {
    ASSERT_EQ_FMTm("returns loss score", MIN_SCORE, computed_pv.score, "%d");
  } else if (result_score == INCREASE) {
    ASSERT_GTm("returns score increase", computed_pv.score, static_eval);
  } else if (result_score == DECREASE) {
    ASSERT_LTm("returns score decrease", computed_pv.score, static_eval);
  } else if (result_score == MIDDLING) {
    ASSERT_GTm("not losing", computed_pv.score, MIN_SCORE + 100);
    ASSERT_LTm("not winning", computed_pv.score, MAX_SCORE - 100);
  } else if (result_score == ANY) {
  } else {
    printf("you've added a new element you fool");
    exit(1);
  }

  PASS();
}

#define ASSERT_PV(_f, _t, _n, _b, _s, ...)                                     \
  greatest_set_test_suffix(_n);                                                \
  RUN_TESTp(assert_pv, _f, _t, _b, _s, WITH_COUNT(FOR_EACH(STR, __VA_ARGS__)))

#define ASSERT_PV_QUIESCE_WHITE(...)                                           \
  ASSERT_PV(quiesce_white_runner, false, __VA_ARGS__)

#define ASSERT_PV_QUIESCE_BLACK(...)                                           \
  ASSERT_PV(quiesce_black_runner, true, __VA_ARGS__)

#define EMPTY_PV k1k1

/* Tests for quiesce_white which don't rely on black quiescence logic */
SUITE(quiesce_white_only) {
  ASSERT_PV_QUIESCE_WHITE(
      "king escape is victory",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  .  .  .  .  X |"
      "  8  | O  O  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | .  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  .  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  #  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      VICTORY,
      EMPTY_PV);

  ASSERT_PV_QUIESCE_WHITE(
      "white captures black",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  O  .  X  .  . |"
      " 10  | .  X  .  .  .  .  X  .  .  X  . |"
      "  9  | X  .  .  .  .  O  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  #  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      INCREASE,
      f9g9);

  ASSERT_PV_QUIESCE_WHITE(
      "test king will escape in 1 rather than capture",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  .  .  .  .  X |"
      "  8  | O  O  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | .  .  .  .  .  O  .  .  .  .  X |"
      "  2  | .  .  .  .  .  X  .  .  .  X  . |"
      "  1  | .  .  #  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      VICTORY,
      EMPTY_PV);
};

/* Tests for quiesce_white which don't rely on black quiescence logic beyond
 * static evaluation. */
SUITE(quiesce_white_shallow) {
  ASSERT_PV_QUIESCE_WHITE(
      "king captures black",
      "     +---------------------------------+"
      " 11  | .  .  X  .  O  X  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  #  .  .  .  X |"
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
      INCREASE,
      g9g11);

  ASSERT_PV_QUIESCE_WHITE(
      "king will capture against the corner",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  O  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  .  X |"
      "  1  | .  .  X  .  .  .  #  .  .  X  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      VICTORY,
      g1i1);

  ASSERT_PV_QUIESCE_WHITE(
      "white captures black",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  #  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  O  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  X  O  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      INCREASE,
      e4e1);

  ASSERT_PV_QUIESCE_WHITE(
      "white captures against corner",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  #  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  O  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  X  .  .  .  .  O  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      INCREASE,
      g1c1);
}
/* Tests for quiesce_white which rely on full black quiescence logic */
SUITE(quiesce_white_recursive) {
  ASSERT_PV_QUIESCE_WHITE(
      "king pursues escape in 2 rather than capturing",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  .  .  . |"
      " 10  | .  X  .  .  .  .  .  .  O  .  . |"
      "  9  | X  .  .  .  .  .  .  .  .  O  X |"
      "  8  | .  .  .  .  .  .  #  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  X |"
      "  6  | .  .  .  .  .  .  .  .  .  .  O |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      VICTORY,
      g8g11);

  ASSERT_PV_QUIESCE_WHITE(
      "white does the tricky corner move",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  #  X  . |"
      " 10  | .  .  .  .  .  .  .  .  .  .  X |"
      "  9  | X  X  .  .  .  .  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  O  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      VICTORY,
      // There aren't more moves because no black moves beyond this point can
      // prevent an escape, thus none can raise the best score.
      i11i10);

  ASSERT_PV_QUIESCE_WHITE(
      "white finds the tricky corner move",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  .  .  . |"
      " 10  | .  .  .  .  .  .  .  .  .  X  X |"
      "  9  | X  X  .  .  .  .  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  #  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  O  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      VICTORY,
      // There aren't more moves because no black moves beyond this point can
      // prevent an escape, thus none can raise the best score.
      i7i11);
  /*
   */
}

/* Tests for quiesce_black which don't rely on white quiescence logic beyond
 * static evaluation. */
SUITE(quiesce_black_only) {
  ASSERT_PV_QUIESCE_BLACK(
      "black loss when can't block 1-move king escape",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  #  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  O  X |"
      "  9  | X  .  .  .  .  .  .  .  .  .  . |"
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
      LOSS,
      EMPTY_PV);
}

/* Tests for quiesce_black which don't rely on white quiescence logic beyond
 * static evaluation. */
SUITE(quiesce_black_shallow) {
  ASSERT_PV_QUIESCE_BLACK(
      "black performs obvious capture",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  X  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  #  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  X  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  X  O  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      INCREASE,
      f4f1);

  ASSERT_PV_QUIESCE_BLACK(
      "black prevents escape in 1",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  #  .  .  .  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  .  X |"
      "  9  | X  .  .  .  .  .  .  .  .  .  . |"
      "  8  | .  .  .  .  .  .  .  X  O  O  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      ANY,
      h8h11);

  ASSERT_PV_QUIESCE_BLACK(
      "black prevents escape in 2",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  .  .  . |"
      " 10  | .  X  O  .  .  .  .  .  O  X  . |"
      "  9  | X  O  .  .  .  #  .  .  .  O  X |"
      "  8  | O  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | O  .  .  .  .  .  .  .  .  .  O |"
      "  3  | X  O  .  .  .  .  .  .  .  O  X |"
      "  2  | .  X  O  .  .  .  .  .  O  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      ANY,
      c11f11);

  ASSERT_PV_QUIESCE_BLACK(
      "black loss when can't black 1 move king escape",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  #  .  .  .  . |"
      " 10  | .  X  .  .  .  .  .  .  O  .  . |"
      "  9  | X  .  .  .  .  .  .  .  .  O  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  X |"
      "  6  | .  .  .  .  .  .  .  .  .  .  O |"
      "  5  | .  .  .  .  .  .  .  .  .  X  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      LOSS,
      EMPTY_PV);

  ASSERT_PV_QUIESCE_BLACK(
      "black loss when can't block 2 move king escape",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  O  .  .  .  .  . |"
      " 10  | .  X  .  .  .  O  .  .  O  .  . |"
      "  9  | X  .  .  .  .  O  .  .  O  O  X |"
      "  8  | .  .  .  .  .  .  #  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  X |"
      "  6  | .  .  .  .  .  .  .  .  .  .  O |"
      "  5  | .  .  .  .  .  .  .  .  .  X  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      LOSS,
      EMPTY_PV);
}

/* Tests for quiesce_white which rely on full white quiescence logic */
SUITE(quiesce_black_recursive) {
  ASSERT_PV_QUIESCE_BLACK(
      "test black loss when can only block 1 of 2 1 move escapes",
      "     +---------------------------------+"
      " 11  | .  .  .  .  .  #  .  .  .  .  . |"
      " 10  | .  X  O  .  .  .  .  .  O  X  . |"
      "  9  | X  O  .  .  .  .  .  .  .  O  X |"
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
      LOSS,
      EMPTY_PV);

  ASSERT_PV_QUIESCE_BLACK(
      "black loss when can only block 1 of 2 2 move escapes",
      "     +---------------------------------+"
      " 11  | .  .  .  .  .  .  .  .  .  .  . |"
      " 10  | X  .  .  .  .  #  .  .  .  .  X |"
      "  9  | .  .  .  .  .  .  .  .  .  .  . |"
      "  8  | .  O  O  X  .  .  .  X  O  O  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      LOSS,
      EMPTY_PV);

  ASSERT_PV_QUIESCE_BLACK(
      "black survives when can block both 2 move escapes",
      "     +---------------------------------+"
      " 11  | .  .  .  .  .  .  .  .  .  .  . |"
      " 10  | .  X  .  X  .  #  .  X  .  X  . |"
      "  9  | .  .  .  .  .  .  .  .  .  .  . |"
      "  8  | .  O  O  X  .  .  .  X  O  O  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      MIDDLING,
      h10h11);

  ASSERT_PV_QUIESCE_BLACK(
      "black doesn't perform a capture that will result in a king escape",
      "     +---------------------------------+"
      " 11  | .  .  .  .  .  #  .  .  X  .  . |"
      " 10  | .  X  X  .  .  .  .  .  .  X  . |"
      "  9  | X  .  .  .  .  .  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  X  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  X  O  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      INCREASE,
      c10c11);

  ASSERT_PV_QUIESCE_BLACK(
      "test tricky corner causes loss for black",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  .  .  .  #  .  . |"
      " 10  | .  .  .  .  .  .  .  .  .  .  X |"
      "  9  | X  X  .  .  .  .  .  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  X  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  O  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      LOSS,
      // There aren't more moves because no black moves beyond this point can
      // prevent an escape, thus none can raise the best score.
      EMPTY_PV);
}
