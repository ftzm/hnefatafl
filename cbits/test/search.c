#include "search.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "limits.h"
#include "macro_util.h"
#include "position_set.h"
#include "score.h"
#include "stdbool.h"
#include "zobrist.h"

typedef enum result_score {
  VICTORY,
  LOSS,
  INCREASE,
  DECREASE,
  MIDDLING,
  ANY
} result_score;

typedef enum pv_assertion_tag { PV_IGNORE, PV_EXPECT } pv_assertion_tag;

typedef struct pv_assertion {
  pv_assertion_tag tag;
  char **move_strings; // only valid when tag == PV_EXPECT
  int length;          // only valid when tag == PV_EXPECT
} pv_assertion;

static const pv_assertion IGNORE_PV = {.tag = PV_IGNORE};
static const pv_assertion EMPTY_PV = {
    .tag = PV_EXPECT,
    .move_strings = NULL,
    .length = 0};
#define PV(...)                                                                \
  ((pv_assertion){                                                             \
      .tag = PV_EXPECT,                                                        \
      .move_strings = (char *[]){FOR_EACH(STR, __VA_ARGS__)},                  \
      .length =                                                                \
          sizeof((char *[]){FOR_EACH(STR, __VA_ARGS__)}) / sizeof(char *)})

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
    print_move(m.orig, m.dest);
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

typedef enum {
  STAT_SEARCH_POSITIONS_BLACK,
  STAT_SEARCH_POSITIONS_WHITE,
  STAT_SEARCH_BETA_CUTOFF_BLACK,
  STAT_SEARCH_BETA_CUTOFF_WHITE,
  STAT_QUIESCENCE_POSITIONS_BLACK,
  STAT_QUIESCENCE_POSITIONS_WHITE,
  STAT_QUIESCENCE_BETA_CUTOFF_BLACK,
  STAT_QUIESCENCE_BETA_CUTOFF_WHITE,
  STAT_QUIESCENCE_LIMIT_REACHED,
  STAT_REPEAT_MOVES_ENCOUNTERED
} stats_field;

typedef enum { EQ, GT, LT } comparison;

typedef struct {
  stats_field field;
  comparison comp;
  int value;
} stats_assertion;

const char *stats_field_name(stats_field field) {
  switch (field) {
  case STAT_SEARCH_POSITIONS_BLACK:
    return "search_positions_black";
  case STAT_SEARCH_POSITIONS_WHITE:
    return "search_positions_white";
  case STAT_SEARCH_BETA_CUTOFF_BLACK:
    return "search_beta_cutoff_black";
  case STAT_SEARCH_BETA_CUTOFF_WHITE:
    return "search_beta_cutoff_white";
  case STAT_QUIESCENCE_POSITIONS_BLACK:
    return "quiescence_positions_black";
  case STAT_QUIESCENCE_POSITIONS_WHITE:
    return "quiescence_positions_white";
  case STAT_QUIESCENCE_BETA_CUTOFF_BLACK:
    return "quiescence_beta_cutoff_black";
  case STAT_QUIESCENCE_BETA_CUTOFF_WHITE:
    return "quiescence_beta_cutoff_white";
  case STAT_QUIESCENCE_LIMIT_REACHED:
    return "quiescence_limit_reached";
  case STAT_REPEAT_MOVES_ENCOUNTERED:
    return "repeat_moves_encountered";
  default:
    return "unknown_field";
  }
}

const char *comparison_name(comparison comp) {
  switch (comp) {
  case EQ:
    return "==";
  case GT:
    return ">";
  case LT:
    return "<";
  default:
    return "??";
  }
}

#define SEARCH_POSITIONS_BLACK(comp, val)                                      \
  { STAT_SEARCH_POSITIONS_BLACK, comp, val }
#define SEARCH_POSITIONS_WHITE(comp, val)                                      \
  { STAT_SEARCH_POSITIONS_WHITE, comp, val }
#define SEARCH_BETA_CUTOFF_BLACK(comp, val)                                    \
  { STAT_SEARCH_BETA_CUTOFF_BLACK, comp, val }
#define SEARCH_BETA_CUTOFF_WHITE(comp, val)                                    \
  { STAT_SEARCH_BETA_CUTOFF_WHITE, comp, val }
#define QUIESCENCE_POSITIONS_BLACK(comp, val)                                  \
  { STAT_QUIESCENCE_POSITIONS_BLACK, comp, val }
#define QUIESCENCE_POSITIONS_WHITE(comp, val)                                  \
  { STAT_QUIESCENCE_POSITIONS_WHITE, comp, val }
#define QUIESCENCE_BETA_CUTOFF_BLACK(comp, val)                                \
  { STAT_QUIESCENCE_BETA_CUTOFF_BLACK, comp, val }
#define QUIESCENCE_BETA_CUTOFF_WHITE(comp, val)                                \
  { STAT_QUIESCENCE_BETA_CUTOFF_WHITE, comp, val }
#define QUIESCENCE_LIMIT_REACHED(comp, val)                                    \
  { STAT_QUIESCENCE_LIMIT_REACHED, comp, val }
#define REPEAT_MOVES_ENCOUNTERED(comp, val)                                    \
  { STAT_REPEAT_MOVES_ENCOUNTERED, comp, val }

// Wrapper functions to adapt quiesce runners to the search signature
pv_line quiesce_white_runner_adapter(
    board b,
    int depth,
    bool is_pv,
    stats *statistics,
    position_set *positions) {
  (void)depth;
  (void)is_pv; // Ignore these parameters for quiesce
  return quiesce_white_runner_with_stats(b, statistics, positions);
}

pv_line quiesce_black_runner_adapter(
    board b,
    int depth,
    bool is_pv,
    stats *statistics,
    position_set *positions) {
  (void)depth;
  (void)is_pv; // Ignore these parameters for quiesce
  return quiesce_black_runner_with_stats(b, statistics, positions);
}

TEST assert_pv(
    pv_line (*f)(board, int, bool, stats *, position_set *),
    bool is_black_turn,
    char *board_string,
    result_score result_score,
    pv_assertion pv_assert,
    stats_assertion *stats_assertions,
    int num_stats_assertions,
    int depth,
    bool is_pv,
    position_set *positions) {
  board b = read_board(board_string);

  score_weights w = init_default_weights();
  score_state s = init_score_state(&w, &b);
  i32 static_eval;
  if (is_black_turn) {
    static_eval = black_score(&w, &s, &b);
  } else {
    static_eval = white_score(&w, &s, &b);
  }

  // Build expected PV from assertion
  bool skip_line = (pv_assert.tag == PV_IGNORE);
  pv_line expected_pv = {0};

  if (pv_assert.tag == PV_EXPECT) {
    move *moves = malloc(sizeof(move) * pv_assert.length);
    for (int i = 0; i < pv_assert.length; i++) {
      moves[i] = read_move(pv_assert.move_strings[i]);
    }
    expected_pv = (pv_line){is_black_turn, moves, pv_assert.length, 0};
  }

  // Compute PV using provided function
  stats statistics = {0};
  pv_line computed_pv = f(b, depth, is_pv, &statistics, positions);

  bool equal = true;
  if (!skip_line && !pvs_equal(&expected_pv, &computed_pv)) {
    equal = false;
  }

  if (!equal) {
    printf("expected PV:\n\n");
    print_pv(b, &expected_pv);
    printf("\ncomputed PV:\n\n");
    print_pv(b, &computed_pv);
    if (pv_assert.tag == PV_EXPECT) {
      destroy_pv_line(&expected_pv);
    }
    destroy_pv_line(&computed_pv);
    FAILm("PVs unequal");
  } else {
    if (pv_assert.tag == PV_EXPECT) {
      destroy_pv_line(&expected_pv);
    }
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

  if (stats_assertions != NULL && num_stats_assertions > 0) {
    // Use the statistics already collected from the main function call above
    // (statistics variable from line 227)

    for (int i = 0; i < num_stats_assertions; i++) {
      stats_assertion assertion = stats_assertions[i];
      int actual_value = 0;
      switch (assertion.field) {
      case STAT_SEARCH_POSITIONS_BLACK:
        actual_value = statistics.search_positions_black;
        break;
      case STAT_SEARCH_POSITIONS_WHITE:
        actual_value = statistics.search_positions_white;
        break;
      case STAT_SEARCH_BETA_CUTOFF_BLACK:
        actual_value = statistics.search_beta_cutoff_black;
        break;
      case STAT_SEARCH_BETA_CUTOFF_WHITE:
        actual_value = statistics.search_beta_cutoff_white;
        break;
      case STAT_QUIESCENCE_POSITIONS_BLACK:
        actual_value = statistics.quiescence_positions_black;
        break;
      case STAT_QUIESCENCE_POSITIONS_WHITE:
        actual_value = statistics.quiescence_positions_white;
        break;
      case STAT_QUIESCENCE_BETA_CUTOFF_BLACK:
        actual_value = statistics.quiencence_beta_cutoff_black;
        break;
      case STAT_QUIESCENCE_BETA_CUTOFF_WHITE:
        actual_value = statistics.quiencence_beta_cutoff_white;
        break;
      case STAT_QUIESCENCE_LIMIT_REACHED:
        actual_value = statistics.quiescence_limit_reached;
        break;
      case STAT_REPEAT_MOVES_ENCOUNTERED:
        actual_value = statistics.repeat_moves_encountered;
        break;
      }
      switch (assertion.comp) {
      case EQ:
        ASSERT_EQ_FMTm(
            stats_field_name(assertion.field),
            assertion.value,
            actual_value,
            "%d");
        break;
      case GT:
        ASSERT_GTm(
            stats_field_name(assertion.field),
            actual_value,
            assertion.value);
        break;
      case LT:
        ASSERT_LTm(
            stats_field_name(assertion.field),
            actual_value,
            assertion.value);
        break;
      }
    }
  }

  PASS();
}

#define ASSERT_PV(_f, _t, _n, _b, _s, _pv_assert)                              \
  greatest_set_test_suffix(_n);                                                \
  RUN_TESTp(assert_pv, _f, _t, _b, _s, _pv_assert, NULL, 0, 1, false, NULL)

#define ASSERT_PV_QUIESCE_GENERIC(                                             \
    runner,                                                                    \
    is_black_turn,                                                             \
    test_name,                                                                 \
    board_string,                                                              \
    ...)                                                                       \
  do {                                                                         \
    _Pragma("GCC diagnostic push");                                            \
    _Pragma("GCC diagnostic ignored \"-Woverride-init\"");                     \
    struct {                                                                   \
      int score;                                                               \
      pv_assertion pv;                                                         \
      stats_assertion stats_assertions[32];                                    \
      int depth;                                                               \
      bool is_pv;                                                              \
      position_set *positions;                                                 \
    } args = {                                                                 \
        .score = ANY,                                                          \
        .pv = IGNORE_PV,                                                       \
        .stats_assertions = {{0}},                                             \
        .depth = 1,                                                            \
        .is_pv = false,                                                        \
        .positions = NULL,                                                     \
        __VA_ARGS__};                                                          \
    _Pragma("GCC diagnostic pop") greatest_set_test_suffix(test_name);         \
    int num_stats_assertions = 0;                                              \
    while (num_stats_assertions < 32 &&                                        \
           (args.stats_assertions[num_stats_assertions].field != 0 ||          \
            args.stats_assertions[num_stats_assertions].comp != 0 ||           \
            args.stats_assertions[num_stats_assertions].value != 0)) {         \
      num_stats_assertions++;                                                  \
    }                                                                          \
    RUN_TESTp(                                                                 \
        assert_pv,                                                             \
        runner,                                                                \
        is_black_turn,                                                         \
        board_string,                                                          \
        args.score,                                                            \
        args.pv,                                                               \
        num_stats_assertions > 0 ? args.stats_assertions : NULL,               \
        num_stats_assertions,                                                  \
        args.depth,                                                            \
        args.is_pv,                                                            \
        args.positions);                                                       \
  } while (0)

#define ASSERT_PV_QUIESCE_WHITE(...)                                           \
  ASSERT_PV_QUIESCE_GENERIC(quiesce_white_runner_adapter, false, __VA_ARGS__)

#define ASSERT_PV_QUIESCE_BLACK(...)                                           \
  ASSERT_PV_QUIESCE_GENERIC(quiesce_black_runner_adapter, true, __VA_ARGS__)

#define ASSERT_PV_SEARCH_WHITE(...)                                            \
  ASSERT_PV_QUIESCE_GENERIC(search_white_runner_with_stats, false, __VA_ARGS__)

#define ASSERT_PV_SEARCH_BLACK(...)                                            \
  ASSERT_PV_QUIESCE_GENERIC(search_black_runner_with_stats, true, __VA_ARGS__)

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
      .score = VICTORY,
      .pv = EMPTY_PV,
      .stats_assertions = {QUIESCENCE_POSITIONS_WHITE(EQ, 1)});

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
      .score = INCREASE,
      .pv = PV(f9g9));

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
      .score = VICTORY,
      .pv = EMPTY_PV);
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
      .score = INCREASE,
      .pv = PV(g9g11));

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
      .score = VICTORY,
      .pv = PV(g1i1));

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
      .score = INCREASE,
      .pv = PV(e4e1));

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
      .score = INCREASE,
      .pv = PV(g1c1));
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
      .score = VICTORY,
      .pv = PV(g8g11));

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
      .score = VICTORY
      // There aren't more moves because no black moves beyond this point can
      // prevent an escape, thus none can raise the best score.
  );

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
      .score = VICTORY
      // There aren't more moves because no black moves beyond this point can
      // prevent an escape, thus none can raise the best score.
  );

  ASSERT_PV_QUIESCE_WHITE(
      "white prevents escape from being blocked",
      "     +---------------------------------+"
      " 11  | .  .  X  O  .  .  .  .  .  .  . |"
      " 10  | .  X  .  O  .  .  O  .  .  O  X |"
      "  9  | X  .  .  O  .  .  O  .  .  O  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  #  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  O  .  . |"
      "  5  | .  .  .  .  .  .  .  .  O  O  O |"
      "  4  | .  .  .  .  .  .  .  X  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .score = VICTORY);
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
      .score = LOSS,
      .pv = EMPTY_PV);
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
      .score = INCREASE,
      .pv = PV(f4f1));

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
      .score = ANY,
      .pv = PV(h8h11));

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
      .score = ANY,
      .pv = PV(c11f11));

  ASSERT_PV_QUIESCE_BLACK(
      "black loss when can't block 1 move king escape",
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
      .score = LOSS,
      .pv = EMPTY_PV);

  ASSERT_PV_QUIESCE_BLACK(
      "black loss when can't block 2 move king escape",
      "     +---------------------------------+"
      " 11  | .  .  X  .  .  O  .  .  .  .  . |"
      " 10  | .  X  .  .  .  O  .  .  O  .  . |"
      "  9  | X  .  .  .  .  O  .  .  O  O  X |"
      "  8  | .  .  .  .  .  .  #  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  O  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  O |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .score = LOSS,
      .pv = EMPTY_PV);
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
      .score = LOSS,
      .pv = EMPTY_PV);

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
      .score = LOSS,
      .pv = EMPTY_PV);

  ASSERT_PV_QUIESCE_BLACK(
      "black survives when can block both 2 move escapes",
      "     +---------------------------------+"
      " 11  | .  .  .  .  .  .  .  .  .  .  . |"
      " 10  | .  X  .  X  .  #  .  X  .  X  . |"
      "  9  | X  .  .  .  .  .  .  .  .  .  X |"
      "  8  | .  O  O  .  .  .  .  .  O  O  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .score = MIDDLING);

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
      .score = INCREASE,
      .pv = PV(c10c11));

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
      .score = LOSS,
      // There aren't more moves because no black moves beyond this point can
      // prevent an escape, thus none can raise the best score.
      .pv = EMPTY_PV);

  ASSERT_PV_QUIESCE_BLACK(
      "black finds 2-move blocking move around obstacle",
      "     +---------------------------------+"
      " 11  | .  .  X  .  O  .  .  .  .  .  . |"
      " 10  | .  X  .  .  O  .  O  .  .  .  X |"
      "  9  | X  .  .  .  O  .  O  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  #  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  O  O  . |"
      "  3  | X  .  .  .  O  .  .  .  .  .  X |"
      "  2  | .  X  .  .  O  .  .  .  .  X  . |"
      "  1  | .  .  X  .  O  .  X  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .score = MIDDLING);
}

SUITE(search_black_suite) {
  ASSERT_PV_SEARCH_BLACK(
      "detects white victory",
      "     +---------------------------------+"
      " 11  | .  .  X  .  O  .  .  .  .  .  # |"
      " 10  | .  X  .  .  O  .  O  .  .  .  X |"
      "  9  | X  .  .  .  O  .  O  .  .  .  X |"
      "  8  | .  .  .  .  .  .  .  .  .  .  . |"
      "  7  | .  .  .  .  .  .  .  .  .  .  . |"
      "  6  | .  .  .  .  .  .  .  .  .  .  . |"
      "  5  | .  .  .  .  .  .  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  O  O  . |"
      "  3  | X  .  .  .  O  .  .  .  .  .  X |"
      "  2  | .  X  .  .  O  .  .  .  .  X  . |"
      "  1  | .  .  X  .  O  .  X  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .score = LOSS,
      .stats_assertions = {SEARCH_POSITIONS_BLACK(GT, 0)});

  {
    char pos[] = "     +---------------------------------+"
                 " 11  | .  .  X  .  O  .  .  .  .  .  . |"
                 " 10  | .  X  .  .  O  .  O  .  .  .  X |"
                 "  9  | X  .  .  .  O  .  O  .  .  .  X |"
                 "  8  | .  .  .  .  .  .  .  .  .  .  . |"
                 "  7  | .  .  .  .  .  .  .  .  .  .  . |"
                 "  6  | .  .  .  .  .  #  .  .  .  .  . |"
                 "  5  | .  .  .  .  .  .  .  .  .  .  . |"
                 "  4  | .  .  .  .  .  .  .  .  O  O  . |"
                 "  3  | X  .  .  .  O  .  .  .  .  .  X |"
                 "  2  | .  X  .  .  O  .  .  .  .  X  . |"
                 "  1  | .  .  X  .  O  .  X  .  X  .  . |"
                 "     +---------------------------------+"
                 "       a  b  c  d  e  f  g  h  i  j  k  ";
    ASSERT_PV_SEARCH_BLACK(
        "detects repetition",
        pos,
        .positions = POSITION_SET(pos, true),
        .stats_assertions = {REPEAT_MOVES_ENCOUNTERED(EQ, 1)});
  }

  // calls quiescence when depth is 0
  // returns pv move (when over beta)
}
