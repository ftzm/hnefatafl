#include "search.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "limits.h"
#include "macro_util.h"
#include "position_set.h"
#include "score.h"
#include "stdbool.h"
#include "zobrist.h" // IWYU pragma: export

#ifdef __GNUC__
#ifndef __clang__
#define DO_PRAGMA(x) _Pragma(#x)
#else
#define DO_PRAGMA(x)
#endif
#else
#define DO_PRAGMA(x)
#endif

#define POSITION_SET(...)                                                      \
  ({                                                                           \
    u64 hashes[] = {FOR_EACH_PAIR(CREATE_POSITION, __VA_ARGS__)};              \
    size_t count = sizeof(hashes) / sizeof(u64);                               \
    position_set *set = create_position_set(100);                              \
    int deletion_index;                                                        \
    for (size_t i = 0; i < count; i++) {                                       \
      insert_position(set, hashes[i], &deletion_index);                        \
    }                                                                          \
    set;                                                                       \
  })

#define EMPTY_POSITION_SET create_position_set(100)

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
  ((pv_assertion){.tag = PV_EXPECT,                                            \
                  .move_strings = (char *[]){FOR_EACH(STR, __VA_ARGS__)},      \
                  .length = sizeof((char *[]){FOR_EACH(STR, __VA_ARGS__)}) /   \
                            sizeof(char *)})

// Helper macro to convert string to move
#define READ_MOVE(move_str) read_move(STR(move_str))

// Macro to create a pv struct from a list of moves
#define PREV_PV(...)                                                           \
  (&(pv){.pv_length = {0},                                                     \
         .pv_table = {0},                                                      \
         .prev_pv_length =                                                     \
             sizeof((move[]){FOR_EACH(READ_MOVE, __VA_ARGS__)}) /              \
             sizeof(move),                                                     \
         .prev_pv = {FOR_EACH(READ_MOVE, __VA_ARGS__)}})

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

typedef struct {
  stats_assertion *assertions;
  int length;
} stats_assertions;

#define STATS(...)                                                             \
  ((stats_assertions){.assertions = (stats_assertion[]){__VA_ARGS__},          \
                      .length = sizeof((stats_assertion[]){__VA_ARGS__}) /     \
                                sizeof(stats_assertion)})

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
  {STAT_SEARCH_POSITIONS_BLACK, comp, val}
#define SEARCH_POSITIONS_WHITE(comp, val)                                      \
  {STAT_SEARCH_POSITIONS_WHITE, comp, val}
#define SEARCH_BETA_CUTOFF_BLACK(comp, val)                                    \
  {STAT_SEARCH_BETA_CUTOFF_BLACK, comp, val}
#define SEARCH_BETA_CUTOFF_WHITE(comp, val)                                    \
  {STAT_SEARCH_BETA_CUTOFF_WHITE, comp, val}
#define QUIESCENCE_POSITIONS_BLACK(comp, val)                                  \
  {STAT_QUIESCENCE_POSITIONS_BLACK, comp, val}
#define QUIESCENCE_POSITIONS_WHITE(comp, val)                                  \
  {STAT_QUIESCENCE_POSITIONS_WHITE, comp, val}
#define QUIESCENCE_BETA_CUTOFF_BLACK(comp, val)                                \
  {STAT_QUIESCENCE_BETA_CUTOFF_BLACK, comp, val}
#define QUIESCENCE_BETA_CUTOFF_WHITE(comp, val)                                \
  {STAT_QUIESCENCE_BETA_CUTOFF_WHITE, comp, val}
#define QUIESCENCE_LIMIT_REACHED(comp, val)                                    \
  {STAT_QUIESCENCE_LIMIT_REACHED, comp, val}
#define REPEAT_MOVES_ENCOUNTERED(comp, val)                                    \
  {STAT_REPEAT_MOVES_ENCOUNTERED, comp, val}

typedef struct {
  int score;
  pv_assertion pv;
  stats_assertions stats_assertions;
  int depth;
  bool is_pv;
  position_set *positions;
  i32 alpha;
  i32 beta;
  pv *pv_data;
  score_weights score_weights;
  score_state score_state;
} search_args;

pv_line create_pv_line(pv *pv_data, bool is_black_turn, i32 result) {
  move *moves = malloc(sizeof(move) * pv_data->pv_length[0]);
  memcpy(moves, pv_data->pv_table[0], sizeof(move) * pv_data->pv_length[0]);
  return (pv_line){is_black_turn, moves, pv_data->pv_length[0], result};
}

pv_line quiesce_black_adapter(board b, search_args args, stats *statistics) {
  i32 result = quiesce_black(
      args.pv_data,
      args.positions,
      &args.score_weights,
      args.score_state,
      b,
      hash_for_board(b, false),
      0,
      args.alpha,
      args.beta,
      statistics);
  return create_pv_line(args.pv_data, true, result);
}

pv_line quiesce_white_adapter(board b, search_args args, stats *statistics) {
  i32 result = quiesce_white(
      args.pv_data,
      args.positions,
      &args.score_weights,
      args.score_state,
      b,
      hash_for_board(b, false),
      0,
      args.alpha,
      args.beta,
      statistics);
  return create_pv_line(args.pv_data, true, result);
}

TEST assert_pv(
    pv_line (*f)(board, search_args, stats *),
    bool is_black_turn,
    board b,
    search_args args) {

  i32 static_eval;
  if (is_black_turn) {
    static_eval = black_score(&args.score_weights, &args.score_state, &b);
  } else {
    static_eval = white_score(&args.score_weights, &args.score_state, &b);
  }

  // Build expected PV from assertion
  bool skip_line = (args.pv.tag == PV_IGNORE);
  pv_line expected_pv = {0};

  if (args.pv.tag == PV_EXPECT) {
    move *moves = malloc(sizeof(move) * args.pv.length);
    for (int i = 0; i < args.pv.length; i++) {
      moves[i] = read_move(args.pv.move_strings[i]);
    }
    expected_pv = (pv_line){is_black_turn, moves, args.pv.length, 0};
  }

  // Compute PV using provided function
  stats statistics = {0};
  pv_line computed_pv = f(b, args, &statistics);

  bool equal = true;
  if (!skip_line && !pvs_equal(&expected_pv, &computed_pv)) {
    equal = false;
  }

  if (!equal) {
    print_board(b);
    printf("expected PV:\n\n");
    print_pv(b, &expected_pv);
    printf("\ncomputed PV:\n\n");
    print_pv(b, &computed_pv);
    if (args.pv.tag == PV_EXPECT) {
      destroy_pv_line(&expected_pv);
    }
    destroy_pv_line(&computed_pv);
    FAILm("PVs unequal");
  } else {
    if (args.pv.tag == PV_EXPECT) {
      destroy_pv_line(&expected_pv);
    }
    destroy_pv_line(&computed_pv);
  }

  if (args.score == VICTORY) {
    ASSERT_EQ_FMTm("returns victory score", MAX_SCORE, computed_pv.score, "%d");
  } else if (args.score == LOSS) {
    ASSERT_EQ_FMTm("returns loss score", MIN_SCORE, computed_pv.score, "%d");
  } else if (args.score == INCREASE) {
    ASSERT_GTm("returns score increase", computed_pv.score, static_eval);
  } else if (args.score == DECREASE) {
    ASSERT_LTm("returns score decrease", computed_pv.score, static_eval);
  } else if (args.score == MIDDLING) {
    ASSERT_GTm("not losing", computed_pv.score, MIN_SCORE + 100);
    ASSERT_LTm("not winning", computed_pv.score, MAX_SCORE - 100);
  } else if (args.score == ANY) {
  } else {
    printf("you've added a new element you fool");
    exit(1);
  }

  for (int i = 0; i < args.stats_assertions.length; i++) {
    stats_assertion assertion = args.stats_assertions.assertions[i];
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

  PASS();
}

#define ASSERT_QUIESCE_GENERIC(                                                \
    runner,                                                                    \
    is_black_turn,                                                             \
    test_name,                                                                 \
    board_string,                                                              \
    ...)                                                                       \
  do {                                                                         \
    _Pragma("GCC diagnostic push");                                            \
    _Pragma("GCC diagnostic ignored \"-Woverride-init\"");                     \
    _Pragma("GCC diagnostic ignored \"-Wmissing-field-initializers\"");        \
    /* for some reason it insists the pv is missing braces but I can't see     \
     * how*/                                                                   \
    _Pragma("GCC diagnostic ignored \"-Wmissing-braces\"");                    \
    DO_PRAGMA(GCC diagnostic ignored "-Woverride-init-side-effects")           \
    search_args args = {                                                       \
        .score = ANY,                                                          \
        .pv = IGNORE_PV,                                                       \
        .stats_assertions =                                                    \
            (stats_assertions){.assertions = NULL, .length = 0},               \
        .depth = 1,                                                            \
        .is_pv = false,                                                        \
        .positions = EMPTY_POSITION_SET,                                       \
        .alpha = -INFINITY,                                                    \
        .beta = INFINITY,                                                      \
        .pv_data = &(pv){.pv_length = {0},                                     \
                         .pv_table = {{0}},                                    \
                         .prev_pv = {0},                                       \
                         .prev_pv_length = 0},                                 \
        .score_weights = init_default_weights(),                               \
        __VA_ARGS__};                                                          \
    _Pragma("GCC diagnostic pop") greatest_set_test_suffix(test_name);         \
    board b = read_board(board_string);                                        \
    args.score_state = init_score_state(&args.score_weights, &b);              \
    RUN_TESTp(assert_pv, runner, is_black_turn, b, args);                      \
    destroy_position_set(args.positions);                                      \
  } while (0)

pv_line
search_black_runner_with_stats(board b, search_args args, stats *statistics) {
  i32 result = search_black(
      args.pv_data,
      args.positions,
      &args.score_weights,
      args.score_state,
      b,
      hash_for_board(b, true),
      0,
      args.depth,
      args.alpha,
      args.beta,
      statistics,
      args.is_pv);
  return create_pv_line(args.pv_data, true, result);
}

pv_line
search_white_runner_with_stats(board b, search_args args, stats *statistics) {
  i32 result = search_white(
      args.pv_data,
      args.positions,
      &args.score_weights,
      args.score_state,
      b,
      hash_for_board(b, false),
      0,
      args.depth,
      args.alpha,
      args.beta,
      statistics,
      args.is_pv);
  return create_pv_line(args.pv_data, false, result);
}

#define ASSERT_QUIESCE_WHITE(...)                                              \
  ASSERT_QUIESCE_GENERIC(quiesce_white_adapter, false, __VA_ARGS__)

#define ASSERT_QUIESCE_BLACK(...)                                              \
  ASSERT_QUIESCE_GENERIC(quiesce_black_adapter, true, __VA_ARGS__)

#define ASSERT_SEARCH_WHITE(...)                                               \
  ASSERT_QUIESCE_GENERIC(search_white_runner_with_stats, false, __VA_ARGS__)

#define ASSERT_SEARCH_BLACK(...)                                               \
  ASSERT_QUIESCE_GENERIC(search_black_runner_with_stats, true, __VA_ARGS__)

/* Tests for quiesce_white which don't rely on black quiescence logic */
SUITE(quiesce_white_only) {
  ASSERT_QUIESCE_WHITE(
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
      .stats_assertions = STATS(QUIESCENCE_POSITIONS_WHITE(EQ, 1)));

  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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
  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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
  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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

  ASSERT_QUIESCE_WHITE(
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
  ASSERT_QUIESCE_BLACK(
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
  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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
  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

  ASSERT_QUIESCE_BLACK(
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

SUITE(search_black_shallow) {
  ASSERT_SEARCH_BLACK(
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
      .stats_assertions = STATS(SEARCH_POSITIONS_BLACK(GT, 0)));

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
    ASSERT_SEARCH_BLACK(
        "detects repetition",
        pos,
        .positions = POSITION_SET(pos, true),
        .stats_assertions = STATS(REPEAT_MOVES_ENCOUNTERED(EQ, 1)));
  }

  ASSERT_SEARCH_BLACK(
      "calls quiescence when depth is 0",
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
      .depth = 0,
      .score = MIDDLING,
      .stats_assertions = STATS(
          SEARCH_POSITIONS_BLACK(EQ, 1),
          QUIESCENCE_POSITIONS_BLACK(GT, 0), ));

  // Example of using PREV_PV macro for testing with previous PV
  ASSERT_SEARCH_BLACK(
      "gets a beta cutoff from a PV move when possible",
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
      .beta = MIN_SCORE,
      .is_pv = true,
      .pv_data = PREV_PV(f7f1),
      // With depth 1 and a PV move return after 1 position examined through
      // to white quiescence
      .stats_assertions = STATS(
          SEARCH_POSITIONS_BLACK(EQ, 1),
          SEARCH_POSITIONS_WHITE(EQ, 1),
          QUIESCENCE_POSITIONS_WHITE(EQ, 1),
          SEARCH_BETA_CUTOFF_BLACK(EQ, 1), ));

  // makes obvious capture

  ASSERT_SEARCH_BLACK(
      "makes obvious capture",
      "     +---------------------------------+"
      " 11  | .  X  .  .  .  .  .  .  .  .  . |"
      " 10  | X  .  .  .  .  .  .  .  .  .  X |"
      "  9  | .  .  .  .  .  .  .  .  .  .  . |"
      "  8  | .  .  O  X  .  .  .  .  O  O  . |"
      "  7  | .  .  .  .  .  O  .  .  .  .  . |"
      "  6  | .  .  .  .  O  #  O  .  .  .  . |"
      "  5  | .  X  .  .  .  O  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .pv = PV(b5b8));

  ASSERT_SEARCH_BLACK(
      "takes corner guard position",
      "     +---------------------------------+"
      " 11  | .  X  .  .  .  .  .  .  .  .  . |"
      " 10  | .  X  .  .  .  .  .  .  .  .  X |"
      "  9  | X  .  .  .  .  .  .  .  .  .  . |"
      "  8  | .  O  O  .  .  .  .  .  O  O  . |"
      "  7  | .  .  .  .  .  O  .  .  .  .  . |"
      "  6  | .  .  .  .  O  #  O  .  .  .  . |"
      "  5  | .  .  .  .  .  O  .  .  .  .  . |"
      "  4  | .  .  .  .  .  .  .  .  .  .  . |"
      "  3  | X  .  .  .  .  .  .  .  .  .  X |"
      "  2  | .  X  .  .  .  .  .  .  .  X  . |"
      "  1  | .  .  X  .  .  .  .  .  X  .  . |"
      "     +---------------------------------+"
      "       a  b  c  d  e  f  g  h  i  j  k  ",
      .pv = PV(b11c11));

  // takes trade that results in better position
  //
}
SUITE(search_white_shallow) {
  // white pv beta cutoff
  // king pv beta cutoff
  // king makes a capture
  // king takes a position with greater mobility.
  // white makes a capture
}
