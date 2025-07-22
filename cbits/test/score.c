#include "score.h"
#include "board.h"
#include "capture.h"
#include "fixtures.h"
#include "greatest.h"
#include "io.h"
#include "layer.h"
#include "move.h"
#include "move_legacy.h"
#include "theft.h"
#include "theft_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool corner_guard_states_equal(corner_guard_state *a, corner_guard_state *b) {
  return a->ne_guard_count == b->ne_guard_count &&
         a->nw_guard_count == b->nw_guard_count &&
         a->se_guard_count == b->se_guard_count &&
         a->sw_guard_count == b->sw_guard_count;
}

bool score_states_equal(score_state *a, score_state *b) {
  return a->score == b->score &&
         corner_guard_states_equal(&a->corner_guard, &b->corner_guard);
}

void print_corner_guard_state(FILE *f, corner_guard_state *cgs) {
  fprintf(f, "ne_guard_count: %d\n", cgs->ne_guard_count);
  fprintf(f, "nw_guard_count: %d\n", cgs->nw_guard_count);
  fprintf(f, "se_guard_count: %d\n", cgs->se_guard_count);
  fprintf(f, "sw_guard_count: %d\n", cgs->sw_guard_count);
}

void print_score_state(FILE *f, score_state *ss) {
  print_corner_guard_state(f, &ss->corner_guard);
  fprintf(f, "score: %d\n", ss->score);
}

typedef struct score_evaluations {
  board b;
  move moves[400];
  board results_boards[400];
  score_state full_score_states[400];
  score_state incremental_score_states[400];
  int total;
} score_evaluations;

static enum theft_trial_res
score_evaluations_equal(struct theft *t, void *arg1) {
  struct score_evaluations *input = (struct score_evaluations *)arg1;

  for (int i; i < input->total; i++) {
    if (!score_states_equal(
            &input->full_score_states[i],
            &input->incremental_score_states[i])) {
      return THEFT_TRIAL_FAIL;
    }
  }
  return THEFT_TRIAL_PASS;
}

void score_evaluations_print_cb(FILE *f, const void *instance, void *env) {
  struct score_evaluations *input = (struct score_evaluations *)instance;

  // print board
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(input->b, output);
  fprintf(f, "%s", output);

  for (int i; i < input->total; i++) {
    if (!score_states_equal(
            &input->full_score_states[i],
            &input->incremental_score_states[i])) {
      print_board_move(
          input->results_boards[i],
          input->moves[i].orig,
          input->moves[i].dest,
          EMPTY_LAYER);
      fprintf(f, "full:\n");
      print_score_state(f, &input->full_score_states[i]);
      fprintf(f, "\ninc:\n");
      print_score_state(f, &input->incremental_score_states[i]);
    }
  }
}

// -----------------------------------------------------------------------------

inline layer unoccupied(const board *b) {
  layer res = LAYER_NEG(board_occ(*b));
  res._[1] &= UPPER_HALF_MASK;
  return res;
}

inline layer unoccupied_r(const board *b) {
  layer res = LAYER_NEG(board_occ_r(*b));
  res._[1] &= UPPER_HALF_MASK;
  return res;
}

inline layer unoccupied_king(const board *b) {
  layer res = LAYER_NEG(king_board_occ(*b));
  res._[1] &= UPPER_HALF_MASK;
  return res;
}

inline layer unoccupied_king_r(const board *b) {
  layer res = LAYER_NEG(king_board_occ(*b));
  res._[1] &= UPPER_HALF_MASK;
  return res;
}

inline layer non_capture(board *b, const layer *captures) {
  return LAYER_XOR(unoccupied(b), (*captures));
}

inline layer non_capture_r(board *b, const layer *captures) {
  return LAYER_XOR(unoccupied_r(b), (*captures));
}

inline layer non_capture_king_r(board *b, const layer *captures) {
  return LAYER_XOR(unoccupied_king_r(b), (*captures));
}

static enum theft_alloc_res
white_scores_no_capture_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  score_weights w = {3, 3, 3, 3, 3, 3, 3, init_default_psts()};
  score_state ss = init_score_state(&w, &b);

  const layer capture_dests =
      find_capture_destinations(b.white, b.black, board_occ(b));
  const layer non_capture_dests =  non_capture(&b, &capture_dests);

    

  const layer capture_dests_r =
      find_capture_destinations(b.white_r, b.black_r, board_occ_r(b));

  const layer non_capture_dests_r = non_capture_r(&b, &capture_dests_r);

  moves_to_t moves = moves_to_white(b, non_capture_dests, non_capture_dests_r);

  score_evaluations results = {.b = b, .total = moves.total};
  for (int i; i < moves.total; i++) {
    move m = moves.ms[i];

    results.moves[i] = m;
    // print_move(m.orig, m.dest);
    // printf("\n");
    board result_board = b;
    LAYER_XOR_ASSG(result_board.white, moves.ls[i]);
    LAYER_XOR_ASSG(result_board.white_r, moves.ls_r[i]);
    results.results_boards[i] = result_board;

    score_state updated_score_state = ss;
    updated_score_state = update_score_state_white_move(
        &w, updated_score_state, m.orig, m.dest);
    results.incremental_score_states[i] = updated_score_state;

    score_state recalculated_score_state = init_score_state(&w, &result_board);
    results.full_score_states[i] = recalculated_score_state;
  }

  struct score_evaluations *output = malloc(sizeof(results));
  *output = results;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST prop_white_scores_no_capture_inc_correct(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = white_scores_no_capture_cb,
      .free = theft_generic_free_cb,
      .print = score_evaluations_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = score_evaluations_equal,
      .type_info = {&info},
      .trials = 500,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------

static enum theft_alloc_res
white_scores_capture_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  score_weights w = {1, 1, 1, 1, 1, 1, 1, init_default_psts()};
  score_state ss = init_score_state(&w, &b);

  const layer capture_dests =
      find_capture_destinations(b.white, b.black, board_occ(b));

  const layer capture_dests_r =
      find_capture_destinations(b.white_r, b.black_r, board_occ_r(b));

  moves_to_t moves = moves_to_white(b, capture_dests, capture_dests_r);

  score_evaluations results = {.b = b, .total = moves.total};
  for (int i; i < moves.total; i++) {
    move m = moves.ms[i];

    results.moves[i] = m;
    board result_board = b;
    LAYER_XOR_ASSG(result_board.white, moves.ls[i]);
    LAYER_XOR_ASSG(result_board.white_r, moves.ls_r[i]);
    apply_captures_niave(
        result_board.white, &result_board.black, &result_board.black_r, m.dest);
    results.results_boards[i] = result_board;
    layer captures = LAYER_XOR(b.black, result_board.black);
    // print_layer(b.black);
    // print_layer(result_board.black);
    // print_layer(captures);

    score_state updated_score_state = ss;
    updated_score_state = update_score_state_white_move_and_capture(
        &w, updated_score_state, m.orig, m.dest, captures);
    results.incremental_score_states[i] = updated_score_state;

    score_state recalculated_score_state = init_score_state(&w, &result_board);
    results.full_score_states[i] = recalculated_score_state;
  }

  struct score_evaluations *output = malloc(sizeof(results));
  *output = results;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST prop_white_scores_capture_inc_correct(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = white_scores_capture_cb,
      .free = theft_generic_free_cb,
      .print = score_evaluations_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = score_evaluations_equal,
      .type_info = {&info},
      .trials = 500,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------

SUITE(score_suite) {
  RUN_TEST(prop_white_scores_no_capture_inc_correct);
  RUN_TEST(prop_white_scores_capture_inc_correct);
}
