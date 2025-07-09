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

typedef struct score_evaluations {
  board b;
  move moves[400];
  board results_boards[400];
  i32 full_evaluations[400];
  i32 incremental_evaluations[400];
  int total;
} score_evaluations;

static enum theft_trial_res
score_evaluations_equal(struct theft *t, void *arg1) {
  struct score_evaluations *input = (struct score_evaluations *)arg1;

  for (int i; i < input->total; i++) {
    if (input->full_evaluations[i] != input->incremental_evaluations[i]) {
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
    if (input->full_evaluations[i] != input->incremental_evaluations[i]) {
      print_board_move(
          input->results_boards[i], input->moves[i].orig, input->moves[i].dest, EMPTY_LAYER);
      fprintf(f, "full: %d\n", input->full_evaluations[i]);
      fprintf(f, "inc: %d\n", input->incremental_evaluations[i]);
    }
  }
}

// -----------------------------------------------------------------------------

static enum theft_alloc_res
white_scores_no_capture_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  score_weights w = {3, 3, 3, 3, 3, 3, init_psts()};
  score_state ss = init_score_state(&w, &b);

  const layer non_capture_dests = LAYER_XOR(
      board_occ(b), find_capture_destinations(b.white, b.black, board_occ(b)));

  const layer non_capture_dests_r = LAYER_XOR(
      board_occ_r(b),
      find_capture_destinations(b.white_r, b.black_r, board_occ_r(b)));

  moves_to_t moves = moves_to_white(b, non_capture_dests, non_capture_dests_r);

  score_evaluations results = {.b = b, .total = moves.total};
  for (int i; i < moves.total; i++) {
    move m = moves.ms[i];

    results.moves[i] = m;
    board result_board = b;
    LAYER_XOR_ASSG(result_board.white, moves.ls[i]);
    LAYER_XOR_ASSG(result_board.white_r, moves.ls_r[i]);
    results.results_boards[i] = result_board;

    score_state updated_score_state = ss;
    update_score_state_white_no_capture(&w, &updated_score_state, m.orig, m.dest);
    results.incremental_evaluations[i] =
        white_score(&w, &updated_score_state, &result_board);

    score_state recalculated_score_state = init_score_state(&w, &result_board);
    results.full_evaluations[i] = white_score(&w, &recalculated_score_state, &result_board);
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

  score_weights w = {1, 1, 1, 1, 1, 1, init_psts()};
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
    update_score_state_white_no_capture(&w, &updated_score_state, m.orig, m.dest);
    update_score_state_white_capture(&w, &updated_score_state, captures);
    results.incremental_evaluations[i] =
        white_score(&w, &updated_score_state, &result_board);

    score_state recalculated_score_state = init_score_state(&w, &result_board);
    results.full_evaluations[i] = white_score(&w, &recalculated_score_state, &result_board);
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

SUITE(score_tests) {
  // RUN_TEST(prop_white_scores_no_capture_inc_correct);
  RUN_TEST(prop_white_scores_capture_inc_correct);
  }

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {
  GREATEST_MAIN_BEGIN();
  RUN_SUITE(score_tests);
  GREATEST_MAIN_END();
}
