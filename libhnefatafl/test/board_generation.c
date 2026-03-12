#include "board.h"
#include "fixtures.h"
#include "greatest.h"
#include "io.h"
#include "theft.h"
#include "theft_types.h"
#include <stdlib.h>

static enum theft_alloc_res
alloc_board(struct theft *t, void *env, void **instance) {
  (void)env;
  board *b = malloc(sizeof(board));
  if (!b)
    return THEFT_ALLOC_ERROR;
  *b = theft_create_board(t);
  *instance = b;
  return THEFT_ALLOC_OK;
}

static void print_board_cb(FILE *f, const void *instance, void *env) {
  (void)env;
  const board *b = (const board *)instance;
  board_string_t s = to_board_string(*b);
  fprintf(f, "%s", s._);
}

static enum theft_trial_res
prop_valid_piece_counts(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_has_valid_piece_counts(b) ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
}

static enum theft_trial_res
prop_no_overlapping_pieces(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_has_no_overlapping_pieces(b) ? THEFT_TRIAL_PASS
                                            : THEFT_TRIAL_FAIL;
}

static enum theft_trial_res
prop_no_pieces_on_corners(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_has_no_pieces_on_corners(b) ? THEFT_TRIAL_PASS
                                           : THEFT_TRIAL_FAIL;
}

static enum theft_trial_res
prop_no_pawns_on_throne(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_has_no_pawns_on_throne(b) ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
}

static enum theft_trial_res
prop_consistent_rotations(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_has_consistent_rotations(b) ? THEFT_TRIAL_PASS
                                           : THEFT_TRIAL_FAIL;
}

static enum theft_trial_res
prop_board_is_valid(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_is_valid(b) ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
}

static struct theft_type_info board_info = {
    .alloc = alloc_board,
    .free = theft_generic_free_cb,
    .print = print_board_cb,
    .autoshrink_config = {.enable = false},
};

#define BOARD_PROP_TEST(test_name, prop_fn)                                    \
  TEST test_name(void) {                                                       \
    theft_seed seed = theft_seed_of_time();                                    \
    struct theft_run_config config = {                                          \
        .name = __func__,                                                      \
        .prop1 = prop_fn,                                                      \
        .type_info = {&board_info},                                            \
        .trials = 1000,                                                        \
        .seed = seed,                                                          \
    };                                                                         \
    enum theft_run_res res = theft_run(&config);                               \
    ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);          \
    PASS();                                                                    \
  }

BOARD_PROP_TEST(prop_generated_boards_have_valid_piece_counts,
                prop_valid_piece_counts)
BOARD_PROP_TEST(prop_generated_boards_have_no_overlapping_pieces,
                prop_no_overlapping_pieces)
BOARD_PROP_TEST(prop_generated_boards_have_no_pieces_on_corners,
                prop_no_pieces_on_corners)
BOARD_PROP_TEST(prop_generated_boards_have_no_pawns_on_throne,
                prop_no_pawns_on_throne)
BOARD_PROP_TEST(prop_generated_boards_have_consistent_rotations,
                prop_consistent_rotations)
BOARD_PROP_TEST(prop_generated_boards_are_valid, prop_board_is_valid)

SUITE(board_generation_suite) {
  RUN_TEST(prop_generated_boards_have_valid_piece_counts);
  RUN_TEST(prop_generated_boards_have_no_overlapping_pieces);
  RUN_TEST(prop_generated_boards_have_no_pieces_on_corners);
  RUN_TEST(prop_generated_boards_have_no_pawns_on_throne);
  RUN_TEST(prop_generated_boards_have_consistent_rotations);
  RUN_TEST(prop_generated_boards_are_valid);
}
