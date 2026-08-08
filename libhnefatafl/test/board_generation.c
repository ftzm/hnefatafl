#include "board.h"
#include "test_util.h"
#include "validation.h"

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

static enum theft_trial_res prop_board_is_valid(struct theft *t, void *arg1) {
  (void)t;
  const board *b = (const board *)arg1;
  return board_is_valid(b) ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
}

#define BOARD_PROP_TEST(test_name, prop_fn)                                    \
  PROP_TEST(test_name, prop_fn, &theft_board_info, 1000)

BOARD_PROP_TEST(
    prop_generated_boards_have_valid_piece_counts,
    prop_valid_piece_counts)
BOARD_PROP_TEST(
    prop_generated_boards_have_no_overlapping_pieces,
    prop_no_overlapping_pieces)
BOARD_PROP_TEST(
    prop_generated_boards_have_no_pieces_on_corners,
    prop_no_pieces_on_corners)
BOARD_PROP_TEST(
    prop_generated_boards_have_no_pawns_on_throne,
    prop_no_pawns_on_throne)
BOARD_PROP_TEST(
    prop_generated_boards_have_consistent_rotations,
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
