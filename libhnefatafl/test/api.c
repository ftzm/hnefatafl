#include "api.h"
#include "greatest.h"
#include "move.h"
#include "stdio.h"
#include "types.h"

TEST test_get_possible_moves() {

  // Get the first black move from start_black_moves
  move first_move = start_black_moves[0];

  // Create move history with just one move
  move history[1] = {first_move};

  // Call next_game_state
  int move_count = 0;
  game_status gs;
  move *possible_moves = NULL;
  move_validation_result result;
  next_game_state_with_moves(
      history,
      1,
      &possible_moves,
      &move_count,
      &gs,
      &result,
      false);

  if (result.error != move_error_no_error) {
    printf(
        "ERROR: next_game_state_with_moves failed with error %d at move %d\n",
        result.error,
        result.move_index);
    FAIL();
  }

  if (possible_moves == NULL) {
    printf("ERROR: next_game_state_with_moves returned NULL moves\n");
    FAIL();
  }

  // Should have some moves for white after black's first move
  ASSERT(move_count > 0);

  // Clean up
  free(possible_moves);

  PASS();
}

TEST test_next_game_state() {
  // Get the first black move from start_black_moves
  move first_move = start_black_moves[0];

  // Create move history with just one move
  move history[1] = {first_move};

  // Call next_game_state (just status, no moves)
  game_status gs;
  move_validation_result result;
  next_game_state(history, 1, &gs, &result, false);

  if (result.error != move_error_no_error) {
    printf(
        "ERROR: next_game_state failed with error %d at move %d\n",
        result.error,
        result.move_index);
    FAIL();
  }

  // After black's first move, game should still be ongoing
  ASSERT_EQ(gs, status_ongoing);

  PASS();
}

SUITE(api_suite) {
  RUN_TEST(test_get_possible_moves);
  RUN_TEST(test_next_game_state);
}