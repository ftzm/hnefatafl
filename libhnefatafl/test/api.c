#include "api.h"
#include "greatest.h"
#include "move.h"
#include "stdatomic.h"
#include "stdio.h"
#include "types.h"
#include "zobrist.h"

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

TEST test_search_trusted_black_move() {
  // Test search from starting position with black to move
  compact_board start;
  start_board_extern(&start);

  u64 empty_hashes[] = {};
  _Atomic bool should_stop = false;

  move result_move;
  compact_board result_board;
  u64 result_hash;
  game_status result_status;

  search_trusted(
      &start,
      true,
      empty_hashes,
      0,
      &should_stop,
      &result_move,
      &result_board,
      &result_hash,
      &result_status);

  // Verify the move is a valid black move from the starting position
  bool found_move = false;
  for (int i = 0; i < 116; i++) {
    if (start_black_moves[i].orig
        == result_move.orig
        && start_black_moves[i].dest
        == result_move.dest) {
      found_move = true;
      break;
    }
  }
  ASSERT(found_move);

  // Apply the move manually to verify board and hash
  board original_board = from_compact(&start);
  u64 original_hash = hash_for_board(original_board, true);

  board expected_board =
      apply_black_move_m(original_board, result_move.orig, result_move.dest);
  u64 expected_hash =
      next_hash_black(original_hash, result_move.orig, result_move.dest);
  apply_captures_z_black(&expected_board, &expected_hash, result_move.dest);

  // Verify the updated board matches our manual application
  board result_board_full = from_compact(&result_board);
  ASSERT_EQ(expected_board.black._[0], result_board_full.black._[0]);
  ASSERT_EQ(expected_board.black._[1], result_board_full.black._[1]);
  ASSERT_EQ(expected_board.white._[0], result_board_full.white._[0]);
  ASSERT_EQ(expected_board.white._[1], result_board_full.white._[1]);
  ASSERT_EQ(expected_board.king._[0], result_board_full.king._[0]);
  ASSERT_EQ(expected_board.king._[1], result_board_full.king._[1]);

  // Verify the zobrist hash matches
  ASSERT_EQ(expected_hash, result_hash);

  PASS();
}

TEST test_search_trusted_white_move() {
  // Create a position where it's white's turn by applying one black move
  compact_board start;
  start_board_extern(&start);

  board original_board = from_compact(&start);
  move black_move = start_black_moves[0];

  // Apply black move to get to white's turn
  board after_black =
      apply_black_move_m(original_board, black_move.orig, black_move.dest);
  u64 after_black_hash = next_hash_black(
      hash_for_board(original_board, true),
      black_move.orig,
      black_move.dest);
  apply_captures_z_black(&after_black, &after_black_hash, black_move.dest);

  compact_board white_turn_board = to_compact(&after_black);
  u64 position_history[] = {after_black_hash};
  _Atomic bool should_stop = false;

  move result_move;
  compact_board result_board;
  u64 result_hash;
  game_status result_status;

  search_trusted(
      &white_turn_board,
      false,
      position_history,
      1,
      &should_stop,
      &result_move,
      &result_board,
      &result_hash,
      &result_status);

  // Apply the move manually to verify board and hash
  u64 original_hash_white = hash_for_board(after_black, false);
  board expected_board = after_black;
  u64 expected_hash = original_hash_white;

  u8 king_pos = LOWEST_INDEX(after_black.king);
  if (result_move.orig == king_pos) {
    // King move
    expected_board =
        apply_king_move_m(expected_board, result_move.orig, result_move.dest);
    expected_hash =
        next_hash_king(expected_hash, result_move.orig, result_move.dest);
    apply_captures_z_white(&expected_board, &expected_hash, result_move.dest);
  } else {
    // White pawn move
    expected_board =
        apply_white_move_m(expected_board, result_move.orig, result_move.dest);
    expected_hash =
        next_hash_white(expected_hash, result_move.orig, result_move.dest);
    apply_captures_z_white(&expected_board, &expected_hash, result_move.dest);
  }

  // Verify the updated board matches our manual application
  board result_board_full = from_compact(&result_board);
  ASSERT_EQ(expected_board.black._[0], result_board_full.black._[0]);
  ASSERT_EQ(expected_board.black._[1], result_board_full.black._[1]);
  ASSERT_EQ(expected_board.white._[0], result_board_full.white._[0]);
  ASSERT_EQ(expected_board.white._[1], result_board_full.white._[1]);
  ASSERT_EQ(expected_board.king._[0], result_board_full.king._[0]);
  ASSERT_EQ(expected_board.king._[1], result_board_full.king._[1]);

  // Verify the zobrist hash matches
  ASSERT_EQ(expected_hash, result_hash);

  PASS();
}

SUITE(api_suite) {
  RUN_TEST(test_get_possible_moves);
  RUN_TEST(test_next_game_state);
  RUN_TEST(test_search_trusted_black_move);
  RUN_TEST(test_search_trusted_white_move);
}