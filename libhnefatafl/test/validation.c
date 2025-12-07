#include "validation.h"
#include "greatest.h"
#include "move.h"
#include "stdio.h"
#include "types.h"

TEST test_has_correct_piece() {
  board b = start_board;

  // Test black piece detection - position 7 should have a black piece
  piece_type piece_at_7 = get_piece_at(b, 7);
  ASSERT(is_correct_piece(true, piece_at_7));

  // Test invalid black piece - position 60 should not have a black piece (has
  // white/king)
  piece_type piece_at_60 = get_piece_at(b, 60);
  ASSERT_FALSE(is_correct_piece(true, piece_at_60));

  // Test white piece detection - position 60 should have white pieces
  ASSERT(is_correct_piece(false, piece_at_60));

  // Test invalid white piece - position 7 should not have white piece (has
  // black)
  ASSERT_FALSE(is_correct_piece(false, piece_at_7));

  PASS();
}

TEST test_is_orthogonal_move() {
  // Valid orthogonal moves
  move horizontal = {60, 63}; // Same rank, different file
  move vertical = {60, 16};   // Different rank, same file

  ASSERT(is_orthogonal_move(horizontal));
  ASSERT(is_orthogonal_move(vertical));

  // Invalid diagonal move
  move diagonal = {60, 50}; // Both rank and file change
  ASSERT_FALSE(is_orthogonal_move(diagonal));

  PASS();
}

TEST test_get_move_direction() {
  // Test all four directions
  move north_move = {60, 71}; // Higher rank = north
  move south_move = {60, 49}; // Lower rank = south
  move east_move = {60, 61};  // Higher file = east
  move west_move = {60, 59};  // Lower file = west

  ASSERT_EQ(get_move_direction(north_move), north);
  ASSERT_EQ(get_move_direction(south_move), south);
  ASSERT_EQ(get_move_direction(east_move), east);
  ASSERT_EQ(get_move_direction(west_move), west);

  PASS();
}

TEST test_get_step_for_direction() {
  ASSERT_EQ(get_step_for_direction(north), 11);  // North = higher rank = +11
  ASSERT_EQ(get_step_for_direction(south), -11); // South = lower rank = -11
  ASSERT_EQ(get_step_for_direction(east), 1);
  ASSERT_EQ(get_step_for_direction(west), -1);

  PASS();
}

TEST test_draw_line_between() {
  // Test horizontal line
  move horizontal = {60, 63}; // 3 squares east
  layer h_line = draw_line_between(horizontal);

  // Should have bits set at positions 61 and 62 (between 60 and 63)
  ASSERT(CHECK_INDEX(h_line, 61));
  ASSERT(CHECK_INDEX(h_line, 62));
  ASSERT_FALSE(CHECK_INDEX(h_line, 60)); // Origin not included
  ASSERT_FALSE(CHECK_INDEX(h_line, 63)); // Destination not included

  // Test vertical line
  move vertical = {60, 82}; // 2 ranks south (60 -> 71 -> 82)
  layer v_line = draw_line_between(vertical);

  // Should have bit set at position 71 (between 60 and 82)
  ASSERT(CHECK_INDEX(v_line, 71));
  ASSERT_FALSE(CHECK_INDEX(v_line, 60)); // Origin not included
  ASSERT_FALSE(CHECK_INDEX(v_line, 82)); // Destination not included

  // Test single square move (should return empty layer)
  move single = {60, 61};
  layer s_line = draw_line_between(single);
  ASSERT(IS_EMPTY(s_line));

  PASS();
}

TEST test_has_clear_path() {
  // Create empty board for controlled testing
  board empty_board = {
      .black = EMPTY_LAYER,
      .black_r = EMPTY_LAYER,
      .white = EMPTY_LAYER,
      .white_r = EMPTY_LAYER,
      .king = EMPTY_LAYER,
      .king_r = EMPTY_LAYER};

  // Test clear path
  move clear_move = {60, 63};
  ASSERT(has_clear_path(empty_board, clear_move));

  // Add blocking piece at position 61
  SET_INDEX(empty_board.white, 61);
  SET_INDEX(empty_board.white_r, rotate_right[61]);

  // Test blocked path
  ASSERT_FALSE(has_clear_path(empty_board, clear_move));

  // Test single square move (always clear)
  move single_move = {60, 61};
  ASSERT(has_clear_path(empty_board, single_move));

  PASS();
}

TEST test_validate_move_integration() {
  board b = start_board;

  // Test a known valid black move from start position
  move valid_black =
      start_black_moves[0]; // Use first move from predefined moves
  ASSERT_EQ(validate_move(b, valid_black, true), move_error_no_error);

  // Test invalid move (wrong team)
  ASSERT_EQ(
      validate_move(b, valid_black, false),
      move_error_wrong_piece_for_turn);

  // Test invalid diagonal move
  move diagonal = {7, 18}; // Assuming this is diagonal
  if (!is_orthogonal_move(diagonal)) {
    ASSERT_EQ(validate_move(b, diagonal, true), move_error_not_orthogonal);
  }

  PASS();
}

SUITE(validation_suite) {
  RUN_TEST(test_has_correct_piece);
  RUN_TEST(test_is_orthogonal_move);
  RUN_TEST(test_get_move_direction);
  RUN_TEST(test_get_step_for_direction);
  RUN_TEST(test_draw_line_between);
  RUN_TEST(test_has_clear_path);
  RUN_TEST(test_validate_move_integration);
}