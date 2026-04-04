#include "capture.h"
#include "assert.h"
#include "board.h"
#include "greatest.h"
#include "io.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "zobrist.h"

// Uses greatest's ASSERT so a board mismatch is reported as a test failure
// rather than killing the entire test runner with exit(1).
#define ASSERT_BOARDS_EQUAL(a, b)                                              \
  do {                                                                         \
    if (!boards_equal(a, b)) {                                                 \
      board_string_t _a_str = to_board_string(a);                              \
      board_string_t _b_str = to_board_string(b);                              \
      printf("Boards not equal:\n%s%s", _a_str._, _b_str._);                  \
      FAIL();                                                                  \
    }                                                                          \
  } while (0)

board swap_sides(board b) {
  return (board){.black = b.white,
                 .black_r = b.white_r,
                 .white = b.black,
                 .white_r = b.black_r,
                 .king = b.king,
                 .king_r = b.king_r};
}

TEST test_shield_wall_capture(
    const char *input,
    const char *expected,
    unsigned char capture_pos,
    int line,
    const char *func) {
  const board exp = read_board(expected);

  board white = read_board(input);
  board black = swap_sides(white);

  // Calculate zobrist hashes for initial board states
  u64 white_zobrist = hash_for_board(white, false);
  u64 black_zobrist = hash_for_board(black, true);

  // Verify the capture position is detected by the capture destinations function
  layer pos_layer = EMPTY_LAYER;
  SET_INDEX(pos_layer, capture_pos);

  layer capture_dests_white = white_capture_destinations(&white);
  if (IS_EMPTY(LAYER_AND(pos_layer, capture_dests_white))) {
    printf(
        "%s (line %d): capture_dests_white missing pos %d\nExpected:\n",
        func, line, capture_pos);
    print_layer(pos_layer);
    printf("Actual capture destinations:\n");
    print_layer(capture_dests_white);
    FAIL();
  }

  layer capture_dests_black = black_capture_destinations(&black);
  if (IS_EMPTY(LAYER_AND(pos_layer, capture_dests_black))) {
    printf(
        "%s (line %d): capture_dests_black missing pos %d\nExpected:\n",
        func, line, capture_pos);
    print_layer(pos_layer);
    printf("Actual capture destinations:\n");
    print_layer(capture_dests_black);
    FAIL();
  }

  // Apply shield wall and verify resulting board state
  shield_wall_white(&white, &white_zobrist, capture_pos);
  ASSERT_BOARDS_EQUAL(exp, white);

  shield_wall_black(&black, &black_zobrist, capture_pos);
  ASSERT_BOARDS_EQUAL(swap_sides(exp), black);

  // Verify zobrist hashes are consistent after capture
  ASSERT_EQm("white zobrist should match after shield wall",
             hash_for_board(white, false), white_zobrist);
  ASSERT_EQm("black zobrist should match after shield wall",
             hash_for_board(black, true), black_zobrist);

  PASS();
}

#define TEST_SHIELD_WALL(a, b, p)                                              \
  test_shield_wall_capture(a, b, p, __LINE__, __FUNCTION__)

TEST test_team(
    bool is_black,
    const char *input,
    const char *expected,
    unsigned char pos) {
  const board exp = read_board(expected);
  board got = read_board(input);
  u64 z = 0;

  if (is_black) {
    shield_wall_black(&got, &z, pos);
  } else {
    shield_wall_white(&got, &z, pos);
  }
  ASSERT_BOARDS_EQUAL(exp, got);

  PASS();
}

#define TEST_SHIELD_WALL_TEAM(is_black, a, b, p) test_team(is_black, a, b, p)

TEST test_shield_wall_south_middle(void) {
  const char *s_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  O  O  O  .  O  O  O  .  ."
                        ".  O  X  X  X  .  X  X  X  O  .";

  const char *s_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  O  O  O  .  O  O  O  .  ."
                           ".  O  .  .  .  .  .  .  .  O  .";

  return TEST_SHIELD_WALL(s_input, s_expected, 5);
}

TEST test_shield_wall_south_left(void) {
  const char *se_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  O  O  .  ."
                         ".  .  .  .  .  .  O  X  X  .  .";

  const char *se_expected = ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  O  O  .  ."
                            ".  .  .  .  .  .  O  .  .  .  .";

  return TEST_SHIELD_WALL(se_input, se_expected, 1);
}

TEST test_shield_wall_south_right(void) {
  const char *sw_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  O  O  O  O  .  .  .  .  ."
                         ".  .  X  X  X  X  O  .  .  .  .";

  const char *sw_expected = ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  O  O  O  O  .  .  .  .  ."
                            ".  .  .  .  .  .  O  .  .  .  .";

  return TEST_SHIELD_WALL(sw_input, sw_expected, 9);
}

TEST test_shield_wall_east_middle(void) {

  const char *e_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  O"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  .  O"
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *e_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  O"
                           ".  .  .  .  .  .  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  .  O"
                           ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL(e_input, e_expected, 55);
}

TEST test_shield_wall_east_right(void) {
  const char *en_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  O"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  .";

  const char *en_expected = ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  O"
                            ".  .  .  .  .  .  .  .  .  O  ."
                            ".  .  .  .  .  .  .  .  .  O  ."
                            ".  .  .  .  .  .  .  .  .  O  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL(en_input, en_expected, 55);
}

TEST test_shield_wall_east_left(void) {
  const char *es_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  .  O"
                         ".  .  .  .  .  .  .  .  .  .  .";

  const char *es_expected = ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  O  ."
                            ".  .  .  .  .  .  .  .  .  O  ."
                            ".  .  .  .  .  .  .  .  .  O  ."
                            ".  .  .  .  .  .  .  .  .  .  O"
                            ".  .  .  .  .  .  .  .  .  .  .";

  return TEST_SHIELD_WALL(es_input, es_expected, 55);
}

TEST test_shield_wall_west_middle(void) {
  const char *w_input = ".  .  .  .  .  .  .  .  .  .  ."
                        "O  .  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "O  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *w_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           "O  .  .  .  .  .  .  .  .  .  ."
                           ".  O  .  .  .  .  .  .  .  .  ."
                           ".  O  .  .  .  .  .  .  .  .  ."
                           ".  O  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  O  .  .  .  .  .  .  .  .  ."
                           ".  O  .  .  .  .  .  .  .  .  ."
                           ".  O  .  .  .  .  .  .  .  .  ."
                           "O  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";

  return TEST_SHIELD_WALL(w_input, w_expected, 65);
}

TEST test_shield_wall_west_right(void) {
  const char *wn_input = ".  .  .  .  .  .  .  .  .  .  ."
                         "O  .  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  .";

  const char *wn_expected = ".  .  .  .  .  .  .  .  .  .  ."
                            "O  .  .  .  .  .  .  .  .  .  ."
                            ".  O  .  .  .  .  .  .  .  .  ."
                            ".  O  .  .  .  .  .  .  .  .  ."
                            ".  O  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  .";

  return TEST_SHIELD_WALL(wn_input, wn_expected, 65);
}

TEST test_shield_wall_west_left(void) {
  const char *ws_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "O  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  .";

  const char *ws_expected = ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  O  .  .  .  .  .  .  .  .  ."
                            ".  O  .  .  .  .  .  .  .  .  ."
                            ".  O  .  .  .  .  .  .  .  .  ."
                            "O  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL(ws_input, ws_expected, 65);
}

TEST test_shield_wall_north_middle(void) {
  const char *n_input = ".  O  X  X  X  .  X  X  X  O  ."
                        ".  .  O  O  O  .  O  O  O  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *n_expected = ".  O  .  .  .  .  .  .  .  O  ."
                           ".  .  O  O  O  .  O  O  O  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";

  return TEST_SHIELD_WALL(n_input, n_expected, 115);
}

TEST test_shield_wall_north_middle_2(void) {
  const char *n_input_2 = ".  .  X  X  X  .  X  X  X  O  ."
                          ".  .  O  O  O  .  O  O  O  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";

  const char *n_expected_2 = ".  .  X  X  X  .  .  .  .  O  ."
                             ".  .  O  O  O  .  O  O  O  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL(n_input_2, n_expected_2, 115);
}

TEST test_shield_wall_north_east(void) {
  const char *ne_input = ".  .  .  X  X  X  X  O  .  .  ."
                         ".  .  .  O  O  O  O  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  .";

  const char *ne_expected = ".  .  .  .  .  .  .  O  .  .  ."
                            ".  .  .  O  O  O  O  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL(ne_input, ne_expected, 118);
}

TEST test_shield_wall_north_left(void) {
  const char *nw_input = ".  .  O  X  X  X  X  .  .  .  ."
                         ".  .  .  O  O  O  O  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  .";

  const char *nw_expected = ".  .  O  .  .  .  .  .  .  .  ."
                            ".  .  .  O  O  O  O  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  ."
                            ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL(nw_input, nw_expected, 113);
}

TEST test_shield_wall_north_left_king(void) {
  const char *inp = ".  .  X  #  O  O  O  .  .  .  ."
                    ".  .  .  X  X  X  X  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";

  const char *exp = ".  .  X  #  .  .  .  .  .  .  ."
                    ".  .  .  X  X  X  X  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_SHIELD_WALL_TEAM(true, inp, exp, 113);
}

TEST test_capture_destinations_runner(
    const char *board_str,
    const char *expected_layer_str,
    bool is_black_turn) {

  board b = read_board(board_str);
  layer expected_layer = read_layer(expected_layer_str, 'X');
  layer actual_layer;

  if (is_black_turn) {
    actual_layer = black_capture_destinations(&b);
  } else {
    actual_layer = white_capture_destinations(&b);
  }

  if (!LAYERS_EQUAL(actual_layer, expected_layer)) {
    printf("Capture destinations test failed\n");
    printf("Expected:\n");
    print_layer(expected_layer);
    printf("Actual:\n");
    print_layer(actual_layer);
    FAIL();
  }

  PASS();
}

#define TEST_CAPTURE_DESTINATIONS(                                             \
    board_str,                                                                 \
    expected_layer_str,                                                        \
    is_black_turn)                                                             \
  test_capture_destinations_runner(board_str, expected_layer_str, is_black_turn)

TEST test_throne_capture_dest_black_occupied(void) {
  const char *board = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  O  #  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  X  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";

  const char *dests = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_CAPTURE_DESTINATIONS(board, dests, true);
}

TEST test_throne_capture_dest_black_unoccupied(void) {
  const char *board = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  #  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  O  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  X  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";

  const char *dests = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  X  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";
  return TEST_CAPTURE_DESTINATIONS(board, dests, true);
}

TEST test_throne_capture_dest_white_occupied(void) {
  const char *board = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  X  #  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  O  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";

  const char *dests = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  X  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";

  return TEST_CAPTURE_DESTINATIONS(board, dests, false);
}

TEST test_throne_capture_dest_white_unoccupied(void) {
  const char *board = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  #  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  X  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  O  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";

  const char *dests = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  X  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";

  return TEST_CAPTURE_DESTINATIONS(board, dests, false);
}

SUITE(capture_suite) {
  RUN_TEST(test_shield_wall_south_middle);
  RUN_TEST(test_shield_wall_south_left);
  RUN_TEST(test_shield_wall_south_right);
  RUN_TEST(test_shield_wall_east_middle);
  RUN_TEST(test_shield_wall_east_right);
  RUN_TEST(test_shield_wall_east_left);
  RUN_TEST(test_shield_wall_west_middle);
  RUN_TEST(test_shield_wall_west_right);
  RUN_TEST(test_shield_wall_west_left);
  RUN_TEST(test_shield_wall_north_middle);
  RUN_TEST(test_shield_wall_north_middle_2);
  RUN_TEST(test_shield_wall_north_east);
  RUN_TEST(test_shield_wall_north_left);
  RUN_TEST(test_shield_wall_north_left_king);
  RUN_TEST(test_throne_capture_dest_black_occupied);
  RUN_TEST(test_throne_capture_dest_black_unoccupied);
  RUN_TEST(test_throne_capture_dest_white_occupied);
  RUN_TEST(test_throne_capture_dest_white_unoccupied);
}
