#include "assert.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "stdbool.h"

void assert_boards_equal(board a, board b, int line, const char *func) {
  if (!boards_equal(a, b)) {

    char a_str[strlen(base) + 1];
    strcpy(a_str, base);
    fmt_board(a, a_str);

    char b_str[strlen(base) + 1];
    strcpy(b_str, base);
    fmt_board(b, b_str);

    printf("Boards not equal in function %s at line %d:\n%s%s", func, line, a_str, b_str);
    exit(1);
  }
}

board reverse_teams(board b) {
  return (board){
      .black = b.white,
      .black_r = b.white_r,
      .white = b.black,
      .white_r = b.black_r,
      .king = b.king,
      .king_r = b.king_r};
}

TEST test(const char *input, const char *expected, unsigned char pos, int line, const char *func) {
  const board exp = read_board(expected);

  board white = read_board(input);
  board black = reverse_teams(white);

  shield_wall_white_gen(&white, pos);
  assert_boards_equal(exp, white, line, func);

  shield_wall_black_gen(&black, pos);
  assert_boards_equal(reverse_teams(exp), black, line, func);
  
  PASS();
}

#define TEST_SHIELD_WALL(a, b, p) test(a, b, p, __LINE__, __FUNCTION__)

TEST test_team(bool is_black, const char *input, const char *expected, unsigned char pos, int line, const char *func) {
  const board exp = read_board(expected);

  board got = read_board(input);

  if (is_black) {
    shield_wall_black(&got, pos);
    assert_boards_equal(exp, got, line, func);
  } else {
    shield_wall_white(&got, pos);
    assert_boards_equal(exp, got, line, func);
  }

  PASS();
}

#define TEST_SHIELD_WALL_TEAM(is_black, a, b, p) test_team(is_black, a, b, p, __LINE__, __FUNCTION__)

TEST test_capture_s_m(void) {
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


TEST test_capture_s_left(void) {
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

TEST test_capture_s_right(void) {
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


TEST test_capture_e_m(void) {

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

TEST test_capture_e_r(void) {
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

TEST test_capture_e_l(void) {
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

TEST test_capture_w_m(void) {
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

TEST test_capture_w_r(void) {
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

TEST test_capture_w_l(void) {
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

TEST test_capture_n_m(void) {
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

TEST test_capture_n_m_2(void) {
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

TEST test_capture_n_e(void) {
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

TEST test_capture_n_l(void) {
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

TEST test_capture_n_l_king(void) {
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

SUITE(capture_suite) {
  RUN_TEST(test_capture_s_m);
  RUN_TEST(test_capture_s_left);
  RUN_TEST(test_capture_s_right);
  RUN_TEST(test_capture_e_m);
  RUN_TEST(test_capture_e_r);
  RUN_TEST(test_capture_e_l);
  RUN_TEST(test_capture_w_m);
  RUN_TEST(test_capture_w_r);
  RUN_TEST(test_capture_w_l);
  RUN_TEST(test_capture_n_m);
  RUN_TEST(test_capture_n_m_2);
  RUN_TEST(test_capture_n_e);
  RUN_TEST(test_capture_n_l);
  RUN_TEST(test_capture_n_l_king);
}
