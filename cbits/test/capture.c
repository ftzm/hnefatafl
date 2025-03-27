#include "assert.h"
#include "capture.h"
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

void test(const char *input, const char *expected, unsigned char pos, int line, const char *func) {
  const board exp = read_board(expected);

  board white = read_board(input);
  board black = reverse_teams(white);

  shield_wall_white_gen(&white, pos);
  assert_boards_equal(exp, white, line, func);

  shield_wall_black_gen(&black, pos);
  assert_boards_equal(reverse_teams(exp), black, line, func);
}

#define TEST(a, b, p) test(a, b, p, __LINE__, __FUNCTION__)

void test_team(bool is_black, const char *input, const char *expected, unsigned char pos, int line, const char *func) {
  const board exp = read_board(expected);

  board got = read_board(input);

  if (is_black) {
    shield_wall_black(&got, pos);
    assert_boards_equal(exp, got, line, func);
  } else {
    shield_wall_white(&got, pos);
    assert_boards_equal(exp, got, line, func);
  }

}

#define TEST_TEAM(is_black, a, b, p) test_team(is_black, a, b, p, __LINE__, __FUNCTION__)

void test_capture_s_m() {
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

  TEST(s_input, s_expected, 5);
}


void test_capture_s_left() {
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

  TEST(se_input, se_expected, 1);
}

void test_capture_s_right() {
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

  TEST(sw_input, sw_expected, 9);
}


void test_capture_e_m() {

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
  TEST(e_input, e_expected, 55);
}

void test_capture_e_r() {
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
  TEST(en_input, en_expected, 55);
}

void test_capture_e_l() {
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

  TEST(es_input, es_expected, 55);
}

void test_capture_w_m() {
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

  TEST(w_input, w_expected, 65);
}

void test_capture_w_r() {
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

  TEST(wn_input, wn_expected, 65);
}

void test_capture_w_l() {
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
  TEST(ws_input, ws_expected, 65);
}

void test_capture_n_m() {
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

  TEST(n_input, n_expected, 115);
}

void test_capture_n_m_2() {
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
  TEST(n_input_2, n_expected_2, 115);
}

void test_capture_n_e() {
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
  TEST(ne_input, ne_expected, 118);
}

void test_capture_n_l() {
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
  TEST(nw_input, nw_expected, 113);
}

void test_capture_n_l_king() {
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
  TEST_TEAM(true, inp, exp, 113);
}


int main() {
  test_capture_s_m();
  test_capture_s_left();
  test_capture_s_right();
  test_capture_e_m();
  test_capture_e_r();
  test_capture_e_l();
  test_capture_w_m();
  test_capture_w_r();
  test_capture_w_l();
  test_capture_n_m();
  test_capture_n_m_2();
  test_capture_n_e();
  test_capture_n_l();
  test_capture_n_l_king ();
  printf("all tests passed\n"); 
  }
