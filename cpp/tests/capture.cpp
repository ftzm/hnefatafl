#include "../capture.cpp"
#include "../board.cpp"
#include "../move.cpp"
#include "../search.cpp"

#include <iostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include <iterator>
#include <rapidcheck/catch.h>
#include <format>
#include <regex>
#include <tuple>
#include <vector>
#include <algorithm>
#include <optional>

using std::vector;
using std::optional;

// initialize global variables before running tests
class testRunListener : public Catch::EventListenerBase {
public:
  using Catch::EventListenerBase::EventListenerBase;

  void testRunStarting(Catch::TestRunInfo const &) override {
    init_move_globals();
    init_hashes();
  }
};
CATCH_REGISTER_LISTENER(testRunListener)

//******************************************************************************

void test_capture(void (*func)(const layer &, const layer &, layer &, layer &,
                               const unsigned char),
                  const char *input_string, const char *expected_string,
                  unsigned char pos) {
  layer expected = read_layer(expected_string, 'X');
  layer expected_r = rotate_layer(expected);

  layer foes = read_layer(input_string, 'X');
  layer foes_r = rotate_layer(foes);

  layer allies = read_layer(input_string, 'O');
  layer allies_r = rotate_layer(allies);

  func(allies, allies_r, foes, foes_r, pos);

  REQUIRE(stringify(expected) == stringify(foes));
  REQUIRE(stringify(expected_r) == stringify(foes_r));
}

template <bool is_black>
void test_capture_s(const char *input_string, const char *expected_string,
                  unsigned char pos) {

  board inp = read_board(input_string);
  board exp = read_board(expected_string);
  shield_wall<is_black>(&inp, pos);
  REQUIRE(inp == exp);
}

TEST_CASE("capture_u") {
  const char *u_input = ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  O  X  .  X  O  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *u_expected = ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  O  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";
  test_capture(capture_u, u_input, u_expected, 93);
}

TEST_CASE("capture_l") {
  const char *l_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  O  X  .  X  O  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *l_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  O  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";
  test_capture(capture_l, l_input, l_expected, 38);
}

TEST_CASE("capture_x") {

  const char *x_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  O  X  .  X  O  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *x_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  O  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";

  test_capture(capture_x, x_input, x_expected, 60);
}

TEST_CASE("capture_y") {

  const char *y_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  O  X  .  X  O  .  .  ."
                        ".  .  .  .  .  X  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *y_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  O  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";

  test_capture(capture_y, y_input, y_expected, 71);
}

TEST_CASE("capture_r") {
  const char *r_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  O  .  .  .  .  .  .  .  ."
                        ".  .  X  .  .  .  .  .  .  .  ."
                        "O  X  .  X  O  .  .  .  .  .  ."
                        ".  .  X  .  .  .  .  .  .  .  ."
                        ".  .  O  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *r_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  O  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           "O  .  .  .  O  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  O  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";

  test_capture(capture_r, r_input, r_expected, 63);
}

TEST_CASE("capture_R") {
  const char *R_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  O  .  ."
                        ".  .  .  .  .  .  .  .  X  .  ."
                        ".  .  .  .  .  .  O  X  .  X  O"
                        ".  .  .  .  .  .  .  .  X  .  ."
                        ".  .  .  .  .  .  .  .  O  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *R_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  O  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  O  .  .  .  O"
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  O  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";
  test_capture(capture_R, R_input, R_expected, 57);
}

TEST_CASE("capture_b") {
  const char *b_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  O  .  .  ."
                        ".  .  .  .  .  .  .  X  .  .  ."
                        ".  .  .  .  .  O  X  .  X  O  ."
                        ".  .  .  .  .  .  .  X  .  .  ."
                        ".  .  .  .  .  .  .  O  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *b_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";
  test_capture(capture_b, b_input, b_expected, 47);
}

TEST_CASE("capture_B") {
  const char *B_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  O  .  .  ."
                        ".  .  .  .  .  .  .  X  .  .  ."
                        ".  .  .  .  .  O  X  .  X  O  ."
                        ".  .  .  .  .  .  .  X  .  .  ."
                        ".  .  .  .  .  .  .  O  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *B_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  O  .  .  .  O  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  O  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";

  test_capture(capture_B, B_input, B_expected, 80);
}

TEST_CASE("capture_62") {
  const char *c62_input = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  O  .  .  .  .  .  .  ."
                          ".  .  .  X  .  .  .  .  .  .  ."
                          ".  O  X  .  X  O  .  .  .  .  ."
                          ".  .  .  X  .  .  .  .  .  .  ."
                          ".  .  .  O  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";

  const char *c62_expected = ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  O  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  O  .  .  .  O  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  O  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  ."
                             ".  .  .  .  .  .  .  .  .  .  .";

  test_capture(capture_62, c62_input, c62_expected, 62);
}

TEST_CASE("capture_u_corner_west") {
  const char *u_input = ".  X  O  .  .  .  .  .  .  .  ."
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

  const char *u_expected = ".  .  O  .  .  .  .  .  .  .  ."
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
  test_capture(capture_u, u_input, u_expected, 118);
}

TEST_CASE("capture_u corner west lower") {
  const char *u_input = ".  .  .  .  .  .  .  .  .  .  ."
                        "X  .  .  .  .  .  .  .  .  .  ."
                        "O  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *u_expected = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           "O  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  .";
  test_capture(capture_u, u_input, u_expected, 98);
}
TEST_CASE("capture_u_corner_east") {
  const char *u_input = ".  .  .  .  .  .  .  .  O  X  ."
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

  const char *u_expected = ".  .  .  .  .  .  .  .  O  .  ."
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
  test_capture(capture_u, u_input, u_expected, 112);
}

TEST_CASE("capture_l_corner_west") {
  const char *inp = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  X  O  .  .  .  .  .  .  .  .";

  const char *exp = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  O  .  .  .  .  .  .  .  .";
  test_capture(capture_l, inp, exp, 8);
}

TEST_CASE("capture_l_corner_east") {
  const char *inp = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  O  X  .";

  const char *exp = ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  .  .  ."
                           ".  .  .  .  .  .  .  .  O  .  .";
  test_capture(capture_l, inp, exp, 2);
}


TEST_CASE("capture_s_s") {
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

  test_capture_s<false>(s_input, s_expected, 5);
}

TEST_CASE("capture_s_se") {
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

  test_capture_s<false>(se_input, se_expected, 1);
}

TEST_CASE("capture_s_sw") {
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

  test_capture_s<false>(sw_input, sw_expected, 9);
}

TEST_CASE("capture_s_e") {

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
  test_capture_s<false>(e_input, e_expected, 55);
}

TEST_CASE("capture_s_en") {
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
  test_capture_s<false>(en_input, en_expected, 55);
}

TEST_CASE("capture_s_es") {
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

  test_capture_s<false>(es_input, es_expected, 55);
}

TEST_CASE("capture_s_w") {
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

  test_capture_s<false>(w_input, w_expected, 65);
}

TEST_CASE("capture_s_wn") {
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

  test_capture_s<false>(wn_input, wn_expected, 65);
}

TEST_CASE("capture_s_ws") {
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
  test_capture_s<false>(ws_input, ws_expected, 65);
}

TEST_CASE("capture_s_n") {
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

  test_capture_s<false>(n_input, n_expected, 115);
}

TEST_CASE("capture_s_n 2") {
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
  test_capture_s<false>(n_input_2, n_expected_2, 115);
}

TEST_CASE("capture_s_ne") {
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
  test_capture_s<false>(ne_input, ne_expected, 118);
}

TEST_CASE("capture_s_nw") {
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
  test_capture_s<false>(nw_input, nw_expected, 113);
}

TEST_CASE("capture_s_nw king") {
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
  test_capture_s<true>(inp, exp, 113);
}

TEST_CASE("all captures") {
  for (int i = 1; i < 120; i++) {
    SECTION(std::format("capture at destination {}", i)) {
      layer allies = {ally_masks[i][0], ally_masks[i][1]};
      layer allies_r = {ally_masks_r[i][0], ally_masks_r[i][1]};
      layer foes = {foe_masks[i][0], foe_masks[i][1]};
      layer foes_r = {foe_masks_r[i][0], foe_masks_r[i][1]};
      capture_functions[i](allies, allies_r, foes, foes_r, i);
      REQUIRE(stringify(foes) == stringify({0, 0}));
      REQUIRE(stringify(foes_r) == stringify({0, 0}));
    }
  }
}

TEST_CASE("bulk capture destinations") {
  const char *input = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  O  X  O  .  ."
                      ".  .  X  O  .  X  .  .  .  .  ."
                      ".  .  .  X  .  .  .  .  .  X  ."
                      ".  .  .  O  .  .  X  .  .  .  ."
                      ".  .  X  O  .  X  O  .  .  .  ."
                      ".  X  .  .  .  .  .  .  .  .  ."
                      ".  O  .  .  .  .  .  O  X  .  ."
                      ".  .  .  X  .  O  .  .  .  .  ."
                      ".  .  .  .  .  .  X  O  .  .  ."
                      ".  X  O  .  X  .  .  .  .  .  .";

  const char *exp_s = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  X  .  X  .  .  .  X  ."
                      ".  .  .  .  X  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  X  .  .  X  .  .  ."
                      ".  .  .  .  .  .  X  .  .  .  ."
                      ".  .  .  .  .  .  X  .  .  .  ."
                      ".  X  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  X  .  ."
                      ".  .  .  X  .  .  .  .  .  .  .";

  layer allies = read_layer(input, 'X');
  layer foes = read_layer(input, 'O');
  layer exp_l = read_layer(exp_s, 'X');
  layer capture_dests = find_capture_destinations_op(allies, foes);
  REQUIRE(stringify(capture_dests) == stringify(exp_l));
}

TEST_CASE("king excluded from shield wall captures") {
}

TEST_CASE("get_capture_move_boards lower") {
}

TEST_CASE("get_capture_move_boards lower r") {
}

TEST_CASE("get_capture_move_boards upper") {
}

TEST_CASE("get_capture_move_boards upper r") {
}

TEST_CASE("get_capture_move_boards center") {
}

TEST_CASE("get_capture_move_boards center r") {
}

TEST_CASE("get_capture_move_boards rotation is always correct") {
}

TEST_CASE("move indices are within index bounds") {
}

TEST_CASE("move origins are valid") {
}

TEST_CASE("moves are vertically or horizontally aligned") {
}

TEST_CASE("move rotation is always correct") {
}

//TEST_CASE("") {
//  //BENCHMARK
//}

void showValue(const board &board, std::ostream &os) {
  // something about the unicode bars conflicts with the catch2
  // printing, so we replace them with slightly uglier dashes
  auto s = std::regex_replace(pretty_fmt_board(board), std::regex("─"), "-");
  os << '\n' << s << std::endl;
}

template <typename T> bool elem(std::vector<T> v, T item) {
  return std::find(v.begin(), v.end(), item) != v.end();
}

bool corner_pred(uint8_t val) {return val == 0 || val == 10 || val == 110 || val == 120;};

namespace rc {
template <> struct Arbitrary<board> {
  static Gen<board> arbitrary() {
    return gen::exec([] {
      // black
      layer black = {0, 0};
      const size_t black_size = *gen::inRange(1, 25);
      const std::vector<int> black_indices = *gen::unique<std::vector<int>>(
          black_size,
          gen::suchThat(gen::inRange(1, 121), [](int x) {
            return !corner_pred(x) && !(x == 60);
          }));
      for (int i : black_indices) {
        black[sub_layer[i]] |= ((uint64_t)1 << sub_layer_offset_direct[i]);
      }
      layer black_r = rotate_layer(black);

      // white
      layer white = {0, 0};
      const size_t white_size = *gen::inRange(1, 13);
      const std::vector<int> white_indices = *gen::unique<std::vector<int>>(
          black_size,
          gen::suchThat(gen::inRange(1, 121), [black_indices](int x) {
            return !elem(black_indices, x) && !corner_pred(x) && !(x == 60);
          }));
      for (int i : white_indices) {
        white[sub_layer[i]] |= ((uint64_t)1 << sub_layer_offset_direct[i]);
      }
      layer white_r = rotate_layer(white);

      // king
      layer king = {0, 0};
      const int king_index = *gen::suchThat(
          gen::inRange(1, 121), [black_indices, white_indices](int x) {
        return !elem(black_indices, x) && !elem(white_indices, x) && !corner_pred(x);
          });
      king[sub_layer[king_index]] |=
          ((uint64_t)1 << sub_layer_offset_direct[king_index]);
      layer king_r = rotate_layer(king);

      // std::cout << '\n';
      board board = {black, black_r, white, white_r, king, king_r};
      // std::cout << pretty_fmt_board(board);
      return board;
    });
  }
};
} // namespace rc

/*
TEST_CASE("generated board layers don't overlap") {
  rc::prop("test", [](board a) {
    return !(a.white[0] & a.black[0]) && !(a.white[1] & a.black[1]);
  });
}
*/

template <typename T> bool elems_unique(std::vector<T> v) {
  std::set<T> s(v.begin(), v.end());
  return v.size() == s.size();
}

//*****************************************************************************
// Util
//*****************************************************************************

template <typename T>
std::vector<T> filter_v(std::vector<T> v, std::function<bool(T)> f) {
  std::vector<T> result;
  auto it = std::copy_if(v.begin(), v.end(), std::back_inserter(result), f);
  result.shrink_to_fit();
  return result;
}

template <typename T>
bool contains(vector<T> v, T x) {
  return std::find(v.begin(), v.end(), x) != v.end();
}

std::optional<uint8_t> move_north(uint i) {
  return i < 110 ? std::optional{i + 11} : std::nullopt;
}

std::optional<uint8_t> move_south(uint i) {
  return i > 11 ? std::optional{i - 11} : std::nullopt;
}

std::optional<uint8_t> move_east(uint i) {
  return (i % 11) > 0 ? std::optional{i - 1} : std::nullopt;
}

std::optional<uint8_t> move_west(uint i) {
  return (i % 11) < 10 ? std::optional{i + 1} : std::nullopt;
}



//*****************************************************************************
// Capture move boards
//*****************************************************************************
// Util


/** find position of a reachable (no intervening occ pieces) ally in the given direction.
 */
template <auto f>
optional<uint8_t> dir_ally(uint8_t i, layer occ, layer allies) {
  optional<uint8_t> next_move = f(i);
  while (next_move.has_value()) {
    uint8_t val = next_move.value();
    bool is_ally = 
      allies[sub_layer[val]] & ((uint64_t)1 << sub_layer_offset_direct[val]);
    if (is_ally) {
      return val;
    }
    bool is_corner = corner_pred(val);
    bool occupied = 
      occ[sub_layer[val]] & ((uint64_t)1 << sub_layer_offset_direct[val]);
    if (occupied || is_corner) {
      return std::nullopt;
    }
    next_move = f(val);
  }
  return std::nullopt;
}

struct compass_allies {
  vector<move> as_moves(uint8_t dest) const {
    vector<move> result; 
    if (north.has_value()) {
      result.push_back(move{north.value(), dest});
    }
    if (south.has_value()) {
      result.push_back(move{south.value(), dest});
    }
    if (east.has_value()) {
      result.push_back(move{east.value(), dest});
    }
    if (west.has_value()) {
      result.push_back(move{west.value(), dest});
    }
    return result;
  }
  optional<uint8_t> north;
  optional<uint8_t> south;
  optional<uint8_t> east;
  optional<uint8_t> west;
};

compass_allies get_compass_allies(uint i, layer occ, layer allies) {
  return {
    dir_ally<move_north>(i, occ, allies),
    dir_ally<move_south>(i, occ, allies),
    dir_ally<move_east>(i, occ, allies),
    dir_ally<move_west>(i, occ, allies),
  };
}

template <bool is_black>
void test_capture_moves_correct(board *bs, move *ms, int total, board b) {
  // convert to vectors for ease of use
  std::vector<move> moves(ms, ms + total);
  std::vector<board> boards(bs, bs + total);

  layer occ = b.black | b.white | b.king;
  layer allies = name_layer(b, is_black, false);
  layer foes = name_layer(b, !is_black, false);

  layer cd_allies = allies | corners;
  layer cd_foes = foes;
  if constexpr (!is_black) {
    cd_allies[0] |= b.king[0];
    cd_allies[1] |= b.king[1];
  }
  layer capture_dests = find_capture_destinations_op(cd_allies, cd_foes);

  for (uint8_t i = 0; i < 121; i++) {
    if (corner_pred(i)) {
      continue;
    }
    bool is_dest =
        capture_dests[sub_layer[i]] & ((uint64_t)1 << sub_layer_offset_direct[i]);
    if (is_dest) {
      compass_allies correct_allies = get_compass_allies(i, occ, allies);
      if (correct_allies.north.has_value()) {
        uint8_t val = correct_allies.north.value();
        SECTION(std::format("{} is destination - north ally is {}", as_notation(i), as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.south.has_value()) {
        uint8_t val = correct_allies.south.value();
        SECTION(std::format("{} is destination - south ally is {}", as_notation(i), as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.east.has_value()) {
        uint8_t val = correct_allies.east.value();
        SECTION(std::format("{} is destination - east ally is {}", as_notation(i), as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.west.has_value()) {
        uint8_t val = correct_allies.west.value();
        SECTION(std::format("{} is destination - west ally is {}", as_notation(i), as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      SECTION(std::format("{} occupied - no spurious moves", i)) {
        vector<move> found = filter_v<move>(moves, [correct_allies, i](move m) {
          return m.orig == i && !contains(correct_allies.as_moves(i), m);
        });
        REQUIRE(found == vector<move>());
      }
    } else {
      SECTION(std::format("{} empty - no moves", i)) {
        auto from_here =
            filter_v<move>(moves, [i](move m) { return m.orig == i; });
        REQUIRE(from_here == std::vector<move>());
      }
    }
  }
}

//*****************************************************************************
// Black

TEST_CASE("black capture moves are unique") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<true>(boards, a, &total, moves);
    std::vector<std::tuple<char, char>> move_tuples;
    for (int i = 0; i < total; i++) {
      std::tuple<char, char> e = {moves[i].orig, moves[i].dest};
      move_tuples.push_back(e);
    }
    RC_ASSERT(elems_unique(move_tuples));
  });
}

/*
TEST_CASE("black capture moves are within bounds") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<true>(boards, a, &total, moves);
    for (int i = 0; i < total; i++) {
      RC_ASSERT_FALSE(moves[i].orig < 1);
      RC_ASSERT_FALSE(moves[i].dest < 1);
      RC_ASSERT_FALSE(moves[i].orig > 120);
      RC_ASSERT_FALSE(moves[i].dest > 120);
    }
    return true;
  });
}
*/

TEST_CASE("black capture moves and boards match") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<true>(boards, a, &total, moves);
    for (int i = 0; i < total; i++) {
      layer diff = a.black ^ boards[i].black;
      move m = moves[i];
      diff[sub_layer[m.orig]] -= ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      diff[sub_layer[m.dest]] -= ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
      RC_ASSERT_FALSE(diff[0]);
      RC_ASSERT_FALSE(diff[1]);
    }
    return true;
  });
}

TEST_CASE("black capture board rotation correct") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<true>(boards, a, &total, moves);
    for (int i = 0; i < total; i++) {
      layer l = boards[i].black;
      layer r = boards[i].black_r;
      // need std::ranged::equal rather than == so the arrays don't
      // decay to points to the first elements only
      RC_ASSERT(std::ranges::equal(rotate_layer(l), r));
    }
    return true;
  });
}

TEST_CASE("black capture moves are correct") {
  rc::prop("test", [](board b) {
    board bs[100];
    move ms[100];
    int total = 0;
    get_capture_move_boards<true>(bs, b, &total, ms);
    test_capture_moves_correct<true>(bs, ms, total, b);
  });
}

//*****************************************************************************
// White

TEST_CASE("white capture moves are unique") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<false>(boards, a, &total, moves);
    std::vector<std::tuple<char, char>> move_tuples;
    for (int i = 0; i < total; i++) {
      std::tuple<char, char> e = {moves[i].orig, moves[i].dest};
      move_tuples.push_back(e);
    }
    RC_ASSERT(elems_unique(move_tuples));
  });
}

TEST_CASE("white capture moves are within bounds") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<false>(boards, a, &total, moves);
    for (int i = 0; i < total; i++) {
      RC_ASSERT_FALSE(moves[i].orig < 1);
      RC_ASSERT_FALSE(moves[i].dest < 1);
      RC_ASSERT_FALSE(moves[i].orig > 120);
      RC_ASSERT_FALSE(moves[i].dest > 120);
    }
    return true;
  });
}

TEST_CASE("white capture moves and boards match") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<false>(boards, a, &total, moves);
    for (int i = 0; i < total; i++) {
      layer diff = a.white ^ boards[i].white;
      move m = moves[i];
      diff[sub_layer[m.orig]] -= ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      diff[sub_layer[m.dest]] -= ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
      RC_ASSERT_FALSE(diff[0]);
      RC_ASSERT_FALSE(diff[1]);
    }
    return true;
  });
}

TEST_CASE("white capture board rotation correct") {
  rc::prop("test", [](board a) {
    board boards[100];
    move moves[100];
    int total = 0;
    get_capture_move_boards<false>(boards, a, &total, moves);
    for (int i = 0; i < total; i++) {
      layer l = boards[i].white;
      layer r = boards[i].white_r;
      // need std::ranged::equal rather than == so the arrays don't
      // decay to points to the first elements only
      RC_ASSERT(std::ranges::equal(rotate_layer(l), r));
    }
    return true;
  });
}

TEST_CASE("white capture moves are correct") {
  rc::prop("test", [](board b) {
    board bs[100];
    move ms[100];
    int total = 0;
    get_capture_move_boards<false>(bs, b, &total, ms);
    test_capture_moves_correct<false>(bs, ms, total, b);
  });
}

//*****************************************************************************
// King

//*****************************************************************************
// Move boards
//*****************************************************************************

template <auto f>
vector<move> dir_moves(uint8_t i, layer occ, bool is_king = false) {
  vector<move> result;
  optional<uint8_t> next_move = f(i);
  while (next_move.has_value()) {
    uint8_t val = next_move.value();
    bool is_corner = corner_pred(val);
    bool occupied = 
      occ[sub_layer[val]] & ((uint64_t)1 << sub_layer_offset_direct[val]);
    if (occupied || is_corner) {
      break;
    }
    if (!is_king && val == 60) {
      // only king can land in center square
      next_move = f(val);
      continue;
    }
    result.push_back(move{i, val});
    next_move = f(val);
  }
  std::sort(result.begin(), result.end());
  return result;
}

struct compass_moves {
  vector<move> combine() const {
    vector<move> result;
    result.reserve(north.size() + south.size() + east.size() + west.size());
    result.insert(result.end(), north.begin(), north.end());
    result.insert(result.end(), south.begin(), south.end());
    result.insert(result.end(), east.begin(), east.end());
    result.insert(result.end(), west.begin(), west.end());
    // std::sort(result.begin(), result.end());
    return result;
  }
  vector<move> north;
  vector<move> south;
  vector<move> east;
  vector<move> west;
};

compass_moves get_compass_moves(uint i, layer occ, bool is_king = false) {
  return {
    dir_moves<move_north>(i, occ, is_king),
    dir_moves<move_south>(i, occ, is_king),
    dir_moves<move_east>(i, occ, is_king),
    dir_moves<move_west>(i, occ, is_king),
  };
}

void test_moves_correct(board *bs, move *ms, int total, layer movers, layer occ, bool is_king = false) {
  // convert to vectors for ease of use
  std::vector<move> moves(ms, ms + total);
  std::vector<board> boards(bs, bs + total);

  for (uint8_t i = 0; i < 121; i++) {
    if (corner_pred(i)) {
      continue;
    }
    bool is_orig =
        movers[sub_layer[i]] & ((uint64_t)1 << sub_layer_offset_direct[i]);
    if (is_orig) {
      compass_moves correct_moves = get_compass_moves(i, occ, is_king);
      SECTION(std::format("{} occupied - north moves correct", i)) {
        vector<move> found = filter_v<move>(moves, [correct_moves](move m) {
          return contains(correct_moves.north, m);
        });
        std::sort(found.begin(), found.end(), std::less<move>());
        REQUIRE(found == correct_moves.north);
      }
      SECTION(std::format("{} occupied - south moves correct", i)) {
        vector<move> found = filter_v<move>(moves, [correct_moves](move m) {
          return contains(correct_moves.south, m);
        });
        std::sort(found.begin(), found.end());
        REQUIRE(found == correct_moves.south);
      }
      SECTION(std::format("{} occupied - east moves correct", i)) {
        vector<move> found = filter_v<move>(moves, [correct_moves](move m) {
          return contains(correct_moves.east, m);
        });
        std::sort(found.begin(), found.end());
        REQUIRE(found == correct_moves.east);
      }
      SECTION(std::format("{} occupied - west moves correct", i)) {
        vector<move> found = filter_v<move>(moves, [correct_moves](move m) {
          return contains(correct_moves.west, m);
        });
        std::sort(found.begin(), found.end());
        REQUIRE(found == correct_moves.west);
      }
      SECTION(std::format("{} occupied - no spurious moves", i)) {
        vector<move> found = filter_v<move>(moves, [correct_moves, i](move m) {
          return m.orig == i && !contains(correct_moves.combine(), m);
        });
        REQUIRE(found == vector<move>());
      }
    } else {
      SECTION(std::format("{} empty - no moves", i)) {
        auto from_here =
            filter_v<move>(moves, [i](move m) { return m.orig == i; });
        REQUIRE(from_here == std::vector<move>());
      }
    }
  }
}

//*****************************************************************************
// Black

TEST_CASE("black moves correct") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves<true>(b, &total, ms, bs);
    test_moves_correct(bs, ms, total, b.black, b.get_occ());
  });
}

TEST_CASE("black moves correct old") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves_black(b, &total, ms, bs);
    test_moves_correct(bs, ms, total, b.black, b.get_occ());
  });
}

TEST_CASE("black moves and boards match") {
  rc::prop("test", [](const board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves<true>(b, &total, ms, bs);
    for (int i = 0; i < total; i++) {
      move m = ms[i];
      board nb = bs[i];
      layer correct_layer = {b.black[0], b.black[1]};
      // layer correct_layer = {0, 0};
      correct_layer[sub_layer[m.orig]] ^= ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      correct_layer[sub_layer[m.dest]] ^= ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
      REQUIRE(stringify(nb.black) == stringify(correct_layer));
    }
  });
}


//*****************************************************************************
// White

TEST_CASE("white moves correct") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves<false>(b, &total, ms, bs);
    test_moves_correct(bs, ms, total, b.white, b.get_occ());
  });
}

TEST_CASE("white moves correct old") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves_white(b, &total, ms, bs);
    test_moves_correct(bs, ms, total, b.white, b.get_occ());
  });
}

TEST_CASE("white moves and boards match") {
  rc::prop("test", [](const board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves<false>(b, &total, ms, bs);
    for (int i = 0; i < total; i++) {
      move m = ms[i];
      board nb = bs[i];
      layer correct_layer = {b.white[0], b.white[1]};
      // layer correct_layer = {0, 0};
      correct_layer[sub_layer[m.orig]] ^= ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      correct_layer[sub_layer[m.dest]] ^= ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
      REQUIRE(stringify(nb.white) == stringify(correct_layer));
    }
  });
}


//*****************************************************************************
// King

TEST_CASE("king moves correct") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_king_moves(b, &total, ms, bs);
    test_moves_correct(bs, ms, total, b.king, b.get_occ(), true);
  });
}

TEST_CASE("king moves and boards match") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_king_moves(b, &total, ms, bs);
    for (int i = 0; i < total; i++) {
      // layer diff = b.king ^ bs[i].king;
      move m = ms[i];
      board nb = bs[i];
      layer correct_layer = {0, 0};
      correct_layer[sub_layer[m.dest]] |= ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
      REQUIRE(stringify(nb.king) == stringify(correct_layer));
    }
  });
}

TEST_CASE("king move board rotation correct") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_king_moves(b, &total, ms, bs);
    for (int i = 0; i < total; i++) {
      layer l = bs[i].king;
      layer r = bs[i].king_r;
      // need std::ranged::equal rather than == so the arrays don't
      // decay to points to the first elements only
      RC_ASSERT(std::ranges::equal(rotate_layer(l), r));
    }
    return true;
  });
}

TEST_CASE("simple king moves correct") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    get_king_moves_simple(b, &total, ms, bs);
    test_moves_correct(bs, ms, total, b.king, b.get_occ(), true);
  });
}

//*****************************************************************************
// Bench move
//*****************************************************************************

const char* sanity_capture_string_white = \
  " .  .  .  X  X  X  .  X  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  .  .  O  .  O  .  .  . "
  " X  .  .  .  .  .  .  .  .  .  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  X  .  O  O  #  O  .  .  X  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  .  .  .  .  O  .  .  .  .  X "
  " .  .  .  .  X  .  .  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  X  .  X  X  X  .  .  . ";

const board sanity_capture_board_white = read_board(sanity_capture_string_white);


struct split_move_result {
  move moves[235];
  board boards[235];
  int total;
};

TEST_CASE("bench moves", "[benchmark]") {
  int depth = 5;
  /*
  board boards[5] = {
    // rc::gen::arbitrary<board>()(1000, 100).value(),
    // rc::gen::arbitrary<board>()(2000, 100).value(),
    // rc::gen::arbitrary<board>()(3000, 100).value(),
    // rc::gen::arbitrary<board>()(4000, 100).value(),
    // rc::gen::arbitrary<board>()(5000, 100).value(),
    rc::gen::arbitrary<board>()(6000, 100).value(),
    rc::gen::arbitrary<board>()(7000, 100).value(),
    rc::gen::arbitrary<board>()(8000, 100).value(),
    rc::gen::arbitrary<board>()(9000, 100).value(),
    rc::gen::arbitrary<board>()(9999, 100).value(),
  };
  */
  board boards[5] = {
    // rc::gen::arbitrary<board>()(1000, 100).value(),
    // rc::gen::arbitrary<board>()(2000, 100).value(),
    rc::gen::arbitrary<board>()(3000, 100).value(),
    // rc::gen::arbitrary<board>()(4000, 100).value(),
    // rc::gen::arbitrary<board>()(5000, 100).value(),
    rc::gen::arbitrary<board>()(6000, 100).value(),
    // rc::gen::arbitrary<board>()(7000, 100).value(),
    // rc::gen::arbitrary<board>()(8000, 100).value(),
    // rc::gen::arbitrary<board>()(9000, 100).value(),
    rc::gen::arbitrary<board>()(9999, 100).value(),
    start_board,
    sanity_capture_board_white
  };
  split_move_result r;
  /*
  BENCHMARK("black") {
    for (board b : boards) {
      get_team_moves_black(b, &(r.total), r.moves, r.boards);
    }
    return r;
  };
  BENCHMARK("black gen") {
    for (board b : boards) {
      get_team_moves<true>(b, &(r.total), r.moves, r.boards);
    }
    return r;
  };
  BENCHMARK("white") {
    for (board b : boards) {
      get_team_moves_white(b, &(r.total), r.moves, r.boards);
    }
    return r;
  };
  BENCHMARK("white gen") {
    for (board b : boards) {
      get_team_moves<false>(b, &(r.total), r.moves, r.boards);
    }
    return r;
  };
  BENCHMARK("king") {
    for (board b : boards) {
      get_king_moves(b, &(r.total), r.moves, r.boards);
    }
    return r;
  };
  BENCHMARK("king simple") {
    for (board b : boards) {
      get_king_moves_simple(b, &(r.total), r.moves, r.boards);
    }
    return r;
  };
  */
  // BENCHMARK("negamax ab unsorted") {
  //   for (board b : boards) {
  //     auto r = negamax_ab_runner(b, true, depth);
  //   }
  //   // auto r = negamax_ab_runner(start_board, true, 4);
  //   return r;
  // };
  // BENCHMARK("negamax ab sorted") {
  //   for (board b : boards) {
  //     auto r = negamax_ab_sorted_runner(b, true, depth);
  //   }
  //   // auto r = negamax_ab_sorted_runner(start_board, true, 4);
  //   return r;
  // };
  BENCHMARK("negamax ab sorted pv") {
    for (board b : boards) {
      auto r = negamax_ab_sorted_pv_runner(b, true, depth);
    }
    // auto r = negamax_ab_sorted_runner(start_board, true, 4);
    return r;
  };
  // BENCHMARK("negamax ab sorted z") {
  //   for (board b : boards) {
  //     memset(tt, 0, tt_size * sizeof(tt_entry));
  //     auto r = negamax_ab_sorted_z_runner(b, true, depth);
  //   }
  //   // auto r = negamax_ab_sorted_z_runner(start_board, true, 4);
  //   return r;
  // };
  BENCHMARK("negamax ab sorted z iter") {
    for (board b : boards) {
      memset(tt, 0, tt_size * sizeof(tt_entry));
      auto r = negamax_ab_z_iter_runner(b, true, depth);
    }
    return r;
  };
  /*
  */
}

TEST_CASE("hashing results in fewer nodes visited") {
  rc::prop("test", [](board b) {
    // b = start_board;
    memset(tt, 0, tt_size * sizeof(tt_entry));
    int depth = 5;
    int no_hash_tally = 0;
    int hash_tally = 0;
    int iter_hash_tally = 0;
    bool is_black_turn = true;
    uint64_t start_zobrist = hash_for_board(b, is_black_turn);
    z_usage = 0;
    negamax_ab_sorted_z((move){0, 0}, b, start_zobrist, is_black_turn,
                        depth, 0, INT_MIN, INT_MAX, &hash_tally);
    // print_board(b);
    // printf("--------------------\n");
    negamax_ab_sorted((move){0, 0}, b, is_black_turn, depth, INT_MIN,
				 INT_MAX, &no_hash_tally);

    memset(tt, 0, tt_size * sizeof(tt_entry));
    for (int i = 1; i < depth; i++) {
      negamax_ab_z((move){0, 0}, b, start_zobrist, is_black_turn,
                   i, 0,  INT_MIN, INT_MAX, &iter_hash_tally);
    }
    iter_hash_tally = 0;
    negamax_ab_z((move){0, 0}, b, start_zobrist, is_black_turn,
                 depth, 0, INT_MIN, INT_MAX, &iter_hash_tally);

    print_board(b);
    setlocale(LC_NUMERIC, "");
    printf("no hash tally: %'d\n", no_hash_tally);
    printf("hash tally: %'d\n", hash_tally);
    printf("iter hash tally: %'d\n", iter_hash_tally);
    printf("z usage: %'d\n", z_usage);
    printf("\n");
    printf("---------------------------------\n");
    printf("\n");
    return hash_tally < no_hash_tally || z_usage == 0;
  });
}

TEST_CASE("test pv") {
  auto res = negamax_ab_sorted_pv_runner(start_board, true, 4);
  /*
  for (int i = 0; i < MAX_DEPTH; i++) {
    printf("[%d] = %d\n", i, PV_LENGTH[i]);
  }
  */
  for (int i = 0; i < PV_LENGTH[0]; i++) {
    auto m = PV_TABLE[0][i] ;
    std::cout << "\n                == move " << i + 1 << " ==" << "\n";
    std::cout << "move: " << m << "\n";
    std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]), m.orig, m.dest, {0,0});
    std::cout << "[ " << encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
  }
  // print_board(res._board);
  REQUIRE(false);
}

TEST_CASE("test encode") {
  std::string black_string = encode_layer(start_board.black);
  layer black_layer = decode_layer(black_string);
  REQUIRE(stringify(black_layer) == stringify(start_board.black));
}

TEST_CASE("board hash round trip") {
  rc::prop("test", [](board b) {
    std::string board_string = encode_mini(to_mini(b));
    board b2 = decode_mini(board_string).to_full();
    REQUIRE(b == b2);
  });
}

const char* sanity_capture_string = \
  " .  .  .  X  X  X  .  X  .  .  . "
  " .  .  .  .  .  X  O  .  .  .  . "
  " .  .  .  .  .  .  X  .  .  .  . "
  " X  .  .  .  .  O  .  .  .  .  X "
  " X  .  .  .  O  O  .  .  .  .  X "
  " X  X  .  O  O  #  O  O  .  X  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  .  .  .  .  .  .  .  .  .  X "
  " .  .  .  .  X  .  .  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  O "
  " .  .  .  X  .  X  X  X  .  .  . ";

const board sanity_capture_board = read_board(sanity_capture_string);

TEST_CASE("sanity check capture") {
  auto res = negamax_ab_sorted_pv_runner(sanity_capture_board, true, 5);
  /*
  for (int i = 0; i < MAX_DEPTH; i++) {
    printf("[%d] = %d\n", i, PV_LENGTH[i]);
  }
  */
  for (int i = 0; i < PV_LENGTH[0]; i++) {
    auto m = PV_TABLE[0][i] ;
    std::cout << "\n                == move " << i + 1 << " ==" << "\n";
    std::cout << "move: " << m << "\n";
    std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]), m.orig, m.dest, {0,0});
    std::cout << "[ " << encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
  }
  // print_board(res._board);
  std::cout << "score: " << res << "\n";
  REQUIRE(false);
}


TEST_CASE("sanity check capture white") {
  auto res = negamax_ab_sorted_pv_runner(sanity_capture_board_white, true, 1);
  /*
  for (int i = 0; i < MAX_DEPTH; i++) {
    printf("[%d] = %d\n", i, PV_LENGTH[i]);
  }
  */
  for (int i = 0; i < PV_LENGTH[0]; i++) {
    auto m = PV_TABLE[0][i] ;
    std::cout << "\n                == move " << i + 1 << " ==" << "\n";
    std::cout << "move: " << m << "\n";
    std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]), m.orig, m.dest, {0,0});
    std::cout << "[ " << encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
  }
  // print_board(res._board);
  std::cout << "score: " << res << "\n";
  REQUIRE(false);
}

const char* wtf_string = \
  " .  .  .  X  .  X  X  X  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  .  X  .  .  .  .  .  . "
  " X  .  .  .  .  O  .  .  .  .  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  X  .  O  O  #  O  O  .  X  X "
  " X  .  .  .  O  O  .  .  .  .  X "
  " X  .  .  .  .  O  .  .  .  .  X "
  " .  .  .  .  X  .  O  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  X  .  X  X  X  .  .  . ";

const board wtf_board = read_board(wtf_string);

TEST_CASE("wtf") {
  auto res = negamax_ab_sorted_pv_runner(wtf_board, false, 1);
  /*
  for (int i = 0; i < MAX_DEPTH; i++) {
    printf("[%d] = %d\n", i, PV_LENGTH[i]);
  }
  */
  for (int i = 0; i < PV_LENGTH[0]; i++) {
    auto m = PV_TABLE[0][i] ;
    std::cout << "\n                == move " << i + 1 << " ==" << "\n";
    std::cout << "move: " << m << "\n";
    std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]), m.orig, m.dest, {0,0});
    std::cout << "[ " << encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
  }
  // print_board(res._board);
  std::cout << "score: " << res << "\n";
  REQUIRE(false);
}

TEST_CASE("test pv z") {
   auto res = negamax_ab_z_iter_runner(sanity_capture_board, true, 5);
  for (int i = 0; i < MAX_DEPTH; i++) {
    printf("[%d] = %d\n", i, PV_LENGTH[i]);
  }
  /*
  */
  for (int i = 0; i < PV_LENGTH[0]; i++) {
    auto m = PV_TABLE[0][i] ;
    std::cout << "\n                == move " << i + 1 << " ==" << "\n";
    std::cout << "move: " << m << "\n";
    std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]), m.orig, m.dest, {0,0});
    std::cout << "[ " << encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
  }
  // print_board(res._board);
  REQUIRE(false);
}

const char* king_string = \
  " .  .  .  X  .  X  X  X  #  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  .  X  .  .  .  .  .  . "
  " X  .  .  .  .  O  .  .  .  .  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  X  .  O  O  .  .  .  .  X  X "
  " X  .  .  .  O  O  .  .  .  .  X "
  " X  .  .  .  .  O  .  .  .  .  X "
  " .  .  .  .  X  .  O  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  X  .  X  X  X  .  .  . ";

const board king_board = read_board(king_string);

TEST_CASE("king escapes") {
  auto res = negamax_ab_sorted_pv_runner(king_board, false, 2);
  /*
  for (int i = 0; i < MAX_DEPTH; i++) {
    printf("[%d] = %d\n", i, PV_LENGTH[i]);
  }
  */
  for (int i = 0; i < PV_LENGTH[0]; i++) {
    auto m = PV_TABLE[0][i] ;
    std::cout << "\n                == move " << i + 1 << " ==" << "\n";
    std::cout << "move: " << m << "\n";
    std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]), m.orig, m.dest, {0,0});
    std::cout << "[ " << encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
  }
  // print_board(res._board);
  std::cout << "score: " << res << "\n";
  REQUIRE(false);
}

