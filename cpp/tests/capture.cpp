#include "../lib/capture.cpp"
#include "../lib/board.cpp"
#include "../lib/move.cpp"
#include "../lib/search.cpp"

#include <iostream>

#include <algorithm>
#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include <format>
#include <iterator>
#include <optional>
#include <rapidcheck/catch.h>
#include <regex>
#include <tuple>
#include <vector>

using std::optional;
using std::vector;

// initialize global variables before running tests
class testRunListener : public Catch::EventListenerBase {
public:
  using Catch::EventListenerBase::EventListenerBase;

  void testRunStarting(Catch::TestRunInfo const &) override {
    init_move_globals();
  }
};
CATCH_REGISTER_LISTENER(testRunListener)

//******************************************************************************

template <bool is_black>
void test_capture_s(
    const char *input_string, const char *expected_string, unsigned char pos) {

  board inp = read_board(input_string);
  board exp = read_board(expected_string);
  shield_wall<is_black>(&inp, pos);
  REQUIRE(inp == exp);
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
      apply_captures_niave_count(allies, foes, foes_r, i);
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
  layer capture_dests =
      find_capture_destinations_op(allies, foes, allies | foes);
  REQUIRE(stringify(capture_dests) == stringify(exp_l));
}

TEST_CASE("find neighbor accurate") {
  uint16_t occ = 0b11011111101;
  int pos = 1;
  uint16_t neighbors = find_neighbors(occ, pos);
  REQUIRE(neighbors == 0b00000000101);
}

TEST_CASE("get_center_row correct") {
  const char *input = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      "X  X  X  X  X  X  X  X  X  X  X"
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";
  layer input_layer = read_layer(input, 'X');
  uint16_t center = get_center_row(input_layer);
  uint16_t expected = 0b11111111111;
  REQUIRE(center == expected);
}

TEST_CASE("king excluded from shield wall captures") {}

TEST_CASE("get_capture_move_boards lower") {}

TEST_CASE("get_capture_move_boards lower r") {}

TEST_CASE("get_capture_move_boards upper") {}

TEST_CASE("get_capture_move_boards upper r") {}

TEST_CASE("get_capture_move_boards center") {}

TEST_CASE("get_capture_move_boards center r") {}

TEST_CASE("get_capture_move_boards rotation is always correct") {}

TEST_CASE("move indices are within index bounds") {}

TEST_CASE("move origins are valid") {}

TEST_CASE("moves are vertically or horizontally aligned") {}

TEST_CASE("move rotation is always correct") {}

// TEST_CASE("") {
//   //BENCHMARK
// }

void showValue(const board &board, std::ostream &os) {
  // something about the unicode bars conflicts with the catch2
  // printing, so we replace them with slightly uglier dashes
  auto s = std::regex_replace(basic_fmt_board(board), std::regex("─"), "-");
  os << '\n' << s << std::endl;
}

template <typename T> bool elem(std::vector<T> v, T item) {
  return std::find(v.begin(), v.end(), item) != v.end();
}

bool corner_pred(uint8_t val) {
  return val == 0 || val == 10 || val == 110 || val == 120;
};

namespace rc {
template <> struct Arbitrary<board> {
  static Gen<board> arbitrary() {
    return gen::exec([] {
      // black
      layer black = {0, 0};
      const size_t black_size = *gen::inRange(1, 25);
      const std::vector<int> black_indices = *gen::unique<std::vector<int>>(
          black_size, gen::suchThat(gen::inRange(1, 120), [](int x) {
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
          gen::suchThat(gen::inRange(1, 120), [black_indices](int x) {
            return !elem(black_indices, x) && !corner_pred(x) && !(x == 60);
          }));
      for (int i : white_indices) {
        white[sub_layer[i]] |= ((uint64_t)1 << sub_layer_offset_direct[i]);
      }
      layer white_r = rotate_layer(white);

      // king
      layer king = {0, 0};
      const int king_index = *gen::suchThat(
          gen::inRange(1, 120), [black_indices, white_indices](int x) {
            return !elem(black_indices, x) && !elem(white_indices, x) &&
                   !corner_pred(x);
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

template <typename T> bool contains(vector<T> v, T x) {
  return std::find(v.begin(), v.end(), x) != v.end();
}

std::optional<uint8_t> move_north(uint i) {
  return i < 110 ? std::optional{i + 11} : std::nullopt;
}

std::optional<uint8_t> move_south(uint i) {
  return i > 10 ? std::optional{i - 11} : std::nullopt;
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

/** find position of a reachable (no intervening occ pieces) ally in the given
 * direction.
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
  layer capture_dests = find_capture_destinations_op(cd_allies, cd_foes, occ);

  for (uint8_t i = 0; i < 121; i++) {
    if (corner_pred(i)) {
      continue;
    }
    bool is_dest = capture_dests[sub_layer[i]] &
                   ((uint64_t)1 << sub_layer_offset_direct[i]);
    if (is_dest) {
      compass_allies correct_allies = get_compass_allies(i, occ, allies);
      if (correct_allies.north.has_value()) {
        uint8_t val = correct_allies.north.value();
        SECTION(std::format(
            "{} is destination - north ally is {}",
            as_notation(i),
            as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.south.has_value()) {
        uint8_t val = correct_allies.south.value();
        SECTION(std::format(
            "{} is destination - south ally is {}",
            as_notation(i),
            as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.east.has_value()) {
        uint8_t val = correct_allies.east.value();
        SECTION(std::format(
            "{} is destination - east ally is {}",
            as_notation(i),
            as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.west.has_value()) {
        uint8_t val = correct_allies.west.value();
        SECTION(std::format(
            "{} is destination - west ally is {}",
            as_notation(i),
            as_notation(val))) {
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
    uint8_t capture_counts[100] = {0};
    int total = 0;
    get_capture_move_boards<true>(boards, a, &total, moves, capture_counts);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<true>(boards, a, &total, moves, capture_counts);
    for (int i = 0; i < total; i++) {
      layer diff = a.black ^ boards[i].black;
      move m = moves[i];
      diff[sub_layer[m.orig]] -=
          ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      diff[sub_layer[m.dest]] -=
          ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<true>(boards, a, &total, moves, capture_counts);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<true>(bs, b, &total, ms, capture_counts);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<false>(boards, a, &total, moves, capture_counts);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<false>(boards, a, &total, moves, capture_counts);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<false>(boards, a, &total, moves, capture_counts);
    for (int i = 0; i < total; i++) {
      layer diff = a.white ^ boards[i].white;
      move m = moves[i];
      diff[sub_layer[m.orig]] -=
          ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      diff[sub_layer[m.dest]] -=
          ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<false>(boards, a, &total, moves, capture_counts);
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
    uint8_t capture_counts[100] = {0};
    get_capture_move_boards<false>(bs, b, &total, ms, capture_counts);
    test_capture_moves_correct<false>(bs, ms, total, b);
  });
}

//*****************************************************************************
// King

//*****************************************************************************
// Destination move boards
//*****************************************************************************

template <bool is_black>
void test_destination_moves_correct(board *bs, move *ms, int total, board b, layer destinations) {
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

  for (uint8_t i = 0; i < 121; i++) {
    if (corner_pred(i)) {
      continue;
    }
    bool is_dest = destinations[sub_layer[i]] & ((uint64_t)1 << sub_layer_offset_direct[i]);
    if (is_dest) {
      compass_allies correct_allies = get_compass_allies(i, occ, allies);
      if (correct_allies.north.has_value()) {
        uint8_t val = correct_allies.north.value();
        SECTION(std::format(
            "{} is destination - north ally is {}",
            as_notation(i),
            as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.south.has_value()) {
        uint8_t val = correct_allies.south.value();
        SECTION(std::format(
            "{} is destination - south ally is {}",
            as_notation(i),
            as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.east.has_value()) {
        uint8_t val = correct_allies.east.value();
        SECTION(std::format(
            "{} is destination - east ally is {}",
            as_notation(i),
            as_notation(val))) {
          move m = move{val, i};
          REQUIRE(contains(moves, m));
        }
      }
      if (correct_allies.west.has_value()) {
        uint8_t val = correct_allies.west.value();
        SECTION(std::format(
            "{} is destination - west ally is {}",
            as_notation(i),
            as_notation(val))) {
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

TEST_CASE("black destination moves are correct") {
  rc::prop("test", [](board b, layer l) {
    board bs[100];
    move ms[100];
    int total = 0;
    uint8_t capture_counts[100] = {0};
    auto free = ~(b.get_occ());
    auto destinations = l & free;
    get_destination_move_boards<true, false>(bs, b, &total, ms, capture_counts, destinations);
    test_destination_moves_correct<true>(bs, ms, total, b, destinations);
  });
}


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
    if (occupied) {
      break;
    }
    if (is_corner && !is_king) {
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

void test_moves_correct(
    board *bs,
    move *ms,
    int total,
    layer movers,
    layer occ,
    bool is_king = false) {
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
    uint8_t capture_counts[235] = {0};
    get_team_moves<true>(b, &total, ms, capture_counts, bs);
    test_moves_correct(bs, ms, total, b.black, b.get_occ());
  });
}

TEST_CASE("black moves and boards match") {
  rc::prop("test", [](const board b) {
    board bs[235];
    move ms[235];
    uint8_t capture_counts[235] = {0};
    int total = 0;
    get_team_moves<true>(b, &total, ms, capture_counts, bs);
    for (int i = 0; i < total; i++) {
      move m = ms[i];
      board nb = bs[i];
      layer correct_layer = {b.black[0], b.black[1]};
      // layer correct_layer = {0, 0};
      correct_layer[sub_layer[m.orig]] ^=
          ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      correct_layer[sub_layer[m.dest]] ^=
          ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
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
    uint8_t capture_counts[235] = {0};
    get_team_moves<false>(b, &total, ms, capture_counts, bs);
    test_moves_correct(bs, ms, total, b.white, b.get_occ());
  });
}

TEST_CASE("white moves and boards match") {
  rc::prop("test", [](const board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    uint8_t capture_counts[235] = {0};
    get_team_moves<false>(b, &total, ms, capture_counts, bs);
    for (int i = 0; i < total; i++) {
      move m = ms[i];
      board nb = bs[i];
      layer correct_layer = {b.white[0], b.white[1]};
      // layer correct_layer = {0, 0};
      correct_layer[sub_layer[m.orig]] ^=
          ((uint64_t)1 << sub_layer_offset_direct[m.orig]);
      correct_layer[sub_layer[m.dest]] ^=
          ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
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
    uint8_t capture_counts[235] = {0};
    get_king_moves(b, &total, ms, capture_counts, bs);
    test_moves_correct(bs, ms, total, b.king, b.get_occ(), true);
  });
}

TEST_CASE("king moves and boards match") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    uint8_t capture_counts[235] = {0};
    get_king_moves(b, &total, ms, capture_counts, bs);
    for (int i = 0; i < total; i++) {
      // layer diff = b.king ^ bs[i].king;
      move m = ms[i];
      board nb = bs[i];
      layer correct_layer = {0, 0};
      correct_layer[sub_layer[m.dest]] |=
          ((uint64_t)1 << sub_layer_offset_direct[m.dest]);
      REQUIRE(stringify(nb.king) == stringify(correct_layer));
    }
  });
}

TEST_CASE("king move board rotation correct") {
  rc::prop("test", [](board b) {
    board bs[235];
    move ms[235];
    int total = 0;
    uint8_t capture_counts[235] = {0};
    get_king_moves(b, &total, ms, capture_counts, bs);
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

//*****************************************************************************
// Bench move
//*****************************************************************************

const char *sanity_capture_string = " .  .  .  X  X  X  .  X  .  .  . "
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

const char *sanity_capture_string_white = " .  .  .  X  X  X  .  X  .  .  . "
                                          " .  .  .  .  .  .  .  .  .  .  . "
                                          " .  .  .  .  .  O  X  .  .  .  . "
                                          " X  .  .  .  .  .  .  O  .  .  X "
                                          " X  .  .  .  O  O  O  .  .  .  X "
                                          " X  X  .  O  O  #  O  .  .  X  X "
                                          " X  .  .  .  O  O  O  .  .  .  X "
                                          " X  .  .  .  .  O  .  .  .  .  X "
                                          " .  .  .  .  X  .  .  .  .  .  . "
                                          " .  .  .  .  .  X  .  .  .  .  . "
                                          " .  .  .  X  .  X  X  X  .  .  . ";

const board sanity_capture_board_white =
    read_board(sanity_capture_string_white);

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
  board boards[3] = {
      // rc::gen::arbitrary<board>()(1000, 100).value(),
      // rc::gen::arbitrary<board>()(2000, 100).value(),
      // rc::gen::arbitrary<board>()(3000, 100).value(),
      // rc::gen::arbitrary<board>()(4000, 100).value(),
      // rc::gen::arbitrary<board>()(5000, 100).value(),
      // rc::gen::arbitrary<board>()(6000, 100).value(),
      // rc::gen::arbitrary<board>()(7000, 100).value(),
      // rc::gen::arbitrary<board>()(8000, 100).value(),
      // rc::gen::arbitrary<board>()(9000, 100).value(),
      // rc::gen::arbitrary<board>()(9999, 100).value(),
      start_board,
      sanity_capture_board,
      sanity_capture_board_white,
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
  BENCHMARK("negamax ab sorted pv") {
    init_move_globals();
    struct ai_settings ai_settings = init_ai_settings();
    for (board b : boards) {
      auto tr = init_team_repetitions();
      auto r = negamax_ab_sorted_pv_runner(b, tr, true, depth, ai_settings);
    }
    // auto r = negamax_ab_sorted_runner(start_board, true, 4);
    return r;
  };
}

// TEST_CASE("test pv") {
//   auto res = negamax_ab_sorted_pv_runner(start_board, true, 5);
//   /*
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   REQUIRE(true);
// }

TEST_CASE("test encode") {
  std::string black_string = encode_layer(start_board.black);
  layer black_layer = decode_layer(black_string);
  REQUIRE(stringify(black_layer) == stringify(start_board.black));
}

TEST_CASE("test encode mini") {
  std::string board_string = encode_mini(to_mini(start_board));
  std::cout << board_string << "\n";
  board board_again = decode_mini(board_string).to_full();
  REQUIRE(basic_fmt_board(board_again) == basic_fmt_board(start_board));
}

TEST_CASE("board hash round trip") {
  rc::prop("test", [](board b) {
    std::string board_string = encode_mini(to_mini(b));
    board b2 = decode_mini(board_string).to_full();
    REQUIRE(b == b2);
  });
}

// TEST_CASE("sanity check capture") {
//   // auto res = negamax_ab_sorted_pv_runner(sanity_capture_board, true, 5);
//   auto res = negamax_ab_sorted_pv_runner(sanity_capture_board, true, 5);
//   /*
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   std::cout << "score: " << res << "\n";
//   REQUIRE(true);
// }

// TEST_CASE("sanity check capture white") {
//   auto res = negamax_ab_sorted_pv_runner(sanity_capture_board_white, false,
//   5);
//   /*
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   std::cout << "score: " << res << "\n";
//   REQUIRE(true);
// }

const char *wtf_string = " .  .  .  X  .  X  X  X  .  .  . "
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

// TEST_CASE("wtf") {
//   auto res = negamax_ab_sorted_pv_runner(wtf_board, false, 1);
//   /*
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   std::cout << "score: " << res << "\n";
//   REQUIRE(true);
// }

// TEST_CASE("test pv z") {
//    auto res = negamax_ab_sorted_pv_runner(sanity_capture_board, true, 5);
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   /*
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   REQUIRE(true);
// }

const char *king_string = " .  .  .  X  .  X  X  X  #  .  . "
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

// TEST_CASE("king escapes") {
//   auto res = negamax_ab_sorted_pv_runner(king_board, false, 2);
//   /*
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   std::cout << "score: " << res << "\n";
//   REQUIRE(true);
// }

// TEST_CASE("play self") {
//   bool is_black_turn = true;
//   board b = start_board;
//   int32_t s;
//
//   struct ai_settings ai_settings = init_ai_settings();
//
//   auto r = init_team_repetitions();
//   while (!game_over_check(b, is_black_turn, s)) {
//     search_result res = negamax_ab_sorted_pv_runner(b, r, is_black_turn, 6,
//     ai_settings); r = res.r; layer caps; if (is_black_turn) {
//       caps = b.white ^ res.b.white;
//     } else {
//       caps = b.black ^ res.b.black;
//     }
//     std::cout << "\n                == move " << (is_black_turn ? "black" :
//     "white") << " ==" << "\n"; std::cout << "move: " << res.m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(res.b), res.m.orig,
//     res.m.dest, caps);
//     // std::cout << "[ " << encode_mini(to_mini(res.b)) << " ]\n";
//     is_black_turn = !is_black_turn;
//     b = res.b;
//   }
//
//   REQUIRE(false);
// }

struct pos {
  int index;
  int rank;
  int file;
};

#define lowest_index(layer)                                                    \
  (layer[0] ? _tzcnt_u64(layer[0]) : _tzcnt_u64(layer[1]) + 64)

struct pos king_pos(const char *b) {
  layer l = read_layer(b, '#');
  int index = lowest_index(l);
  int rank = index / 11;
  int file = index % 11;
  return {index, rank, file};
}

void test_corner_moves_1(const char *b, bool should_escape) {
  layer occ = read_layer(b, 'X');
  layer occ_r = rotate_layer(occ);
  struct pos king = king_pos(b);
  bool escape = corner_moves_1(occ, occ_r, king.rank, king.file);
  REQUIRE(escape == should_escape);
}

TEST_CASE("test corner_moves_1") {
  SECTION("mid board no escape 1") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("mid board no escape 2") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  #  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("mid board no escape 3") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  #  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("mid board no escape 4") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  #  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }

  SECTION("north no escape") {
    const char *b = ".  .  X  .  .  #  .  .  X  .  ."
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
    test_corner_moves_1(b, false);
  }
  SECTION("north -> west") {
    const char *b = ".  .  .  .  .  #  .  .  X  .  ."
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
    test_corner_moves_1(b, true);
  }
  SECTION("north -> east") {
    const char *b = ".  .  X  .  .  #  .  .  .  .  ."
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
    test_corner_moves_1(b, true);
  }

  SECTION("north adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("north adjacent -> west") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("north adjacent -> east") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }

  SECTION("south adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("south adjacent -> east") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("south adjacent -> west") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }

  SECTION("south no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  X  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("south -> west") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  X  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("south -> east") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }

  SECTION("west no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("west -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("west -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }

  SECTION("west adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("west adjacent -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("west adjacent -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }

  SECTION("east no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("east -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("east -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }

  SECTION("east adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, false);
  }
  SECTION("east adjacent -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
  SECTION("east adjacent -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_moves_1(b, true);
  }
}

void test_corner_paths_1(const char *b, const string e) {
  layer expected = read_layer(e.c_str(), 'X');
  layer expected_r = rotate_layer(expected);

  // setup to generate layers
  layer occ = read_layer(b, 'X');
  layer occ_r = rotate_layer(occ);
  struct pos king = king_pos(b);
  int count = 0;
  layer results = EMPTY_LAYER;
  layer results_r = EMPTY_LAYER;

  // generate layers
  corner_paths_1(
      occ, occ_r, king.rank, king.file, results, results_r);

  // test
  REQUIRE(results == expected);
  REQUIRE(results_r == expected_r);
}

TEST_CASE("test corner_paths_1") {
  SECTION("mid board no escape 1") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("north no escape") {
    const char *b = ".  .  X  .  .  #  .  .  X  .  ."
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
    test_corner_paths_1(b, "");
  }
  SECTION("north -> west") {
    const char *b = ".  .  .  .  .  #  .  .  X  .  ."
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
    const string p = ".  X  X  X  X  .  .  .  .  .  ."
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
    test_corner_paths_1(b, p);
  }
  SECTION("north -> east") {
    const char *b = ".  .  X  .  .  #  .  .  .  .  ."
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
    const string p = ".  .  .  .  .  .  X  X  X  X  ."
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
    test_corner_paths_1(b, p);
  }
  SECTION("north -> both") {
    const char *b = ".  .  .  .  .  #  .  .  .  .  ."
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
    const string p = ".  X  X  X  X  .  X  X  X  X  ."
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
    test_corner_paths_1(b, p);
  }
  SECTION("north adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("north adjacent -> west") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     "X  X  X  X  X  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("north adjacent -> east") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  X  X  X  X  X"
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("north adjacent -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                      "X  X  X  X  X  .  X  X  X  X  X"
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("south no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  X  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("south -> west") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  X  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  X  X  X  X  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("south -> east") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  X  X  X  X  .";
    test_corner_paths_1(b, p);
  }
  SECTION("south -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  X  X  X  X  .  X  X  X  X  .";
    test_corner_paths_1(b, p);
  }
  SECTION("south adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("south adjacent -> west") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  X  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     "X  X  X  X  X  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("south adjacent -> east") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  X  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  X  X  X  X  X"
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("south adjacent -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  #  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      "X  X  X  X  X  .  X  X  X  X  X"
                      ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("east no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("east -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("east -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  X"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("east -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  #"
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  X"
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("east adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("east adjacent -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("east adjacent -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  X  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  .";
    test_corner_paths_1(b, p);
  }
  SECTION("east adjacent -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  #  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  ."
                     ".  .  .  .  .  .  .  .  .  X  .";
    test_corner_paths_1(b, p);
  }
  SECTION("west no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("west -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("west -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "X  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     "X  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("west -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    "#  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      "X  .  .  .  .  .  .  .  .  .  ."
                      ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("west adjacent no escape") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, "");
  }
  SECTION("west adjacent -> north") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("west adjacent -> south") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  X  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
  SECTION("west adjacent -> both") {
    const char *b = ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  #  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  ."
                    ".  .  .  .  .  .  .  .  .  .  .";
    const string p = ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  .  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  ."
                     ".  X  .  .  .  .  .  .  .  .  .";
    test_corner_paths_1(b, p);
  }
}

/*
TEST_CASE("test corner access 2 se") {
  SECTION("rank blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  X  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  X  .  ."
        ".  .  .  .  .  .  .  X  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("file blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  X  .  .  .  X  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("south access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("east access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  X  .  .  .  X  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("south adjacent access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("east adjacent access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  X  .  .  .  .  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
}

TEST_CASE("test corner access 2 sw") {
  SECTION("rank blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  X  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  X  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("file blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  X  .  .  .  X  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("south access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
  {p1, p2}      ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("west access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  X  .  .  .  X  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("south adjacent access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("east adjacent access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  .  .  .  .  X  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_sw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
}

TEST_CASE("test corner access 2 ne") {
  SECTION("rank blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  X  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  X  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_ne(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("file blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  X  .  .  .  X  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_ne(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("north access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_ne(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("east access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  X  .  .  .  X  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_ne(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("north adjacent access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_ne(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("east adjacent access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  X  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  X  .  .  .  .  X"
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_ne(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
}

TEST_CASE("test corner access 2 nw") {
  SECTION("rank blockers") {
    layer occ = read_layer(
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  X  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  X  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_nw(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("file blockers") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  X  .  .  .  X  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_nw(occ, occ_r, 5, 5);
    REQUIRE(!access);
  }
  SECTION("north access") {
    layer occ = read_layer(
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_nw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("west access") {
    layer occ = read_layer(
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  X  .  .  .  X  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_nw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("north adjacent access") {
    layer occ = read_layer(
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_nw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
  SECTION("west adjacent access") {
    layer occ = read_layer(
        ".  .  .  X  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        "X  .  .  .  .  X  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  ."
        ".  .  .  .  .  .  .  .  .  .  .",
        'X');
    layer occ_r = rotate_layer(occ);
    bool access = corner_access_2_nw(occ, occ_r, 5, 5);
    REQUIRE(access);
  }
}
*/

const char *sanity_king_capture_string = " .  .  X  .  X  .  .  O  .  .  . "
                                         " .  X  .  X  .  .  .  .  .  .  . "
                                         " X  .  .  O  O  X  .  .  X  .  . "
                                         " .  .  .  .  .  #  X  .  .  X  . "
                                         " .  X  .  .  .  .  O  .  .  .  X "
                                         " X  .  .  O  O  .  O  .  .  X  X "
                                         " X  .  .  .  O  O  O  .  .  .  X "
                                         " X  .  .  .  .  O  .  .  .  .  X "
                                         " .  .  .  .  .  .  .  .  O  .  . "
                                         " .  .  .  .  .  X  .  .  .  .  . "
                                         " .  .  X  .  X  X  X  X  .  .  . ";

const board sanity_king_capture_board = read_board(sanity_king_capture_string);

// TEST_CASE("sanity check king capture") {
//   // auto res = negamax_ab_sorted_pv_runner(sanity_capture_board, true, 5);
//   auto r = init_team_repetitions();
//   struct ai_settings ai_settings = init_ai_settings();
//   auto res = negamax_ab_sorted_pv_runner(sanity_king_capture_board, r, false,
//   1, ai_settings);
//   /*
//   for (int i = 0; i < MAX_DEPTH; i++) {
//     printf("[%d] = %d\n", i, PV_LENGTH[i]);
//   }
//   */
//   for (int i = 0; i < PV_LENGTH[0]; i++) {
//     auto m = PV_TABLE[0][i] ;
//     std::cout << "\n                == move " << i + 1 << " ==" << "\n";
//     std::cout << "move: " << m << "\n";
//     std::cout << overlay_move_basic(basic_fmt_board(PV_TABLE_BOARDS[0][i]),
//     m.orig, m.dest, {0,0}); std::cout << "[ " <<
//     encode_mini(to_mini(PV_TABLE_BOARDS[0][i])) << " ]\n";
//   }
//   // print_board(res._board);
//   std::cout << "score: " << res.s << "\n";
//   REQUIRE(false);
// }
