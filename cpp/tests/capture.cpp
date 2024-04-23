#include "../capture.cpp"
#include "../board.cpp"
#include "../move.cpp"

#include <array>
#include <iostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include <rapidcheck/catch.h>
#include <format>
#include <regex>

// initialize global variables before running tests
class testRunListener : public Catch::EventListenerBase {
public:
  using Catch::EventListenerBase::EventListenerBase;

  void testRunStarting(Catch::TestRunInfo const &) override {
    init_move_globals();
  }
};
CATCH_REGISTER_LISTENER(testRunListener)

  const char *capture_destinations_input_white =
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  X  O  X  .  ."
    ".  .  O  X  .  O  .  .  .  .  ."
    ".  .  .  O  .  .  .  .  .  O  ."
    ".  .  .  X  .  .  O  .  .  .  ."
    ".  .  O  X  .  O  X  .  .  .  ."
    ".  O  .  .  .  .  .  .  .  .  ."
    ".  X  .  .  .  .  .  X  O  .  ."
    ".  .  .  O  .  X  .  .  .  .  ."
    ".  .  .  .  .  .  O  X  .  .  ."
    ".  O  X  .  O  .  .  .  .  .  .";


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

  REQUIRE(expected == foes);
  REQUIRE(expected_r == foes_r);
}

void bench_all_captures(int count) {
  board start_board = read_board(start_board_string);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  while (count) {
    for (int i = 1; i < 120; i++) {
      // layer allies = {0,0};
      // layer allies_r = {0,0};
      // layer foes = {0,0};
      // layer foes_r = {0,0};
      capture_functions[i](start_board.black, start_board.black_r,
                           start_board.white, start_board.white_r, i);
    }
    count--;
  };

  // end time
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used);
}

void bench_all_captures_niave(int count) {
  board start_board = read_board(start_board_string);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  while (count) {
    for (int i = 1; i < 120; i++) {
      // layer friends = {0,0};
      // layer foes = {0,0};
      layer output = {0, 0};
      apply_captures_niave(start_board.black, start_board.white, output, i);
    }
    count--;
  };

  // end time
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used);
}

void bench_board_gen(int count) {
  move moves[235];
  board boards[235];

  move moves_2[235];
  board boards_2[235];

  move moves_3[235];
  board boards_3[235];

  board start_board = read_board(start_board_string);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  /*
  while (count) {
    get_team_moves_black(start_board, &total, moves, boards);
    count--;
  }
  */

  int sum = 0;
  while (count) {
    int total = 0;
    get_team_moves_black(start_board, &total, moves, boards);
    for (int i = 0; i < total; i++) {
      int total_2 = 0;
      get_team_moves_black(boards[i], &total_2, moves_2, boards_2);
      for (int j = 0; j < total_2; j++) {
        board b2 = boards_2[j];
        int total_3 = 0;
        get_capture_move_boards<true>(boards_3, b2, &total_3, moves_3);
        // const layer occ = {
        //   b2.black[0] | b2.white[0] | b2.king[0] | corners[0],
        //   b2.black[1] | b2.white[1] | b2.king[1] | corners[1]};
        // const layer occ_r = {
        //   b2.black_r[0] | b2.white_r[0] | b2.king_r[0] | corners[0],
        //   b2.black_r[1] | b2.white_r[1] | b2.king_r[1] | corners[1]};
        // sum += get_team_move_count(occ, b2.black, occ_r, b2.black_r);
        // 1.34 for the above
        // sum += __builtin_popcountll(b2.black[0]) +
        // __builtin_popcountll(b2.black[1]);
        sum += total;
      }
      /*
       */
      // sum += total_2;
    }
    count--;
  }
  printf("all moves: %d\n", sum);

  // end time
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used);
}

int main_old(int argc, char **argv) {

  printf("Testing capture!...\n");

  // bench_all_captures(1000000);
  // bench_all_captures_niave(1000000);

  /*
  board start_board = read_board(start_board_string);
  print_board(start_board);

  move moves[235];
  board boards[235];
  int total = 0;
  get_team_moves_black(start_board, &total, moves, boards);

  printf("total: %d\n", total);

  while (total) {
    move m = moves[total];
    printf("orig: %d, dest: %d\n\n", m.orig, m.dest);
    print_board(boards[total]);
    print_board_r(boards[total]);
    total--;
    printf("--------------------------------------------------\n");
  };
  printf("orig: %d, dest: %d\n\n", moves[0].orig, moves[0].dest);
  print_board(boards[0]);
  print_board_r(boards[0]);

  */

  // bench_board_gen(1000);

  // print_row(get_row_moves_2(0b000000010, 5));
  // print_row(2^0b0100100);

  // layer allies = read_layer(capture_destinations_input, 'O');
  // layer foes = read_layer(capture_destinations_input, 'X');
  // layer dests = find_capture_destinations_op(allies, foes);
  // print_layer(dests);

  /*
  int total = 0;
  move moves[235];
  board boards[235];
  board cap_board = read_board(capture_destinations_input);
  print_board(cap_board);
  get_capture_move_boards_black(cap_board, &total, moves, boards);

  printf("results\n");
  for (int i = 0; i < total; i++) {
    std::cout << overlay_move(pretty_fmt_board(boards[i]), moves[i].orig,
                              moves[i].dest, cap_board.white ^ boards[i].white);
  }
  */
  int total = 0;
  move moves[235];
  board boards[235];
  board cap_board = read_board(capture_destinations_input_white);
  print_board(cap_board);
  get_capture_move_boards<false>(boards, cap_board, &total, moves);

  printf("results\n");
  for (int i = 0; i < total; i++) {
    std::cout << overlay_move(pretty_fmt_board(boards[i]), moves[i].orig,
                              moves[i].dest, cap_board.black ^ boards[i].black);
  }
  /*

  board start = read_board(start_board_string);
  string pb = pretty_fmt_board(start);
  move m = {0, 3};
  string mb = overlay_move(pb, m);
  std::cout << mb;
  */

  return 0;
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

TEST_CASE("capture_s") {
  const char *s_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  O  O  O  X  O  O  O  O  ."
                        ".  O  X  X  X  .  X  X  X  X  O";

  const char *s_expected = ".  .  .  .  .  .  .  .  .  .  ."
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

  test_capture(capture_s, s_input, s_expected, 5);
}

TEST_CASE("capture_se") {
  const char *se_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  O  O  O  .  .  .  .  .  ."
                         ".  O  X  X  X  .  .  .  .  .  .";

  const char *se_expected = ".  .  .  .  .  .  .  .  .  .  ."
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

  test_capture(capture_se, se_input, se_expected, 5);
}

TEST_CASE("capture_sw") {
  const char *sw_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  O  O  O  O  ."
                         ".  .  .  .  .  .  X  X  X  X  O";

  const char *sw_expected = ".  .  .  .  .  .  .  .  .  .  ."
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

  test_capture(capture_sw, sw_input, sw_expected, 5);
}

TEST_CASE("capture_e") {

  const char *e_input = ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  O"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  O  X  ."
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  O  X"
                        ".  .  .  .  .  .  .  .  .  .  O";

  const char *e_expected = ".  .  .  .  .  .  .  .  .  .  "
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
  test_capture(capture_e, e_input, e_expected, 55);
}

TEST_CASE("capture_en") {
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

  const char *en_expected = ".  .  .  .  .  .  .  .  .  .  "
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
  test_capture(capture_en, en_input, en_expected, 55);
}

TEST_CASE("capture_es") {
  const char *es_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  O  X"
                         ".  .  .  .  .  .  .  .  .  .  O";

  const char *es_expected = ".  .  .  .  .  .  .  .  .  .  "
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

  test_capture(capture_es, es_input, es_expected, 55);
}

TEST_CASE("capture_w") {
  const char *w_input = ".  .  .  .  .  .  .  .  .  .  ."
                        "O  .  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        ".  X  O  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "X  O  .  .  .  .  .  .  .  .  ."
                        "O  .  .  .  .  .  .  .  .  .  .";

  const char *w_expected = ".  .  .  .  .  .  .  .  .  .  ."
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

  test_capture(capture_w, w_input, w_expected, 65);
}

TEST_CASE("capture_wn") {
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

  test_capture(capture_wn, wn_input, wn_expected, 65);
}

TEST_CASE("capture_ws") {
  const char *ws_input = ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "X  O  .  .  .  .  .  .  .  .  ."
                         "O  .  .  .  .  .  .  .  .  .  .";

  const char *ws_expected = ".  .  .  .  .  .  .  .  .  .  ."
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
  test_capture(capture_ws, ws_input, ws_expected, 65);
}

TEST_CASE("capture_n") {
  const char *n_input = ".  O  X  X  X  .  X  X  X  X  O"
                        ".  .  O  O  O  X  O  O  O  O  ."
                        ".  .  .  .  .  O  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  ."
                        ".  .  .  .  .  .  .  .  .  .  .";

  const char *n_expected = ".  .  .  .  .  .  .  .  .  .  ."
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

  test_capture(capture_n, n_input, n_expected, 115);
}

TEST_CASE("capture_n 2") {
  const char *n_input_2 = ".  .  X  X  X  .  X  X  X  X  O"
                          ".  .  O  O  O  X  O  O  O  O  ."
                          ".  .  .  .  .  O  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";

  const char *n_expected_2 = ".  .  X  X  X  .  .  .  .  .  ."
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
  test_capture(capture_n, n_input_2, n_expected_2, 115);
}

TEST_CASE("capture_ne") {
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

  const char *ne_expected = ".  .  .  .  .  .  .  .  .  .  ."
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
  test_capture(capture_ne, ne_input, ne_expected, 118);
}

TEST_CASE("capture_nw") {
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

  const char *nw_expected = ".  .  .  .  .  .  .  .  .  .  ."
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
  test_capture(capture_nw, nw_input, nw_expected, 113);
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

TEST_CASE("black moves and boards match") {
}

TEST_CASE("white moves and boards match") {
}

TEST_CASE("move indices are within index bounds") {
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

namespace rc {
template <> struct Arbitrary<board> {
  static Gen<board> arbitrary() {
    return gen::exec([] {
      // black
      layer black = {0, 0};
      const size_t black_size = *gen::inRange(1, 25);
      const std::vector<int> black_indices =
          *gen::unique<std::vector<int>>(black_size, gen::inRange(1, 121));
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
            return !elem(black_indices, x);
          }));
      for (int i : white_indices) {
        white[sub_layer[i]] |= ((uint64_t)1 << sub_layer_offset_direct[i]);
      }
      layer white_r = rotate_layer(white);

      // king
      layer king = {0, 0};
      const int king_index = *gen::suchThat(
          gen::inRange(1, 121), [black_indices, white_indices](int x) {
            return !elem(black_indices, x) && !elem(white_indices, x);
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
// Capture move boards
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

//*****************************************************************************
// White

//*****************************************************************************
// King

//*****************************************************************************
// Capture move boards
//*****************************************************************************
