#include "assert.h"
#include "board.h"
#include "io.h"
#include "move_legacy.h"
#include "search.h"
#include "string.h"
#include "ubench.h"

// Forward declarations of original functions
pv_line quiesce_white_runner(board b);
pv_line quiesce_black_runner(board b);
void destroy_pv_line(pv_line *line);

// Test positions for benchmarking
const char *test_position_1 = " .  .  X  .  X  .  .  O  .  .  . "
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

// More tactical position with captures
const char *tactical_position = " .  .  .  .  .  .  .  .  .  .  . "
                                " .  .  .  .  .  .  .  .  .  .  . "
                                " .  .  .  .  .  .  X  .  .  .  . "
                                " .  .  .  .  X  .  .  .  .  .  . "
                                " .  O  .  X  O  .  O  X  .  .  . "
                                " .  .  .  .  X  #  X  .  .  .  . "
                                " .  .  .  X  O  O  O  X  .  O  . "
                                " .  .  .  .  .  .  X  .  .  .  . "
                                " .  .  .  .  X  .  .  .  .  .  . "
                                " .  .  .  .  .  .  .  .  .  .  . "
                                " .  .  .  .  .  .  .  .  .  .  . ";

// King escape position
const char *escape_position = " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . "
                              " .  #  .  .  .  X  .  .  .  .  . "
                              " .  .  .  .  .  .  .  .  .  .  . ";

UBENCH_EX(test_position_1, white) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}
UBENCH_EX(test_position_1, black) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_tactical, white) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_tactical, black) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_escape, white) {
  const board test_board = read_board(escape_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_escape, black) {
  const board test_board = read_board(escape_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// needs to be at top level
UBENCH_STATE();

int main() {
  init_move_globals();
  return ubench_main(0, NULL);
}
