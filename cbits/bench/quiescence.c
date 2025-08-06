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

// Benchmark original quiesce_white_runner
UBENCH_EX(quiescence_comparison, original_white_standard) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}
// Benchmark original quiesce_black_runner
UBENCH_EX(quiescence_comparison, original_black_standard) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Tactical position benchmarks
UBENCH_EX(quiescence_tactical, original_white_tactical) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_tactical, original_black_tactical) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// King escape position
UBENCH_EX(quiescence_escape, original_white_escape) {
  const board test_board = read_board(escape_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Memory usage comparison
UBENCH_EX(quiescence_memory, original_memory_usage) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    // Original uses stack arrays: move ms[100], layer ls[100], layer ls_r[100]
    // Total: ~6.4KB per call (100 * (2 + 16 + 16) bytes)
    // Plus potential recursion depth multiplier
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Early termination scenarios
UBENCH_EX(quiescence_early_term, original_beta_cutoff) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    // Original implementation generates all moves even if early beta cutoff
    // occurs
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Deep search comparison
UBENCH_EX(quiescence_deep, original_deep_search) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    // This should trigger deeper quiescence search with multiple recursive
    // calls
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
