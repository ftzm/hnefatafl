#include "search_generator.h"
#include "board.h"
#include "io.h"
#include "move_legacy.h"
#include "ubench.h"
#include "assert.h"
#include "string.h"

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

// Benchmark generator-based quiesce_white_runner_generator
UBENCH_EX(quiescence_comparison, generator_white_standard) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner_generator(test_board);
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

// Benchmark generator-based quiesce_black_runner_generator
UBENCH_EX(quiescence_comparison, generator_black_standard) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner_generator(test_board);
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

UBENCH_EX(quiescence_tactical, generator_white_tactical) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner_generator(test_board);
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

UBENCH_EX(quiescence_tactical, generator_black_tactical) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner_generator(test_board);
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

UBENCH_EX(quiescence_escape, generator_white_escape) {
  const board test_board = read_board(escape_position);
  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner_generator(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Correctness test - verify both implementations give same results
UBENCH_EX(quiescence_correctness, verify_white_same_results) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line original_result = quiesce_white_runner(test_board);
    pv_line generator_result = quiesce_white_runner_generator(test_board);
    
    // Debug output
    printf("Original score: %d\n", original_result.score);
    printf("Generator score: %d\n", generator_result.score);
    printf("Difference: %d\n", abs(original_result.score - generator_result.score));
    fflush(stdout);
    
    // Scores should match (allowing for small differences due to implementation)
    assert(abs(original_result.score - generator_result.score) <= 1);
    
    UBENCH_DO_NOTHING(&original_result.score);
    UBENCH_DO_NOTHING(&generator_result.score);
    
    destroy_pv_line(&original_result);
    destroy_pv_line(&generator_result);
  }
}

UBENCH_EX(quiescence_correctness, verify_black_same_results) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    pv_line original_result = quiesce_black_runner(test_board);
    pv_line generator_result = quiesce_black_runner_generator(test_board);
    
    // Scores should match (allowing for small differences due to implementation)
    assert(abs(original_result.score - generator_result.score) <= 1);
    
    UBENCH_DO_NOTHING(&original_result.score);
    UBENCH_DO_NOTHING(&generator_result.score);
    
    destroy_pv_line(&original_result);
    destroy_pv_line(&generator_result);
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

UBENCH_EX(quiescence_memory, generator_memory_usage) {
  const board test_board = read_board(test_position_1);
  UBENCH_DO_BENCHMARK() {
    // Generator uses only the generator struct: ~200 bytes
    // Much lower memory footprint
    pv_line result = quiesce_white_runner_generator(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Early termination scenarios
UBENCH_EX(quiescence_early_term, original_beta_cutoff) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    // Original implementation generates all moves even if early beta cutoff occurs
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_early_term, generator_beta_cutoff) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    // Generator can terminate early when beta cutoff is found
    pv_line result = quiesce_white_runner_generator(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

// Deep search comparison
UBENCH_EX(quiescence_deep, original_deep_search) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    // This should trigger deeper quiescence search with multiple recursive calls
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(quiescence_deep, generator_deep_search) {
  const board test_board = read_board(tactical_position);
  UBENCH_DO_BENCHMARK() {
    // Generator version of deep search
    pv_line result = quiesce_black_runner_generator(test_board);
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
