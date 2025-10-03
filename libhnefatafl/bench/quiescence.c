#include "assert.h"
#include "board.h"
#include "io.h"
#include "move_legacy.h"
#include "search.h"
#include "stdbool.h"
#include "stdio.h"
#include "string.h"
#include "ubench.h"
#include "zobrist.h"

void destroy_pv_line(pv_line *line);

// Function to output all statistics
void print_quiescence_stats(const stats *s) {
  int total_positions =
      s->quiescence_positions_black + s->quiescence_positions_white;
  int total_cutoffs =
      s->quiencence_beta_cutoff_black + s->quiencence_beta_cutoff_white;

  printf("Black positions: %d\n", s->quiescence_positions_black);
  printf("Black beta cutoffs: %d\n", s->quiencence_beta_cutoff_black);
  printf("White positions: %d\n", s->quiescence_positions_white);
  printf("White beta cutoffs: %d\n", s->quiencence_beta_cutoff_white);
  printf("Total positions: %d\n", total_positions);
  printf("Total beta cutoffs: %d\n", total_cutoffs);
  printf("Quiescence limit reached: %d\n", s->quiescence_limit_reached);
  printf("Repeat moves encountered: %d\n", s->repeat_moves_encountered);
}

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

// More tactical position with captures
const char *tactical_solution = " .  .  .  .  .  .  .  .  .  .  . "
                                " .  .  .  .  .  .  .  .  .  .  . "
                                " .  .  .  .  .  .  X  .  .  .  . "
                                " .  .  .  .  .  X  .  .  .  .  . "
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
  static bool test_position_1_white_printed = false;
  const board test_board = read_board(test_position_1);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_white_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!test_position_1_white_printed) {
    print_quiescence_stats(&stats);
    test_position_1_white_printed = true;
  }
}
UBENCH_EX(test_position_1, black) {
  static bool test_position_1_black_printed = false;
  const board test_board = read_board(test_position_1);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_black_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!test_position_1_black_printed) {
    print_quiescence_stats(&stats);
    test_position_1_black_printed = true;
  }
}

UBENCH_EX(quiescence_tactical, white) {
  static bool quiescence_tactical_white_printed = false;
  const board test_board = read_board(tactical_position);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_white_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!quiescence_tactical_white_printed) {
    print_quiescence_stats(&stats);
    quiescence_tactical_white_printed = true;
  }
}

UBENCH_EX(quiescence_tactical, black) {
  static bool quiescence_tactical_black_printed = false;
  const board test_board = read_board(tactical_position);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_black_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!quiescence_tactical_black_printed) {
    print_quiescence_stats(&stats);
    quiescence_tactical_black_printed = true;
  }
}

UBENCH_EX(quiescence_tactical_solution, white) {
  static bool quiescence_tactical_white_printed = false;
  const board test_board = read_board(tactical_solution);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_white_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!quiescence_tactical_white_printed) {
    print_quiescence_stats(&stats);
    quiescence_tactical_white_printed = true;
  }
}

UBENCH_EX(quiescence_escape, white) {
  static bool quiescence_escape_white_printed = false;
  const board test_board = read_board(escape_position);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_white_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_white_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!quiescence_escape_white_printed) {
    print_quiescence_stats(&stats);
    quiescence_escape_white_printed = true;
  }
}

UBENCH_EX(quiescence_escape, black) {
  static bool quiescence_escape_black_printed = false;
  const board test_board = read_board(escape_position);

  // Get stats for a single call
  stats stats = {0};
  pv_line single_result = quiesce_black_runner(test_board);
  destroy_pv_line(&single_result);

  UBENCH_DO_BENCHMARK() {
    pv_line result = quiesce_black_runner(test_board);
    UBENCH_DO_NOTHING(&result.score);
    destroy_pv_line(&result);
  }

  if (!quiescence_escape_black_printed) {
    print_quiescence_stats(&stats);
    quiescence_escape_black_printed = true;
  }
}

// needs to be at top level
UBENCH_STATE();

int main() {
  init_move_globals();
  return ubench_main(0, NULL);
}
