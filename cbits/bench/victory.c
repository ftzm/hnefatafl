#include "victory.h"
#include "board.h"
#include "io.h"
#include "ubench.h"

// Benchmark board 1 (not surrounded)
UBENCH_EX(surrounded_bench, board1_not_surrounded) {
  const char *board_str = ".  .  .  X  X  X  X  X  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  O  .  .  .  .  X"
                          "X  .  .  .  O  O  O  .  .  .  X"
                          "X  X  .  O  O  #  O  O  .  X  X"
                          "X  .  .  .  O  O  O  .  .  .  X"
                          "X  .  .  .  .  O  .  .  .  .  X"
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  X  X  X  X  X  .  .  .";
  const board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = surrounded(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

// Benchmark board 2 (not surrounded)
UBENCH_EX(surrounded_bench, board2_not_surrounded) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  O  O  O  O  .  .  .  .  ."
                          ".  .  .  .  O  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  #  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  const board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = surrounded(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

// Benchmark board 3 (surrounded)
UBENCH_EX(surrounded_bench, board3_surrounded) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  X  X  .  .  .  .  .  ."
                          ".  .  X  .  .  X  .  .  .  .  ."
                          ".  X  O  O  O  O  X  .  .  .  ."
                          ".  .  X  .  O  #  X  .  .  .  ."
                          ".  .  .  X  .  X  .  .  .  .  ."
                          ".  .  .  .  X  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  const board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = surrounded(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

// Benchmark board 4 (not surrounded)
UBENCH_EX(surrounded_bench, board4_not_surrounded) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  X  X  .  .  O  .  .  ."
                          ".  .  X  .  .  X  .  .  .  .  ."
                          ".  X  O  O  O  O  X  .  .  .  ."
                          ".  .  X  .  O  #  X  .  .  .  ."
                          ".  .  .  X  .  X  .  .  .  .  ."
                          ".  .  .  .  X  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  const board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = surrounded(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

// Benchmark board with white piece at edge (not surrounded - should be fast)
UBENCH_EX(surrounded_bench, white_at_edge) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          "O  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  X  .  .  .  .  .  .  ."
                          ".  .  .  X  .  #  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  const board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = surrounded(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_STATE();

int main() { return ubench_main(0, NULL); }
