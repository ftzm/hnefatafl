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
  board test_board = read_board(board_str);
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
  board test_board = read_board(board_str);
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
  board test_board = read_board(board_str);
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
  board test_board = read_board(board_str);
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
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = surrounded(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(exit_fort_bench, king_in_center) {
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
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = exit_fort(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(exit_fort_bench, invalidated_by_black) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  O  .  X  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  O  .  .  .  O  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  O  O  .  .  ."
                          ".  .  .  .  X  O  X  O  X  .  ."
                          ".  .  .  .  O  O  #  O  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = exit_fort(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(exit_fort_bench, unrotated_exit) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  O  .  X  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  O  .  .  .  O  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  O  O  .  .  ."
                          ".  .  .  .  X  O  .  O  X  .  ."
                          ".  .  .  .  O  O  #  O  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = exit_fort(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(exit_fort_bench, rotated_exit) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  O  .  X  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  O  .  .  .  O  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  O  O  .  .  ."
                          ".  .  .  .  X  O  .  O  X  .  ."
                          ".  .  .  .  O  O  #  O  .  .  .";
  board test_board = rotate_board_right(read_board(board_str));
  UBENCH_DO_BENCHMARK() {
    bool result = exit_fort(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_capture_check_bench, king_surrounded_interior) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  X  #  X  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_capture_check(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_capture_check_bench, king_not_surrounded) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  X  #  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_capture_check(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_capture_check_bench, king_at_edge) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          "#  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_capture_check(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_captured_bench, king_surrounded_interior) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  X  #  X  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_captured(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_captured_bench, king_not_surrounded) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  X  #  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_captured(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_escaped_bench, king_on_corner) {
  const char *board_str = "#  .  .  .  .  .  .  .  .  .  ."
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
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_escaped(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_escaped_bench, king_in_center) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  #  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_escaped(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_effectively_escaped_bench, king_adjacent_to_corner) {
  const char *board_str = ".  #  .  .  .  .  .  .  .  .  ."
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
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_effectively_escaped(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_effectively_escaped_bench, king_in_center) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  #  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_effectively_escaped(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_capture_check_ref_bench, king_surrounded_interior) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  X  #  X  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_capture_check_ref(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_capture_check_ref_bench, king_not_surrounded) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  X  #  .  .  .  .  ."
                          ".  .  .  .  .  X  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_capture_check_ref(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_EX(king_capture_check_ref_bench, king_at_edge) {
  const char *board_str = ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          "#  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          "X  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  ."
                          ".  .  .  .  .  .  .  .  .  .  .";
  board test_board = read_board(board_str);
  UBENCH_DO_BENCHMARK() {
    bool result = king_capture_check_ref(&test_board);
    UBENCH_DO_NOTHING(&result);
  }
}

UBENCH_STATE();

int main() { return ubench_main(0, NULL); }
