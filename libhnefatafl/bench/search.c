#include "search.h"
#include "board.h"
#include "ubench.h"

UBENCH_EX(search, black_depth_3) {
  UBENCH_DO_BENCHMARK() {
    pv_line result = search_black_runner(start_board, 3);
    UBENCH_DO_NOTHING(&result);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(search, black_depth_4) {
  UBENCH_DO_BENCHMARK() {
    pv_line result = search_black_runner(start_board, 4);
    UBENCH_DO_NOTHING(&result);
    destroy_pv_line(&result);
  }
}

UBENCH_EX(search, black_depth_5) {
  UBENCH_DO_BENCHMARK() {
    pv_line result = search_black_runner(start_board, 5);
    UBENCH_DO_NOTHING(&result);
    destroy_pv_line(&result);
  }
}

// needs to be at top level
UBENCH_STATE();

int main() { return ubench_main(0, NULL); }
