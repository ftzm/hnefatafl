#include "board.h"
#include "search.h"
#include <stdatomic.h>
#include <stdio.h>

int main(void) {
  printf("Starting search profiling: depth 5, iterative deepening\n");

  _Atomic bool should_stop = false;

  {
    search_result result =
        search_black_runner_iterative(start_board, 5, &should_stop);
    printf("Search completed\n");
    printf("Score: %d\n", result.pv.score);
    printf("PV length: %d\n", result.pv.length);
    printf(
        "Positions examined: %d\n",
        result.statistics.search_positions_black
            + result.statistics.search_positions_white);
    destroy_pv_line(&result.pv);
  }

  {
    search_result result =
        search_white_runner_iterative(start_board, 5, &should_stop);
    printf("Search completed\n");
    printf("Score: %d\n", result.pv.score);
    printf("PV length: %d\n", result.pv.length);
    printf(
        "Positions examined: %d\n",
        result.statistics.search_positions_black
            + result.statistics.search_positions_white);
    destroy_pv_line(&result.pv);
  }

  return 0;
}
