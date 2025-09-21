#include "move.h"
#include "stdbool.h"

typedef struct position_set position_set;

// INT_MIN == -2147483648, and -INT_MIN == INT_MIN due to integer overflow, so
// it's important that we define this value that can safely be negated without
#define INFINITY 2147483647

// We define these constants to be 1 less than INFINITY so that we can return a
// maximum/minimum score that does not equal the starting alpha/beta values and
// trigger the beta cutoff branch at the root.
#define MAX_SCORE 2147483646
#define MIN_SCORE -2147483646

typedef struct pv_line {
  bool is_black_turn;
  move *moves;
  int length;
  i32 score;
} pv_line;

typedef struct stats {
  int search_positions_black;
  int search_positions_white;
  int search_beta_cutoff_black;
  int search_beta_cutoff_white;
  int quiescence_positions_black;
  int quiescence_positions_white;
  int quiencence_beta_cutoff_black;
  int quiencence_beta_cutoff_white;
  int quiescence_limit_reached;
  int repeat_moves_encountered;
} stats;

pv_line quiesce_white_runner(board b);
pv_line quiesce_black_runner(board b);

// Runner functions that also return statistics
pv_line quiesce_white_runner_with_stats(board b, i32 alpha, i32 beta, stats *statistics, position_set *positions);
pv_line quiesce_black_runner_with_stats(board b, i32 alpha, i32 beta, stats *statistics, position_set *positions);

// Search runner functions that also return statistics
pv_line search_white_runner_with_stats(board b, int depth, bool is_pv, i32 alpha, i32 beta, stats *statistics, position_set *positions);
pv_line search_black_runner_with_stats(board b, int depth, bool is_pv, i32 alpha, i32 beta, stats *statistics, position_set *positions);

void destroy_pv_line(pv_line *line);

