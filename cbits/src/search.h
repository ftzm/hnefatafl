#include "move.h"
#include "stdbool.h"

// INT_MIN == -2147483648, and -INT_MIN == INT_MIN due to integer overflow, so
// it's important that we define symmetrical min and max scores without
// overflow issues.
#define MAX_SCORE 2147483647
#define MIN_SCORE -2147483647

struct pv_line {
  bool is_black_turn;
  move *moves;
  int length;
  i32 score;
} typedef pv_line;

pv_line quiesce_white_runner(board b);
pv_line quiesce_black_runner(board b);

void destroy_pv_line(pv_line *line);
