#include "move.h"
#include "stdbool.h"

struct pv_line {
  bool is_black_turn;
  move *moves;
  int length;
} typedef pv_line;

pv_line quiesce_white_runner(board b);
pv_line quiesce_black_runner(board b);

void destroy_pv_line(pv_line *line);
