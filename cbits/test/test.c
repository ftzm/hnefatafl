#include "greatest.h"

SUITE_EXTERN(capture_suite);
SUITE_EXTERN(corner_moves_1_suite);
SUITE_EXTERN(corner_moves_2_suite);
SUITE_EXTERN(corner_paths_1_suite);
SUITE_EXTERN(corner_paths_2_suite);
SUITE_EXTERN(move_suite);
SUITE_EXTERN(position_set_suite);
SUITE_EXTERN(score_suite);
SUITE_EXTERN(victory_suite);
SUITE_EXTERN(zobrist_suite);

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {

  GREATEST_MAIN_BEGIN();
  RUN_SUITE(capture_suite);
  RUN_SUITE(corner_moves_1_suite);
  RUN_SUITE(corner_moves_2_suite);
  RUN_SUITE(corner_paths_1_suite);
  RUN_SUITE(corner_paths_2_suite);
  RUN_SUITE(move_suite);
  RUN_SUITE(position_set_suite);
  RUN_SUITE(score_suite);
  RUN_SUITE(victory_suite);
  RUN_SUITE(zobrist_suite);
  GREATEST_MAIN_END();
}
