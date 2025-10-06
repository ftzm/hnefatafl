#include "lib.h"
#include "greatest.h"
#include "move.h"
#include "stdio.h"
#include "types.h"

TEST test_get_possible_moves() {

  // Get the first black move from start_black_moves
  move first_move = start_black_moves[0];

  // Create move history with just one move
  move history[1] = {first_move};

  // Call next_game_state
  int move_count = 0;
  game_status gs;
  move *possible_moves = next_game_state(history, 1, &move_count, &gs);

  if (possible_moves == NULL) {
    printf("ERROR: get_possible_moves returned NULL\n");
    FAIL();
  }

  // Should have some moves for white after black's first move
  ASSERT(move_count > 0);

  // Print first few moves for debugging
  for (int i = 0; i < move_count && i < 5; i++) {
    printf(
        "Move %d: %d -> %d\n",
        i,
        possible_moves[i].orig,
        possible_moves[i].dest);
  }

  // Clean up
  free(possible_moves);

  PASS();
}

SUITE(lib_suite) { RUN_TEST(test_get_possible_moves); }
