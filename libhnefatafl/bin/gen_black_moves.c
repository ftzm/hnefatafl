#include "board.h"
#include "move.h"
#include "position_set.h"
#include "stdio.h"
#include "stdlib.h"

int main() {
  printf("Starting gen_black_moves...\n");
  fflush(stdout);

  // Create an empty position set
  printf("Creating position set...\n");
  fflush(stdout);
  position_set *ps = create_position_set(1000);

  // Get all black moves from the starting board
  printf("Calling all_black_moves...\n");
  fflush(stdout);
  int total;
  move *moves = all_black_moves(start_board, ps, &total);

  printf("Got %d moves, generating output...\n", total);
  fflush(stdout);

  // Print as a valid C array
  printf("// All black starting moves\n");
  printf("static const move start_black_moves[%d] = {\n", total);
  for (int i = 0; i < total; i++) {
    printf("  {%d, %d}", moves[i].orig, moves[i].dest);
    if (i < total - 1) {
      printf(",");
    }
    printf("\n");
  }
  printf("};\n");
  printf("\nstatic const int start_black_moves_count = %d;\n", total);

  // Clean up
  free(moves);
  destroy_position_set(ps);

  return 0;
}
