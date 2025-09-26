/* public-facing interface for this library. It's designed be ergononmic when
 * used via Haskell's FFI.
 */

#include "board.h"
#include "io.h"
#include "layer.h"
#include "util.h"
#include "x86intrin.h" // IWYU pragma: export

typedef struct {
  layer black;
  layer white;
  u8 king;
} compact_board;

compact_board to_compact(board *b) {
  return (compact_board){b->black, b->white, LOWEST_INDEX(b->king)};
}

board from_compact(compact_board *b) {
  layer king = EMPTY_LAYER;
  SET_INDEX(king, b->king);
  return (board){
      b->black,
      rotate_layer_right(b->black),
      b->white,
      rotate_layer_right(b->white),
      king,
      rotate_layer_right(king)};
}

void start_board_extern(compact_board *b) {
  board start_board = read_board(start_board_string);
  *b = to_compact(&start_board);
}
