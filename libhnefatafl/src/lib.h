/* public-facing interface for this library. It's designed be ergononmic when
 * used via Haskell's FFI.
 */

#include "board.h"
#include "layer.h"
#include "move.h"
#include "move_list.h"
#include "position_set.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "util.h"
#include "x86intrin.h" // IWYU pragma: export

typedef struct {
  layer black;
  layer white;
  u8 king;
} compact_board;

compact_board to_compact(const board *b) {
  return (compact_board){b->black, b->white, LOWEST_INDEX(b->king)};
}

board from_compact(compact_board *b) {
  layer king = EMPTY_LAYER;
  SET_INDEX(king, b->king);
  return (board){b->black,
                 rotate_layer_right(b->black),
                 b->white,
                 rotate_layer_right(b->white),
                 king,
                 rotate_layer_right(king)};
}

void start_board_extern(compact_board *b) { *b = to_compact(&start_board); }

/* Get all moves for white player (white pieces + king).
 * Returns dynamically allocated array of moves and sets move_count.
 * Caller must free the returned array.
 */
move *all_white_and_king_moves(board b, position_set *ps, int *total) {
  int white_count, king_count;
  move *white_moves = all_white_moves(b, ps, &white_count);
  move *king_moves = all_king_moves(b, ps, &king_count);

  // Combine the arrays
  int combined_count = white_count + king_count;
  move *combined_moves = malloc(sizeof(move) * combined_count);

  // Copy white moves
  for (int i = 0; i < white_count; i++) {
    combined_moves[i] = white_moves[i];
  }

  // Copy king moves
  for (int i = 0; i < king_count; i++) {
    combined_moves[white_count + i] = king_moves[i];
  }

  // Clean up individual arrays
  free(white_moves);
  free(king_moves);

  *total = combined_count;
  return combined_moves;
}

/* Get the next game state based on the previously played moves. If the game is
 * ongoing we return an "ongoing" status and the available moves for the
 * position. If the last move trigger a victory then we indicate the nature of
 * the victory in the status type. Returns dynamically allocated array of moves
 * and sets move_count. Caller must free the returned array. Returns NULL on
 * error.
 */
move *next_game_state(
    const move *move_history,
    int history_count,
    int *move_count,
    game_status *gs) {

  board *b;
  position_set *ps;
  bool is_black_turn;

  // Get board state from move history
  if (board_state_from_move_list(
          move_history,
          history_count,
          &b,
          &ps,
          &is_black_turn,
          gs) != 0) {
    return NULL;
  }

  move *possible_moves;
  if (is_black_turn) {
    possible_moves = all_black_moves(*b, ps, move_count);
  } else {
    possible_moves = all_white_and_king_moves(*b, ps, move_count);
  }

  // Clean up
  free(b);
  destroy_position_set(ps);

  return possible_moves;
}
