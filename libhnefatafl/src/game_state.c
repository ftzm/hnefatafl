#include "game_state.h"
#include "board.h"
#include "capture.h"
#include "move.h"
#include "position_set.h"
#include "stdlib.h"
#include "validation.h"
#include "victory.h"
#include "x86intrin.h" // IWYU pragma: export
#include "zobrist.h"

int apply_move_to_game(
    board *b,
    position_set *first_ps,
    position_set *second_ps,
    bool *is_black_turn,
    move m,
    game_status *gs) {

  if (!validate_move(*b, m, *is_black_turn)) {
    return 1;
  }

  u64 board_hash;
  if (*is_black_turn) {
    board_hash =
        next_hash_black(hash_for_board(*b, is_black_turn), m.orig, m.dest);
    *b = apply_black_move_m(*b, m.orig, m.dest);
    apply_captures_z_black(b, &board_hash, m.dest);
  } else if (LOWEST_INDEX(b->king) == m.orig) {
    board_hash =
        next_hash_king(hash_for_board(*b, is_black_turn), m.orig, m.dest);
    *b = apply_king_move_m(*b, m.orig, m.dest);
    apply_captures_z_white(b, &board_hash, m.dest);
  } else {
    board_hash =
        next_hash_white(hash_for_board(*b, is_black_turn), m.orig, m.dest);
    *b = apply_white_move_m(*b, m.orig, m.dest);
    apply_captures_z_white(b, &board_hash, m.dest);
  }

  // Check for threefold repetition
  int index;
  if (insert_position(first_ps, board_hash, &index) != 0) {
    // Position already exists in first set, try second set
    if (insert_position(second_ps, board_hash, &index) != 0) {
      // Position already exists in second set - threefold repetition
      return 2;
    }
  }

  *is_black_turn = !*is_black_turn;

  if (*is_black_turn) {
    *gs = white_victory_check(b);
  } else {
    *gs = black_victory_check(b);
  }

  return 0;
}

int board_state_from_move_list(
    const move *moves,
    int count,
    board **b_ptr,
    position_set **ps_ptr,
    bool *is_black_turn,
    game_status *gs) {

  *is_black_turn = true;

  board b = start_board;
  position_set *first_ps = create_position_set(count);
  position_set *second_ps = create_position_set(count);

  for (int i = 0; i < count; i++) {
    move m = moves[i];
    int result =
        apply_move_to_game(&b, first_ps, second_ps, is_black_turn, m, gs);
    if (result != 0) {
      destroy_position_set(first_ps);
      destroy_position_set(second_ps);
      return result;
    }
  }

  *b_ptr = malloc(sizeof(board));
  **b_ptr = b;
  *ps_ptr = second_ps;
  destroy_position_set(first_ps);
  return 0;
}