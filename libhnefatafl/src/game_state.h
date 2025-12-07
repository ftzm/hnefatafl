#pragma once

#include "board.h"
#include "move.h"
#include "position_set.h"
#include "stdbool.h"
#include "types.h"

typedef struct {
  board b;
  position_set *ps;
  bool is_black_turn;
} board_state;

// Board state reconstruction from move history
move_validation_result board_state_from_move_list(
    const move *moves,
    int count,
    board **b_ptr,
    position_set **ps_ptr,
    bool *is_black_turn,
    game_status *gs,
    bool allow_repetition);

// Apply a single move to the game state with validation
// Returns 0 on success, 1 if the move is invalid, 2 if threefold repetition
int apply_move_to_game(
    board *b,
    position_set *first_ps,
    position_set *second_ps,
    bool *is_black_turn,
    move m,
    game_status *gs,
    bool allow_repetition);