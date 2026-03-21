#pragma once

#include "board.h"
#include "move.h"
#include "stdbool.h"
#include "types.h"

// Main validation function
move_error validate_move(board b, move m, bool is_black_turn);

// Board state validation
bool validate_board_state(board b);

// Helper functions exposed for testing
piece_type get_piece_at(board b, u8 position);
bool is_correct_piece(bool is_black_turn, piece_type piece);
bool is_orthogonal_move(move m);
dir get_move_direction(move m);
int get_step_for_direction(dir direction);
layer draw_line_between(move m);
bool has_clear_path(board b, move m);

// Board validation functions
int board_has_valid_piece_counts(const board *b);
int board_has_no_overlapping_pieces(const board *b);
int board_has_no_pieces_on_corners(const board *b);
int board_has_no_pawns_on_throne(const board *b);
int board_has_consistent_rotations(const board *b);
int board_has_no_pieces_out_of_bounds(const board *b);
int board_is_valid(const board *b);