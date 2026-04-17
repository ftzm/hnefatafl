#include "validation.h"
#include "io.h"
#include <stdio.h>

bool validate_board_state(board b) {
  // Check that no pieces overlap
  layer black_white = LAYER_AND(b.black, b.white);
  layer black_king = LAYER_AND(b.black, b.king);
  layer white_king = LAYER_AND(b.white, b.king);

  if (NOT_EMPTY(black_white)) {
#ifndef NDEBUG
    printf("Board validation failed: black and white pieces overlap\n");
    print_board(b);
#endif
    return false;
  }

  if (NOT_EMPTY(black_king)) {
#ifndef NDEBUG
    printf("Board validation failed: black and king overlap\n");
    print_board(b);
#endif
    return false;
  }

  if (NOT_EMPTY(white_king)) {
#ifndef NDEBUG
    printf("Board validation failed: white and king overlap\n");
    print_board(b);
#endif
    return false;
  }

  return true;
}

piece_type get_piece_at(board b, u8 position) {
  if (CHECK_INDEX(b.black, position)) {
    return black_pawn;
  } else if (CHECK_INDEX(b.white, position)) {
    return white_pawn;
  } else if (CHECK_INDEX(b.king, position)) {
    return king;
  } else {
    return empty;
  }
}

static bool
is_valid_destination(board b, u8 position, piece_type moving_piece) {
  if (moving_piece == king) {
    layer occupied = king_board_occ(b);
    return !CHECK_INDEX(occupied, position);
  } else {
    layer occupied = LAYER_OR(throne, board_occ(b));
    return !CHECK_INDEX(occupied, position);
  }
}

bool is_correct_piece(bool is_black_turn, piece_type piece) {
  if (is_black_turn) {
    return piece == black_pawn;
  } else {
    return piece == white_pawn || piece == king;
  }
}

bool is_orthogonal_move(move m) {
  u8 orig_rank = RANK(m.orig);
  u8 orig_file = FILE(m.orig);
  u8 dest_rank = RANK(m.dest);
  u8 dest_file = FILE(m.dest);
  return (orig_rank == dest_rank) || (orig_file == dest_file);
}

dir get_move_direction(move m) {
  u8 orig_rank = RANK(m.orig);
  u8 orig_file = FILE(m.orig);
  u8 dest_rank = RANK(m.dest);
  u8 dest_file = FILE(m.dest);

  if (orig_rank == dest_rank) {
    if (orig_file < dest_file) {
      return east;
    } else {
      return west;
    }
  } else {
    if (orig_rank < dest_rank) {
      return north;
    } else {
      return south;
    }
  }
}

int get_step_for_direction(dir direction) {
  switch (direction) {
  case north:
    return 11;
  case south:
    return -11;
  case east:
    return 1;
  case west:
    return -1;
  default:
    return 0;
  }
}

layer draw_line_between(move m) {
  layer line = EMPTY_LAYER;
  dir direction = get_move_direction(m);
  int step = get_step_for_direction(direction);
  u8 current = m.orig + step;

  // Set bits for all squares between origin and destination (exclusive)
  while (current != m.dest) {
    SET_INDEX(line, current);
    current += step;
  }

  return line;
}

bool has_clear_path(board b, move m) {
  layer path = draw_line_between(m);
  layer occupied = board_occ(b);
  return !LAYERS_OVERLAP(path, occupied);
}

// ---------------------------------------------------------------------------
// Board validation functions
// ---------------------------------------------------------------------------

// Valid bits mask for the upper half of a layer.
// The board has 121 squares; the upper u64 uses 57 bits (indices 64-120).
#define VALID_UPPER_MASK ((u64)((1ULL << 57) - 1))

// Black: 1-24, White: 1-12, King: exactly 1
int board_has_valid_piece_counts(const board *b) {
  int bc = LAYER_POPCOUNT(b->black);
  int wc = LAYER_POPCOUNT(b->white);
  int kc = LAYER_POPCOUNT(b->king);
  return bc >= 1 && bc <= 24 && wc >= 1 && wc <= 12 && kc == 1;
}

// No two piece layers share any set bit
int board_has_no_overlapping_pieces(const board *b) {
  return IS_EMPTY(LAYER_AND(b->black, b->white))
         && IS_EMPTY(LAYER_AND(b->black, b->king))
         && IS_EMPTY(LAYER_AND(b->white, b->king));
}

// No pieces on the four corner squares
int board_has_no_pieces_on_corners(const board *b) {
  layer all_pieces = LAYER_OR(LAYER_OR(b->black, b->white), b->king);
  return IS_EMPTY(LAYER_AND(all_pieces, corners));
}

// Only the king may occupy the throne (index 60)
int board_has_no_pawns_on_throne(const board *b) {
  return !CHECK_INDEX(b->black, 60) && !CHECK_INDEX(b->white, 60);
}

// Rotated layers are consistent with their non-rotated counterparts
int board_has_consistent_rotations(const board *b) {
  layer expected_black_r = rotate_layer_right(b->black);
  layer expected_white_r = rotate_layer_right(b->white);
  layer expected_king_r = rotate_layer_right(b->king);
  return LAYERS_EQUAL(b->black_r, expected_black_r)
         && LAYERS_EQUAL(b->white_r, expected_white_r)
         && LAYERS_EQUAL(b->king_r, expected_king_r);
}

// No piece has bits set beyond the valid 121 board squares in the upper layer
// half
int board_has_no_pieces_out_of_bounds(const board *b) {
  return (b->black._[1] & ~VALID_UPPER_MASK)
         == 0
         && (b->white._[1] & ~VALID_UPPER_MASK)
         == 0
         && (b->king._[1] & ~VALID_UPPER_MASK)
         == 0
         && (b->black_r._[1] & ~VALID_UPPER_MASK)
         == 0
         && (b->white_r._[1] & ~VALID_UPPER_MASK)
         == 0
         && (b->king_r._[1] & ~VALID_UPPER_MASK)
         == 0;
}

// All-in-one validity check
int board_is_valid(const board *b) {
  return board_has_valid_piece_counts(b)
         && board_has_no_overlapping_pieces(b)
         && board_has_no_pieces_on_corners(b)
         && board_has_no_pawns_on_throne(b)
         && board_has_consistent_rotations(b)
         && board_has_no_pieces_out_of_bounds(b);
}

move_error validate_move(board b, move m, bool is_black_turn) {
  piece_type moving_piece = get_piece_at(b, m.orig);

  if (m.dest > 120) {
    return move_error_position_out_of_bounds;
  }

  if (m.orig == m.dest) {
    return move_error_dest_equals_origin;
  }

  if (moving_piece == empty) {
    return move_error_no_piece_at_origin;
  }

  if (!is_correct_piece(is_black_turn, moving_piece)) {
    return move_error_wrong_piece_for_turn;
  }

  if (!is_valid_destination(b, m.dest, moving_piece)) {
    return move_error_invalid_destination;
  }

  if (!is_orthogonal_move(m)) {
    return move_error_not_orthogonal;
  }

  if (!has_clear_path(b, m)) {
    return move_error_path_blocked;
  }

  return move_error_no_error;
}
