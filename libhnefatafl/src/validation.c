#include "validation.h"

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
    layer occupied = board_occ(b);
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

bool validate_move(board b, move m, bool is_black_turn) {
  piece_type moving_piece = get_piece_at(b, m.orig);
  return is_correct_piece(is_black_turn, moving_piece)
         && is_valid_destination(b, m.dest, moving_piece)
         && is_orthogonal_move(m)
         && has_clear_path(b, m);
}