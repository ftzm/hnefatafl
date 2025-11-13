#include "lib.h"

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
          gs)
      != 0) {
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

bool is_valid_destination(board b, u8 position, piece_type moving_piece) {
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
