#include "api.h"
#include "board.h"
#include "game_state.h"
#include "layer.h"
#include "move.h"
#include "position_set.h"
#include "victory.h"

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

int next_game_state_with_moves(
    const move *move_history,
    int history_count,
    move **moves_out,
    int *move_count,
    game_status *gs) {

  board *b;
  position_set *ps;
  bool is_black_turn;

  // Get board state from move history
  int result = board_state_from_move_list(
      move_history,
      history_count,
      &b,
      &ps,
      &is_black_turn,
      gs);

  if (result != 0) {
    *moves_out = NULL;
    *move_count = 0;
    return result;
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

  *moves_out = possible_moves;
  return 0;
}

int next_game_state(
    const move *move_history,
    int history_count,
    game_status *gs) {

  board *b;
  position_set *ps;
  bool is_black_turn;

  // Get board state from move history
  int result = board_state_from_move_list(
      move_history,
      history_count,
      &b,
      &ps,
      &is_black_turn,
      gs);

  if (result != 0) {
    return result;
  }

  // Clean up
  free(b);
  destroy_position_set(ps);

  return 0;
}

int next_game_state_with_moves_trusted(
    compact_board *trusted_board,
    bool is_black_turn,
    const move *move_history,
    int history_count,
    move **moves_out,
    int *move_count,
    game_status *gs) {

  // Convert compact board to full board
  board board_state = from_compact(trusted_board);

  // Build position sets from move history for threefold repetition tracking
  // We need two sets: first_ps tracks first occurrences, second_ps tracks
  // second occurrences
  position_set *first_ps = create_position_set(history_count);
  position_set *second_ps = create_position_set(history_count);

  // Replay moves to build the position sets
  board temp_board = start_board;
  bool temp_is_black_turn = true;

  for (int i = 0; i < history_count; i++) {
    move m = move_history[i];

    // Calculate the board hash after this move
    u64 board_hash;
    if (temp_is_black_turn) {
      board_hash = next_hash_black(
          hash_for_board(temp_board, temp_is_black_turn),
          m.orig,
          m.dest);
      temp_board = apply_black_move_m(temp_board, m.orig, m.dest);
      apply_captures_z_black(&temp_board, &board_hash, m.dest);
    } else if (LOWEST_INDEX(temp_board.king) == m.orig) {
      board_hash = next_hash_king(
          hash_for_board(temp_board, temp_is_black_turn),
          m.orig,
          m.dest);
      temp_board = apply_king_move_m(temp_board, m.orig, m.dest);
      apply_captures_z_white(&temp_board, &board_hash, m.dest);
    } else {
      board_hash = next_hash_white(
          hash_for_board(temp_board, temp_is_black_turn),
          m.orig,
          m.dest);
      temp_board = apply_white_move_m(temp_board, m.orig, m.dest);
      apply_captures_z_white(&temp_board, &board_hash, m.dest);
    }

    // Track position occurrences using two-stage logic
    int deletion_index;
    if (insert_position(first_ps, board_hash, &deletion_index) != 0) {
      // Position already exists in first set, add to second set
      insert_position(second_ps, board_hash, &deletion_index);
    }

    temp_is_black_turn = !temp_is_black_turn;
  }

  // Check game status based on trusted board
  if (is_black_turn) {
    *gs = white_victory_check(&board_state);
  } else {
    *gs = black_victory_check(&board_state);
  }

  // If game is over, no moves to generate
  if (*gs != status_ongoing) {
    *moves_out = NULL;
    *move_count = 0;
    destroy_position_set(first_ps);
    destroy_position_set(second_ps);
    return 0;
  }

  // Generate moves for the current position using second_ps to avoid threefold
  // repetition
  move *possible_moves;
  if (is_black_turn) {
    possible_moves = all_black_moves(board_state, second_ps, move_count);
  } else {
    possible_moves =
        all_white_and_king_moves(board_state, second_ps, move_count);
  }

  // Clean up
  destroy_position_set(first_ps);
  destroy_position_set(second_ps);

  *moves_out = possible_moves;
  return 0;
}

/* Apply a sequence of moves and return detailed data about each move.
 * Does not perform move validation or game state logic - just applies moves
 * and captures. Returns dynamically allocated array of move_result structures.
 * Caller must free the returned array.
 */
move_result *apply_move_sequence(const move *moves, int move_count) {
  move_result *move_results = malloc(sizeof(move_result) * move_count);

  board current_board = start_board;
  bool is_black_turn = true;

  for (int i = 0; i < move_count; i++) {
    move m = moves[i];

    // Store the move and whose turn it was
    move_results[i].move = m;
    move_results[i].was_black_turn = is_black_turn;

    // Apply the move and captures
    layer captures;
    if (is_black_turn) {
      current_board = apply_black_move_m(current_board, m.orig, m.dest);
      u64 dummy_hash = 0;
      captures = apply_captures_z_black(&current_board, &dummy_hash, m.dest);
    } else if (LOWEST_INDEX(current_board.king) == m.orig) {
      // King move
      current_board = apply_king_move_m(current_board, m.orig, m.dest);
      u64 dummy_hash = 0;
      captures = apply_captures_z_white(&current_board, &dummy_hash, m.dest);
    } else {
      // White pawn move
      current_board = apply_white_move_m(current_board, m.orig, m.dest);
      u64 dummy_hash = 0;
      captures = apply_captures_z_white(&current_board, &dummy_hash, m.dest);
    }

    // Store the captures and final board state
    move_results[i].captures = captures;
    move_results[i].board = to_compact(&current_board);

    // Alternate turns
    is_black_turn = !is_black_turn;
  }

  return move_results;
}
