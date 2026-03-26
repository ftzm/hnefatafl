#include "api.h"
#include "board.h"
#include "capture.h"
#include "game_state.h"
#include "io.h"
#include "layer.h"
#include "move.h"
#include "position_set.h"
#include "search.h"
#include "transposition_table.h"
#include "victory.h"
#include "zobrist.h"

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

void next_game_state_with_moves(
    const move *move_history,
    int history_count,
    move **moves_out,
    int *move_count,
    game_status *gs,
    move_validation_result *validation_out,
    bool allow_repetition) {

  // First get the game state - this handles all validation
  next_game_state(
      move_history,
      history_count,
      gs,
      validation_out,
      allow_repetition);

  if (validation_out->error != move_error_no_error || *gs != status_ongoing) {
    *moves_out = NULL;
    *move_count = 0;
    return;
  }

  // Game is ongoing and no errors, generate moves
  board *b;
  position_set *ps;
  bool is_black_turn;

  // Get board state for move generation
  board_state_from_move_list(
      move_history,
      history_count,
      &b,
      &ps,
      &is_black_turn,
      gs,
      allow_repetition);

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
}

void next_game_state(
    const move *move_history,
    int history_count,
    game_status *gs,
    move_validation_result *validation_out,
    bool allow_repetition) {

  board *b;
  position_set *ps;
  bool is_black_turn;

  // Get board state from move history
  move_validation_result result = board_state_from_move_list(
      move_history,
      history_count,
      &b,
      &ps,
      &is_black_turn,
      gs,
      allow_repetition);

  if (result.error != move_error_no_error) {
    *validation_out = result;
    return;
  }

  // Clean up
  free(b);
  destroy_position_set(ps);

  *validation_out = result;
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
 * Writes the final game status to final_status_out.
 * Caller must free the returned array.
 */
move_result *apply_move_sequence(
    const move *moves,
    int move_count,
    game_status *final_status_out) {
  move_result *move_results = malloc(sizeof(move_result) * move_count);

  board current_board = start_board;
  bool is_black_turn = true;
  u64 current_hash = hash_for_board(start_board, is_black_turn);

  for (int i = 0; i < move_count; i++) {
    move m = moves[i];

    // Store the move and whose turn it was
    move_results[i].move = m;
    move_results[i].was_black_turn = is_black_turn;

    // Apply the move and captures, calculating the new hash
    layer captures;
    if (is_black_turn) {
      current_board = apply_black_move_m(current_board, m.orig, m.dest);
      current_hash = next_hash_black(current_hash, m.orig, m.dest);
      captures = apply_captures_z_black(&current_board, &current_hash, m.dest);
    } else if (LOWEST_INDEX(current_board.king) == m.orig) {
      // King move
      current_board = apply_king_move_m(current_board, m.orig, m.dest);
      current_hash = next_hash_king(current_hash, m.orig, m.dest);
      captures = apply_captures_z_white(&current_board, &current_hash, m.dest);
    } else {
      // White pawn move
      current_board = apply_white_move_m(current_board, m.orig, m.dest);
      current_hash = next_hash_white(current_hash, m.orig, m.dest);
      captures = apply_captures_z_white(&current_board, &current_hash, m.dest);
    }

    // Store the captures, final board state, and zobrist hash
    move_results[i].captures = captures;
    move_results[i].board = to_compact(&current_board);
    move_results[i].zobrist_hash = current_hash;

    // Alternate turns
    is_black_turn = !is_black_turn;
  }

  // Check final game status after all moves are applied
  if (is_black_turn) {
    // It's black's turn, so check if white has won
    *final_status_out = white_victory_check(&current_board);
  } else {
    // It's white's turn, so check if black has won
    *final_status_out = black_victory_check(&current_board);
  }

  return move_results;
}

void print_search_stats(const stats *s) {
  int total_search_positions =
      s->search_positions_black + s->search_positions_white;
  int total_search_cutoffs =
      s->search_beta_cutoff_black + s->search_beta_cutoff_white;
  int total_quiescence_positions =
      s->quiescence_positions_black + s->quiescence_positions_white;
  int total_quiescence_cutoffs =
      s->quiencence_beta_cutoff_black + s->quiencence_beta_cutoff_white;

  printf("=== Search Stats ===\n");
  printf("Search - Black positions: %d\n", s->search_positions_black);
  printf("Search - Black beta cutoffs: %d\n", s->search_beta_cutoff_black);
  printf("Search - White positions: %d\n", s->search_positions_white);
  printf("Search - White beta cutoffs: %d\n", s->search_beta_cutoff_white);
  printf("Search - Total positions: %d\n", total_search_positions);
  printf("Search - Total beta cutoffs: %d\n", total_search_cutoffs);
  printf("Quiescence - Black positions: %d\n", s->quiescence_positions_black);
  printf(
      "Quiescence - Black beta cutoffs: %d\n",
      s->quiencence_beta_cutoff_black);
  printf("Quiescence - White positions: %d\n", s->quiescence_positions_white);
  printf(
      "Quiescence - White beta cutoffs: %d\n",
      s->quiencence_beta_cutoff_white);
  printf("Quiescence - Total positions: %d\n", total_quiescence_positions);
  printf("Quiescence - Total beta cutoffs: %d\n", total_quiescence_cutoffs);
  printf("Quiescence - Limit reached: %d\n", s->quiescence_limit_reached);
  printf("Repeat moves encountered: %d\n", s->repeat_moves_encountered);
  printf("TT - Hits: %d\n", s->tt_hits);
  printf("TT - Cutoffs: %d\n", s->tt_cutoffs);
  printf("===================\n");
}

void search_trusted(
    compact_board *trusted_board,
    bool is_black_turn,
    u64 *zobrist_hashes,
    int hash_count,
    _Atomic bool *should_stop,
    transposition_table *tt,
    move *move_out,
    compact_board *board_out,
    layer *captures_out,
    u64 *hash_out,
    game_status *status_out,
    bool enable_administrative_endings) {

  // Convert compact board to full board
  board board_state = from_compact(trusted_board);

  if (enable_administrative_endings) {
    game_status admin_status =
        administrative_ending_check(&board_state, is_black_turn);
    if (admin_status != status_ongoing) {
      *move_out = (move){0, 0};
      *board_out = *trusted_board;
      *captures_out = EMPTY_LAYER;
      *hash_out = 0;
      *status_out = admin_status;
      return;
    }
  }

  // Calculate current position hash
  u64 position_hash = hash_for_board(board_state, is_black_turn);

  // Run iterative deepening search
  search_result result = search_runner_iterative_trusted(
      board_state,
      8,
      should_stop,
      is_black_turn,
      zobrist_hashes,
      hash_count,
      tt);

#ifndef NDEBUG
  // Check statistics for early abortion
  if ((result.statistics.search_positions_black
       + result.statistics.search_positions_white
       < 2)
      && (result.statistics.repeat_moves_encountered > 0)) {
    printf("search immediately aborted due to illegal repetition\n");
    exit(1);
  }
#endif

  // Extract the best move from result
#ifndef NDEBUG
  if (result.pv.length == 0 || result.pv.moves == NULL) {
    printf("null move encountered (no moves in PV)\n");
    print_search_stats(&result.statistics);
    exit(1);
  }
#endif

  move best_move = result.pv.moves[0];

#ifndef NDEBUG
  if (best_move.orig == 0 && best_move.dest == 0) {
    printf("null move encountered\n");
    print_search_stats(&result.statistics);
    exit(1);
  }
#endif

  // Apply the move and calculate updated state
  board new_board_state = board_state;
  u64 new_hash = position_hash;
  layer captures;

  if (is_black_turn) {
    new_board_state =
        apply_black_move_m(new_board_state, best_move.orig, best_move.dest);
    new_hash = next_hash_black(new_hash, best_move.orig, best_move.dest);
    captures =
        apply_captures_z_black(&new_board_state, &new_hash, best_move.dest);
  } else {
    // Check if it's a king move by comparing origin with king position
    u8 king_pos = LOWEST_INDEX(board_state.king);
    if (best_move.orig == king_pos) {
      new_board_state =
          apply_king_move_m(new_board_state, best_move.orig, best_move.dest);
      new_hash = next_hash_king(new_hash, best_move.orig, best_move.dest);
      captures =
          apply_captures_z_white(&new_board_state, &new_hash, best_move.dest);
    } else {
      new_board_state =
          apply_white_move_m(new_board_state, best_move.orig, best_move.dest);
      new_hash = next_hash_white(new_hash, best_move.orig, best_move.dest);
      captures =
          apply_captures_z_white(&new_board_state, &new_hash, best_move.dest);
    }
  }

#ifndef NDEBUG
  // Check if new position hash already exists in the zobrist hashes
  // We need to create a temporary position set to check this
  position_set *temp_positions = create_position_set(hash_count + 100);
  for (int i = 0; i < hash_count; i++) {
    if (i == 0 && zobrist_hashes[i] == position_hash) {
      continue;
    }
    int deletion_index;
    insert_position(temp_positions, zobrist_hashes[i], &deletion_index);
  }

  if (check_position(temp_positions, new_hash)) {
    printf("ERROR: Position repetition detected after move!\n");
    print_board(new_board_state);

    // Find which index in zobrist_hashes matches new_hash
    int matching_index = -1;
    for (int i = 0; i < hash_count; i++) {
      if (zobrist_hashes[i] == new_hash) {
        matching_index = i;
        break;
      }
    }

    printf(
        "Matching hash found at index %d out of %d total hashes\n",
        matching_index,
        hash_count);
    destroy_position_set(temp_positions);
    exit(1);
  }

  destroy_position_set(temp_positions);
#endif

  // Check game status on the updated board
  game_status status;
  if (is_black_turn) {
    // After black moves, check if black has won
    status = black_victory_check(&new_board_state);
  } else {
    // After white moves, check if white has won
    status = white_victory_check(&new_board_state);
  }

  // Clean up result
  destroy_pv_line(&result.pv);

  // Write results to output parameters
  *move_out = best_move;
  *board_out = to_compact(&new_board_state);
  *captures_out = captures;
  *hash_out = new_hash;
  *status_out = status;
}
