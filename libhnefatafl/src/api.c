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
#include "validation.h"
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

// Compute captures for each move and return as move_with_captures array.
// Frees the input moves array.
static move_with_captures *enrich_moves_with_captures(
    move *moves,
    int move_count,
    board b,
    bool next_is_black) {
  move_with_captures *result = malloc(sizeof(move_with_captures) * move_count);
  for (int i = 0; i < move_count; i++) {
    move m = moves[i];
    board copy = b;
    u64 dummy_hash = 0;
    layer captures;
    if (next_is_black) {
      copy = apply_black_move_m(copy, m.orig, m.dest);
      captures = apply_captures_z_black(&copy, &dummy_hash, m.dest);
    } else if (LOWEST_INDEX(copy.king) == m.orig) {
      copy = apply_king_move_m(copy, m.orig, m.dest);
      captures = apply_captures_z_white(&copy, &dummy_hash, m.dest);
    } else {
      copy = apply_white_move_m(copy, m.orig, m.dest);
      captures = apply_captures_z_white(&copy, &dummy_hash, m.dest);
    }
    result[i] = (move_with_captures){m, captures};
  }
  free(moves);
  return result;
}

void next_game_state_with_moves(
    const move *move_history,
    int history_count,
    move_with_captures **moves_out,
    int *move_count,
    game_status *gs,
    move_validation_result *validation_out,
    bool allow_repetition,
    move_result *last_move_out) {

  board *b;
  position_set *ps;
  bool is_black_turn;

  move_validation_result result = board_state_from_move_list(
      move_history,
      history_count,
      &b,
      &ps,
      &is_black_turn,
      gs,
      allow_repetition,
      last_move_out);

  *validation_out = result;

  if (result.error != move_error_no_error || *gs != status_ongoing) {
    *moves_out = NULL;
    *move_count = 0;
    return;
  }

  move *possible_moves;
  if (is_black_turn) {
    possible_moves = all_black_moves(*b, ps, move_count);
  } else {
    possible_moves = all_white_and_king_moves(*b, ps, move_count);
  }

  board board_copy = *b;
  free(b);
  destroy_position_set(ps);

  *moves_out = enrich_moves_with_captures(
      possible_moves,
      *move_count,
      board_copy,
      is_black_turn);
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
      allow_repetition,
      NULL);

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
    move *m,
    u64 *zobrist_hashes,
    int hash_count,
    move_result *result_out,
    game_status *status_out,
    move_with_captures **moves_out,
    int *move_count) {

  board board_state = from_compact(trusted_board);

  // Build position sets from caller-provided hashes
  position_set *first_ps = create_position_set(hash_count + 1);
  position_set *second_ps = create_position_set(hash_count + 1);

  for (int i = 0; i < hash_count; i++) {
    int deletion_index;
    if (insert_position(first_ps, zobrist_hashes[i], &deletion_index) != 0) {
      insert_position(second_ps, zobrist_hashes[i], &deletion_index);
    }
  }

  // Validate the move
  move_error error = validate_move(board_state, *m, is_black_turn);
  if (error != move_error_no_error) {
    destroy_position_set(first_ps);
    destroy_position_set(second_ps);
    return error;
  }

  // Apply the move
  u64 board_hash;
  layer captures;
  if (is_black_turn) {
    board_hash = next_hash_black(
        hash_for_board(board_state, is_black_turn),
        m->orig,
        m->dest);
    board_state = apply_black_move_m(board_state, m->orig, m->dest);
    captures = apply_captures_z_black(&board_state, &board_hash, m->dest);
  } else if (LOWEST_INDEX(board_state.king) == m->orig) {
    board_hash = next_hash_king(
        hash_for_board(board_state, is_black_turn),
        m->orig,
        m->dest);
    board_state = apply_king_move_m(board_state, m->orig, m->dest);
    captures = apply_captures_z_white(&board_state, &board_hash, m->dest);
  } else {
    board_hash = next_hash_white(
        hash_for_board(board_state, is_black_turn),
        m->orig,
        m->dest);
    board_state = apply_white_move_m(board_state, m->orig, m->dest);
    captures = apply_captures_z_white(&board_state, &board_hash, m->dest);
  }

  // Check threefold repetition
  int deletion_index;
  if (insert_position(first_ps, board_hash, &deletion_index) != 0) {
    if (insert_position(second_ps, board_hash, &deletion_index) != 0) {
      destroy_position_set(first_ps);
      destroy_position_set(second_ps);
      return move_error_threefold_repetition;
    }
  }

  // Populate move result
  result_out->move = *m;
  result_out->was_black_turn = is_black_turn;
  result_out->board = to_compact(&board_state);
  result_out->captures = captures;
  result_out->zobrist_hash = board_hash;

  // Check victory after the move
  bool next_is_black = !is_black_turn;
  if (next_is_black) {
    *status_out = white_victory_check(&board_state);
  } else {
    *status_out = black_victory_check(&board_state);
  }

  // Generate valid moves for the next turn
  if (*status_out != status_ongoing) {
    *moves_out = NULL;
    *move_count = 0;
  } else {
    move *raw_moves;
    if (next_is_black) {
      raw_moves = all_black_moves(board_state, second_ps, move_count);
    } else {
      raw_moves = all_white_and_king_moves(board_state, second_ps, move_count);
    }
    *moves_out = enrich_moves_with_captures(
        raw_moves,
        *move_count,
        board_state,
        next_is_black);
  }

  destroy_position_set(first_ps);
  destroy_position_set(second_ps);
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

int search_trusted(
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
      return 0;
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

  // Extract the best move from result. An empty PV means the engine
  // could not find a legal move for the side to move. Normally
  // victory check on the board from the previous move would have
  // caught this as a terminal state before the engine was called;
  // reaching this point means an invariant was violated upstream
  // (i.e. don't attempt search for a move on a terminal
  // board). Return non-zero so the caller can surface a structured
  // exception instead of crashing the process.
  if (result.pv.length == 0 || result.pv.moves == NULL) {
    destroy_pv_line(&result.pv);
    return 1;
  }

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
  // Assert the search didn't return a move that would create a 3-fold
  // repetition. Build a set of hashes that already appear at least twice in
  // history; if the new hash is in that set, this move would be the 3rd
  // occurrence, which is illegal. Reaching this branch means
  // either a bug in the search's repetition handling or the rare case where
  // every legal move from this position is 3-fold (the side has no legal
  // move and the state should have been flagged terminal before we were
  // called).
  {
    position_set *first_ps = create_position_set(hash_count + 1);
    position_set *dup_ps = create_position_set(hash_count + 1);
    for (int i = 0; i < hash_count; i++) {
      int deletion_index;
      if (insert_position(first_ps, zobrist_hashes[i], &deletion_index) != 0) {
        insert_position(dup_ps, zobrist_hashes[i], &deletion_index);
      }
    }

    if (check_position(dup_ps, new_hash)) {
      printf(
          "ERROR: search returned a move that would create a 3-fold "
          "repetition!\n");
      print_board(new_board_state);
      printf("new_hash=%lu\n", new_hash);
      destroy_position_set(first_ps);
      destroy_position_set(dup_ps);
      exit(1);
    }

    destroy_position_set(first_ps);
    destroy_position_set(dup_ps);
  }
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
  return 0;
}
