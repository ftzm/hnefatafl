#include "search.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "io.h"
#include "move.h"
#include "position_set.h"
#include "score.h"
#include "ubench.h"
#include "zobrist.h"
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  printf("===================\n");
}

void print_pv_line(const pv_line *pv) {
  printf("=== Principal Variation ===\n");
  printf("Score: %d\n", pv->score);
  printf("Moves (%d): ", pv->length);
  for (int i = 0; i < pv->length; i++) {
    printf("%d->%d", pv->moves[i].orig, pv->moves[i].dest);
    if (i < pv->length - 1)
      printf(" ");
  }
  printf("\n\n");

  // Print board positions for each move (following test pattern)
  board current_board = start_board;
  bool is_black_turn = pv->is_black_turn;

  printf("Starting position:\n");
  print_board(current_board);

  for (int i = 0; i < pv->length; i++) {
    move m = pv->moves[i];

    printf("Move %d: %s ", i + 1, is_black_turn ? "Black" : "White");
    print_move(m.orig, m.dest);

    // Apply the move and captures (following test pattern)
    layer captures;
    u64 dummy_zobrist = 0;
    if (is_black_turn) {
      current_board = apply_black_move_m(current_board, m.orig, m.dest);
      captures = apply_captures_z_black(&current_board, &dummy_zobrist, m.dest);
    } else {
      current_board = apply_white_move_m(current_board, m.orig, m.dest);
      captures = apply_captures_z_white(&current_board, &dummy_zobrist, m.dest);
    }

    // Print the board with move highlighting
    print_board_move(current_board, m.orig, m.dest, captures);

    // Calculate and print the position score
    score_weights weights = init_default_weights();
    score_state score_st = init_score_state(&weights, &current_board);
    i32 position_score;
    if (is_black_turn) {
      // Just moved black, so evaluate from white's perspective
      position_score = white_score(&weights, &score_st, &current_board);
    } else {
      // Just moved white, so evaluate from black's perspective
      position_score = black_score(&weights, &score_st, &current_board);
    }
    printf("Position score: %d\n\n", position_score);

    is_black_turn = !is_black_turn;
  }
  printf("===========================\n");
}

pv_line create_pv_line(pv *pv_data, bool is_black_turn, i32 result) {
  move *moves = malloc(sizeof(move) * pv_data->pv_length[0]);
  memcpy(moves, pv_data->pv_table[0], sizeof(move) * pv_data->pv_length[0]);
  return (pv_line){is_black_turn, moves, pv_data->pv_length[0], result};
}

UBENCH_EX(search, black_depth_3) {
  static bool black_depth_3_printed = false;
  static pv_line saved_result;
  static stats saved_statistics;

  UBENCH_DO_BENCHMARK() {
    _Atomic bool should_stop = false;
    search_result search_res =
        search_black_runner(start_board, 3, &should_stop);
    pv_line result = search_res.pv;
    stats statistics = search_res.statistics;
    if (!black_depth_3_printed) {
      saved_result = result;
      saved_statistics = statistics;
    } else {
      destroy_pv_line(&result);
    }
    UBENCH_DO_NOTHING(&result);
  }

  if (!black_depth_3_printed) {
    printf("\n=== Black Depth 3 Stats ===\n");
    print_search_stats(&saved_statistics);
    // print_pv_line(&saved_result);
    destroy_pv_line(&saved_result);
    black_depth_3_printed = true;
  }
}

UBENCH_EX(search, black_depth_4) {
  static bool black_depth_4_printed = false;
  static pv_line saved_result;
  static stats saved_statistics;

  UBENCH_DO_BENCHMARK() {
    _Atomic bool should_stop = false;
    search_result search_res =
        search_black_runner(start_board, 4, &should_stop);
    pv_line result = search_res.pv;
    stats statistics = search_res.statistics;
    if (!black_depth_4_printed) {
      saved_result = result;
      saved_statistics = statistics;
    } else {
      destroy_pv_line(&result);
    }
    UBENCH_DO_NOTHING(&result);
  }

  if (!black_depth_4_printed) {
    printf("\n=== Black Depth 4 Stats ===\n");
    print_search_stats(&saved_statistics);
    // print_pv_line(&saved_result);
    destroy_pv_line(&saved_result);
    black_depth_4_printed = true;
  }
}

UBENCH_EX(search, black_depth_5) {
  static bool black_depth_5_printed = false;
  static pv_line saved_result;
  static stats saved_statistics;

  UBENCH_DO_BENCHMARK() {
    _Atomic bool should_stop = false;
    search_result search_res =
        search_black_runner(start_board, 5, &should_stop);
    pv_line result = search_res.pv;
    stats statistics = search_res.statistics;
    if (!black_depth_5_printed) {
      saved_result = result;
      saved_statistics = statistics;
    } else {
      destroy_pv_line(&result);
    }
    UBENCH_DO_NOTHING(&result);
  }

  if (!black_depth_5_printed) {
    printf("\n=== Black Depth 5 Stats ===\n");
    print_search_stats(&saved_statistics);
    // print_pv_line(&saved_result);
    destroy_pv_line(&saved_result);
    black_depth_5_printed = true;
  }
}

UBENCH_EX(search, black_iterative_depth_3) {
  static bool black_iterative_depth_3_printed = false;
  static pv_line saved_result;
  static stats saved_statistics;

  UBENCH_DO_BENCHMARK() {
    _Atomic bool should_stop = false;
    search_result search_res =
        search_black_runner_iterative(start_board, 3, &should_stop);
    pv_line result = search_res.pv;
    stats statistics = search_res.statistics;
    if (!black_iterative_depth_3_printed) {
      saved_result = result;
      saved_statistics = statistics;
    } else {
      destroy_pv_line(&result);
    }
    UBENCH_DO_NOTHING(&result);
  }

  if (!black_iterative_depth_3_printed) {
    printf("\n=== Black Iterative Depth 3 Stats ===\n");
    print_search_stats(&saved_statistics);
    // print_pv_line(&saved_result);
    destroy_pv_line(&saved_result);
    black_iterative_depth_3_printed = true;
  }
}

UBENCH_EX(search, black_iterative_depth_4) {
  static bool black_iterative_depth_4_printed = false;
  static pv_line saved_result;
  static stats saved_statistics;

  UBENCH_DO_BENCHMARK() {
    _Atomic bool should_stop = false;
    search_result search_res =
        search_black_runner_iterative(start_board, 4, &should_stop);
    pv_line result = search_res.pv;
    stats statistics = search_res.statistics;
    if (!black_iterative_depth_4_printed) {
      saved_result = result;
      saved_statistics = statistics;
    } else {
      destroy_pv_line(&result);
    }
    UBENCH_DO_NOTHING(&result);
  }

  if (!black_iterative_depth_4_printed) {
    printf("\n=== Black Iterative Depth 4 Stats ===\n");
    print_search_stats(&saved_statistics);
    // print_pv_line(&saved_result);
    destroy_pv_line(&saved_result);
    black_iterative_depth_4_printed = true;
  }
}

UBENCH_EX(search, black_iterative_depth_5) {
  static bool black_iterative_depth_5_printed = false;
  static pv_line saved_result;
  static stats saved_statistics;

  UBENCH_DO_BENCHMARK() {
    _Atomic bool should_stop = false;
    search_result search_res =
        search_black_runner_iterative(start_board, 5, &should_stop);
    pv_line result = search_res.pv;
    stats statistics = search_res.statistics;
    if (!black_iterative_depth_5_printed) {
      saved_result = result;
      saved_statistics = statistics;
    } else {
      destroy_pv_line(&result);
    }
    UBENCH_DO_NOTHING(&result);
  }

  if (!black_iterative_depth_5_printed) {
    printf("\n=== Black Iterative Depth 5 Stats ===\n");
    print_search_stats(&saved_statistics);
    // print_pv_line(&saved_result);
    destroy_pv_line(&saved_result);
    black_iterative_depth_5_printed = true;
  }
}

// needs to be at top level
UBENCH_STATE();

int main() { return ubench_main(0, NULL); }
