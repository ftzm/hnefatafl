#include "search.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "io.h"
#include "king_mobility.h"
#include "layer.h"
#include "limits.h"
#include "move.h"
#include "position_set.h"
#include "score.h"
#include "stdlib.h"
#include "string.h"
#include "transposition_table.h"
#include "validation.h"
#include "victory.h"
#include "x86intrin.h" // IWYU pragma: export
#include "zobrist.h"
#include "zobrist_constants.h"
#include <pthread.h>
#include <time.h>
#include <unistd.h>

// Timer thread data structure
typedef struct {
  _Atomic bool *should_stop;
  _Atomic bool *search_finished;
  int time_limit_ms;
} timer_data;

// Timer thread function
void *timer_thread(void *arg) {
  timer_data *data = (timer_data *)arg;

  // Check periodically instead of one long sleep
  // Use the time limit itself for intervals below 100ms, otherwise use 100ms
  int sleep_interval_ms =
      (data->time_limit_ms < 100) ? data->time_limit_ms : 100;
  if (sleep_interval_ms < 1)
    sleep_interval_ms = 1; // Minimum 1ms

  int elapsed = 0;
  while (elapsed < data->time_limit_ms) {
    // Check if search finished early
    if (atomic_load(data->search_finished)) {
      return NULL; // Exit early
    }

    // Sleep in appropriate intervals (usleep takes microseconds)
    usleep(sleep_interval_ms * 1000);
    elapsed += sleep_interval_ms;
  }

  // Time limit reached - signal search to stop
  atomic_store(data->should_stop, true);
  return NULL;
}

// typedef struct negamax_ab_result {
//   move _move;
//   board _board;
//   i32 _score;
//   score_state ss;
// } negamax_ab_result;
// // move moves_table[11][240];
// // board boards_table[11][240];
// // board scores_table[11][240];
// const int MAX_DEPTH = 32;
// int PV_LENGTH[MAX_DEPTH];
// move PV_TABLE[MAX_DEPTH][MAX_DEPTH];
// board PV_TABLE_BOARDS[MAX_DEPTH][MAX_DEPTH];
// move PREV_PV[MAX_DEPTH];
// int PREV_PV_LENGTH;
// move KILLER_MOVES[MAX_DEPTH][2];
//
// i32 negamax_ab_sorted_pv(
//     const move m,
//     const board b,
//     const team_repetitions r,
//     const bool is_black_turn,
//     const int depth,
//     const int ply,
//     i32 alpha,
//     const i32 beta,
//     int *tally,
//     const bool is_pv,
//     const score_state ss,
//     const bool allow_null_move,
//     const struct ai_settings &ai_settings) {
//   pv_data->pv_length[ply] = ply;
//
//   /*
//   if (ply > 31) {
//     printf("max depth exceeded\n");
//     exit(1);
//   }
//   */
//
//   i32 game_over_score = 0;
//   bool game_over = game_over_check(b, is_black_turn, game_over_score);
//   if (game_over) {
//     return game_over_score;
//   }
//
//   int total = 0;
//   move moves_table[324];
//   board boards_table[324];
//   u8 cap_counts[324] = {0};
//
//   if (depth > 3 && ply > 0 && allow_null_move && !is_pv) {
//     // Null move heuristic
//     // TODO: figure out the most suitable depth cutoff (if any)
//     // TODO: add check to see if the static evaluation of the board is above
//     // beta
//     // TODO: ensure king is not in check/on verge of escape, otherwise this
//     // will be unsound.
//     int null_shortening = 2;
//     i32 null_result = -negamax_ab_sorted_pv(
//         m,
//         b,
//         r,
//         !is_black_turn,
//         depth - 1 - null_shortening,
//         ply + 1 + null_shortening,
//         -beta,
//         -beta + 1,
//         tally,
//         false,
//         ss,
//         false,
//         ai_settings);
//     if (null_result >= beta) {
//       return beta;
//     }
//   }
//   /*
//    */
//
//   if (depth <= 0) {
//     if (ply < MAX_DEPTH) {
//       if (is_black_turn) {
//         get_capture_move_boards<true>(
//             boards_table, b, &total, moves_table, cap_counts);
//       } else {
//         get_capture_move_boards<false>(
//             boards_table, b, &total, moves_table, cap_counts);
//       }
//     }
//     if (total == 0) {
//       if (!b.black[0] && !b.black[1]) {
//         printf("ply: %d\n", ply);
//         print_board(b);
//       }
//       return score_board(b, is_black_turn) + ss.get_score(is_black_turn);
//     }
//   } else {
//
//     if (is_black_turn) {
//       get_team_moves<true>(b, &total, moves_table, cap_counts, boards_table);
//     } else {
//       get_king_moves(b, &total, moves_table, cap_counts, boards_table);
//       get_team_moves<false>(b, &total, moves_table, cap_counts,
//       boards_table);
//     }
//   }
//
//   negamax_ab_result combi[324];
//   for (int i = 0; i < total; i++) {
//     combi[i] = (negamax_ab_result){moves_table[i], boards_table[i]};
//
//     if (combi[i]._move.orig == 44 && combi[i]._move.dest == 22 &&
//         ss.se_guard_count == 3) {
//       print_board(combi[i]._board);
//     }
//
//     // update score state
//     combi[i].ss = update_score_state(
//         ss, combi[i]._move, is_black_turn ? black_type : white_type);
//
//     // update score state piece counts
//     if (is_black_turn) {
//       combi[i].ss.white_pawn_count -= cap_counts[i];
//     } else {
//       combi[i].ss.black_pawn_count -= cap_counts[i];
//     }
//
//     // TODO: move this somewhere closer to the core capture code se we don't
//     // have to re-calculate the indexes of captures
//     if (cap_counts[i]) {
//       if (is_black_turn) {
//         layer cap_layer = b.white ^ combi[i]._board.white;
//         while (cap_layer[0]) {
//           int cap_index = _tzcnt_u64(cap_layer[0]);
//           combi[i].ss.pst_white_score -= ai_settings.white_pst[cap_index];
//           cap_layer[0] &= cap_layer[0] - 1;
//         }
//         while (cap_layer[1]) {
//           int cap_index = _tzcnt_u64(cap_layer[1]) + 64;
//           combi[i].ss.pst_white_score -= ai_settings.white_pst[cap_index];
//           cap_layer[1] &= cap_layer[1] - 1;
//         }
//       } else {
//         layer cap_layer = b.black ^ combi[i]._board.black;
//         while (cap_layer[0]) {
//           int cap_index = _tzcnt_u64(cap_layer[0]);
//           update_guard_score_state_capture(combi[i].ss, cap_index);
//           combi[i].ss.pst_black_score -= ai_settings.black_pst[cap_index];
//           cap_layer[0] &= cap_layer[0] - 1;
//         }
//         while (cap_layer[1]) {
//           int cap_index = _tzcnt_u64(cap_layer[1]) + 64;
//           update_guard_score_state_capture(combi[i].ss, cap_index);
//           combi[i].ss.pst_black_score -= ai_settings.black_pst[cap_index];
//           cap_layer[1] &= cap_layer[1] - 1;
//         }
//       }
//     }
//
//     u8 king_pos =
//         b.king[0] ? _tzcnt_u64(b.king[0]) : _tzcnt_u64(b.king[1]) + 64;
//
//     // update pst score
//     if (is_black_turn) {
//       combi[i].ss.pst_black_score -=
//       ai_settings.black_pst[combi[i]._move.orig]; combi[i].ss.pst_black_score
//       += ai_settings.black_pst[combi[i]._move.dest];
//     } else if (combi[i]._move.orig == !king_pos) {
//       combi[i].ss.pst_white_score -=
//       ai_settings.white_pst[combi[i]._move.orig]; combi[i].ss.pst_white_score
//       += ai_settings.white_pst[combi[i]._move.dest];
//     } else {
//       combi[i].ss.pst_king_score -=
//       ai_settings.king_pst[combi[i]._move.orig]; combi[i].ss.pst_king_score
//       += ai_settings.king_pst[combi[i]._move.dest];
//     }
//
//     combi[i]._score = depth > 1 ? combi[i].ss.get_score(is_black_turn) : 0;
//     // Add bonus if killer move
//     if (combi[i]._move == KILLER_MOVES[ply][0] ||
//         combi[i]._move == KILLER_MOVES[ply][1])
//       combi[i]._score += 10000000;
//     // Add bonus if we're following the
//     if (is_pv && (combi[i]._move == PREV_PV[ply])) {
//       combi[i]._score += 100000000;
//     }
//   }
//
//   // niave sort
//   /*
//   if (depth > 1) {
//     std::sort(combi, combi + total, [](const auto &lhs, const auto &rhs) {
//       return lhs._score > rhs._score;
//     });
//   }
//   */
//
//   // start with a bogus best
//   i32 best = MIN_SCORE;
//   negamax_ab_result tmp;
//   for (int i = 0; i < total; i++) {
//     if (depth > 1) {
//       int best_index = i;
//       for (int j = i + 1; j < total; j++) {
//         if (combi[j]._score > combi[best_index]._score) {
//           best_index = j;
//         }
//       }
//       if (best_index != i) {
//         tmp = combi[i];
//         combi[i] = combi[best_index];
//         combi[best_index] = tmp;
//       }
//     }
//
//     // TODO: move this to leaf node
//     (*tally)++;
//
//     // update and check repetitions
//     team_repetitions new_r =
//         update_team_repetitions(r, combi[i]._move, is_black_turn);
//     if (new_r.over_limit(is_black_turn)) {
//       continue;
//     }
//
//     // Late Move Reduction: if we have more than two ply left, and
//     // we've already examined the best 25 moves based on the move
//     // ordering heuristics, then search this move 2 ply more shallow
//     // with a narrow window, and skip if we don't raise alpha (which
//     // indicates that this move, at least superficially, conforms to
//     // our expectation in being poor)
//     if (depth > 2 && i > 25) {
//       i32 lmr_eval = -negamax_ab_sorted_pv(
//           combi[i]._move,
//           combi[i]._board,
//           new_r,
//           !is_black_turn,
//           depth == 0 ? 0 : depth - 2,
//           ply + 2,
//           -alpha - 1,
//           -alpha,
//           tally,
//           (is_pv && !i),
//           combi[i].ss,
//           true,
//           ai_settings);
//       if (lmr_eval <= alpha) {
//         continue;
//       }
//     }
//
//     // calcualte result and negate score the is_pv parameter is (is_pv
//     // && !i) because if this _is_ the pv then the first entry must be
//     // the next PV node due to the bonus applied above, unless the.
//     // next PV move wasn't found, which would be a real bug.
//
//     i32 eval = -negamax_ab_sorted_pv(
//         combi[i]._move,
//         combi[i]._board,
//         new_r,
//         !is_black_turn,
//         depth == 0 ? 0 : depth - 1,
//         ply + 1,
//         -beta,
//         -alpha,
//         tally,
//         (is_pv && !i),
//         combi[i].ss,
//         true,
//         ai_settings);
//
//     /*
//     i32 eval;
//     if (i == 0) {
//       i32 eval = -negamax_ab_sorted_pv(
//           combi[i]._move, combi[i]._board, !is_black_turn,
//           depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
//           (is_pv && !i), combi[i].ss, true);
//     } else {
//       // search with null window
//       i32 eval = -negamax_ab_sorted_pv(
//           combi[i]._move, combi[i]._board, !is_black_turn,
//           depth == 0 ? 0 : depth - 1, ply + 1, -alpha-1, -alpha, tally,
//           (is_pv && !i), combi[i].ss, true);
//       if (alpha < eval && eval < beta) {
//         // if failed high, research with full window
//         i32 eval = -negamax_ab_sorted_pv(
//             combi[i]._move, combi[i]._board, !is_black_turn,
//             depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
//             (is_pv && !i), combi[i].ss, true);
//       }
//     }
//     */
//
//     // if (depth == 1 && (m.orig == 115 && m.dest == 114)) printf("score:
//     %d\n",
//     // next_result);
//     // if (depth == 1) printf("score: %d\n", eval);
//     // if (depth == 1) print_board(combi[i]._board);
//
//     if (eval > best) {
//       best = eval;
//     }
//     if (best > alpha) {
//       // ALPHA BETA HANDLING
//       // ---------------------------------------------------------------
//       alpha = best;
//
//       // PV HANDLING
//       // ---------------------------------------------------------------
//       assign
//       // move discovered here
//       PV_TABLE[ply][ply] = combi[i]._move;
//       PV_TABLE_BOARDS[ply][ply] = combi[i]._board; // me only
//       // copy up moves discovered at lower depths
//       for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++)
//       {
//         /*
//         if (next_ply > 31) {
//           printf("next ply over limit\n");
//           exit(1);
//         }
//         */
//         PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
//         PV_TABLE_BOARDS[ply][next_ply] =
//             PV_TABLE_BOARDS[ply + 1][next_ply]; // me only
//       }
//       // adjust pv length
//       PV_LENGTH[ply] = PV_LENGTH[ply + 1];
//
//       // KILLER HANDLING
//       // ---------------------------------------------------------------
//       // TODO: exclude captures
//       // NOTE: this should typically be handled at a beta cutoff, but
//       // benchmarking shows better performance if killers are set
//       // every time alpha is raised
//       KILLER_MOVES[ply][1] = KILLER_MOVES[ply][0];
//       KILLER_MOVES[ply][0] = combi[i]._move;
//     }
//     if (alpha > beta) {
//       break;
//     }
//   }
//   // if (depth == 1 && (m.orig == 115 && m.dest == 114))
//   // printf("----------------------");
//   // if (depth == 1) printf("----------------------");
//
//   return best;
// }
//
// enum Flag : u8 {
//   lower_bound = 1,
//   exact = 2,
//   upper_bound = 3,
// };
// struct search_result {
//   move m;
//   board b;
//   i32 s;
//   team_repetitions r;
// };

// search_result negamax_ab_sorted_pv_runner(
//     board b,
//     team_repetitions r,
//     bool is_black,
//     int depth,
//     struct ai_settings ai_settings) {
//   int tally = 0;
//   score_state s = init_score_state(b);
//   /*
//   printf("nw: %d\n", s.nw_guard_count);
//   printf("ne: %d\n", s.ne_guard_count);
//   printf("sw: %d\n", s.sw_guard_count);
//   printf("se: %d\n", s.se_guard_count);
//   */
//
//   memset(KILLER_MOVES, 0, MAX_DEPTH * sizeof(move) * 2);
//   memset(PREV_PV, 0, MAX_DEPTH * sizeof(move));
//   for (int i = 0; i < depth; i++) {
//     negamax_ab_sorted_pv(
//         (move){0, 0},
//         b,
//         r,
//         is_black,
//         i,
//         0,
//         MIN_SCORE,
//         MAX_SCORE,
//         &tally,
//         true,
//         s,
//         false,
//         ai_settings);
//     for (int j = 0; j < PV_LENGTH[0]; j++) {
//       PREV_PV[j] = PV_TABLE[0][j];
//     }
//     PREV_PV_LENGTH = PV_LENGTH[0];
//   }
//   auto res = negamax_ab_sorted_pv(
//       (move){0, 0},
//       b,
//       r,
//       is_black,
//       depth,
//       0,
//       MIN_SCORE,
//       MAX_SCORE,
//       &tally,
//       true,
//       s,
//       false,
//       ai_settings);
//
//   move result_move = PV_TABLE[0][0];
//   board result_board = PV_TABLE_BOARDS[0][0];
//   team_repetitions new_r = update_team_repetitions(r, result_move, is_black);
//   // printf("tally: %d\n", tally);
//   return {result_move, result_board, res, new_r};
// }

inline void update_pv(pv *pv_data, int ply, move m) {
  pv_data->pv_table[ply][ply] = m;

  // copy up moves discovered at lower depths
  for (int next_ply = ply + 1; next_ply < pv_data->pv_length[ply + 1];
       next_ply++) {
    pv_data->pv_table[ply][next_ply] = pv_data->pv_table[ply + 1][next_ply];
  }

  // adjust pv length
  pv_data->pv_length[ply] = pv_data->pv_length[ply + 1];
}

pv_line create_pv_line(pv *pv_data, bool is_black_turn, i32 result) {
  move *moves = malloc(sizeof(move) * pv_data->pv_length[0]);
  memcpy(moves, pv_data->pv_table[0], sizeof(move) * pv_data->pv_length[0]);
  return (pv_line){is_black_turn, moves, pv_data->pv_length[0], result};
}

void destroy_pv_line(pv_line *line) { free(line->moves); }
/*
  Potential areas of improvement:
  - short circuit in quiet positions earlier
  - identify when there are 2-move escape branches that can't be block and bail
  out early.
*/
i32 quiesce_black(
    pv *pv_data,
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    int quiesce_depth,
    i32 alpha,
    i32 beta,
    stats *statistics) {

  // Increment black position evaluation count
  statistics->quiescence_positions_black++;

  pv_data->pv_length[ply] = ply;

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (white_victory(&b)) {
    return LOSS_SCORE(ply);
  }

  // If we can't find a quiet position in 6 moves we consider the line unstable
  // and score it as a draw for each player, as we can't tell who comes out on
  // top.
  if (quiesce_depth <= 0) {
    statistics->quiescence_limit_reached++;
    return 0;
  }

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // We return MAX_SCORE for draws rather than 0 because our static eval isn't
    // well-calibrated around zero, making the engine too eager to pursue draws.
    // MAX_SCORE here means the side to move "accepts" the draw, but the
    // opponent (who made the move leading here) sees -MAX_SCORE via negation,
    // so they avoid it. This symmetrically discourages both sides from creating
    // draw opportunities, so draws only occur when forced.
    statistics->repeat_moves_encountered++;
    return MAX_SCORE;
  }

  // Start with a static eval as best_value
  i32 best_value = black_score(w, &s, &b);

  // TODO: ensure that I follow a king-escape-in-progress to its conclusion.
  // When I generate a layer of king escape paths and then generate moves to
  // block them, I need to ensure that return a sensible score when there are no
  // blocking moves to generate. In default quiescence if I can't generate any
  // moves then I return a static evaluation, which here is not accurate; the
  // actual consequence will likely be a king escape. So if I generate king
  // escape paths and can't generate any moves to block it, I should score it
  // very poorly (maybe even as a loss). The only problem is that in the
  // instance where I generate a two-move king-escape path, there may be
  // instances where a black piece can't block it in one move, but can in two,
  // and I won't generate that two-move response. The solution would be, in the
  // king escape path layer, instead of only generating the exact path, also
  // smearing the final move vector in both perpindicular directions so that we
  // also generate any move which can obstruct the final move in two moves.
  // Another option would be to preserve the existing 2-move-escape blocking
  // function, and only generate 2-move _blocks_ when I can't find any 1-move
  // blocks.

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  // Generate move layers once for reuse throughout function
  // Defer generation until needed
  bool layers_generated = false;
  move_layers layers = {0};

  // ---------------------------------------------------------------------------
  // escape-in-1 blocking dests
  // TODO: if there are escape paths we should set

  {
    layer corner_paths = EMPTY_LAYER;
    layer corner_paths_r = EMPTY_LAYER;

    corner_paths_1(
        LAYER_OR(b.black, b.white),
        LAYER_OR(b.black_r, b.white_r),
        king_rank,
        king_file,
        &corner_paths,
        &corner_paths_r);

    if (NOT_EMPTY(corner_paths)) {
      // If there are escape paths then we set the best value to a losing score.
      // We can only raise the score by finding a move that does not result in
      // an escape.
      best_value = MIN_SCORE;
      // delete_position(positions, position_index);
      // return MIN_SCORE + 10000;

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsometimes-uninitialized"
#endif
      if (!layers_generated) {
        layers = generate_black_move_layers(&b);
        layers_generated = true;
      }
#ifdef __clang__
#pragma clang diagnostic pop
#endif
      move_layers escape1_layers = layers;
      mask_move_layers(corner_paths, corner_paths_r, &escape1_layers);

      move ms[100];
      int total = 0;
      moves_from_layers(
          &escape1_layers,
          b.black,
          b.black_r,
          ms,
          NULL,
          NULL,
          &total);

      // Subtract these moves from the main layers
      subtract_move_layers(&layers, &escape1_layers);

      // If black can't block an escape in one then they've lost.
      if (!total) {
        delete_position(positions, position_index);
        return INEVITABLE_LOSS_SCORE(ply);
      }

      // hacky bounds check
      assert(total < 100);

      // iterate
      for (int i = 0; i < total; i++) {
        u8 orig = ms[i].orig;
        u8 dest = ms[i].dest;

#ifndef NDEBUG
        // Validate move before applying
        struct move m = ms[i];
        move_error error = validate_move(b, m, true);
        if (error != move_error_no_error) {
          print_board(b);
          print_move(m.orig, m.dest);
          printf(
              "invalid escape-in-1 blocking move generated in quiesce_black at "
              "line %d with code: %d\n",
              __LINE__,
              error);
          exit(1);
        }
#endif

        board new_b = apply_black_move_m(b, orig, dest);
#ifndef NDEBUG
        if (!validate_board_state(new_b)) {
          printf(
              "Board corrupted after apply_black_move in quiesce_black at "
              "line %d, ply %d\n",
              __LINE__,
              ply);
          print_move(orig, dest);
          exit(1);
        }
#endif
        u64 new_position_hash = next_hash_black(position_hash, orig, dest);
        layer captures =
            apply_captures_z_black(&new_b, &new_position_hash, dest);
#ifndef NDEBUG
        if (!validate_board_state(new_b)) {
          printf(
              "Board corrupted after apply_captures_z_black in quiesce_black "
              "at line %d, ply %d\n",
              __LINE__,
              ply);
          print_move(orig, dest);
          exit(1);
        }
#endif
        score_state new_score_state = update_score_state_black_move_and_capture(
            w,
            &s,
            orig,
            dest,

            captures);

        i32 score = -quiesce_white(
            pv_data,
            positions,
            w,
            new_score_state,
            new_b,
            new_position_hash,
            ply + 1,
            quiesce_depth - 1,
            -beta,
            -alpha,
            statistics);

        if (score >= beta) {
          statistics->quiencence_beta_cutoff_black++;
          delete_position(positions, position_index);
          return score;
        }
        if (score > best_value) {
          best_value = score;
          update_pv(pv_data, ply, ms[i]);
        }
        if (score > alpha) {
          alpha = score;
        }
      }

      delete_position(positions, position_index);
      return best_value;
    }
  }

  // ---------------------------------------------------------------------------
  // escape-in-2 blocking dests
  // TODO: if there are escape paths we should set
  {
    layer corner_paths = EMPTY_LAYER;
    layer corner_paths_r = EMPTY_LAYER;

    corner_paths_2_new(
        LAYER_OR(b.black, b.white),
        LAYER_OR(b.black_r, b.white_r),
        king_rank,
        king_file,
        king_pos,
        &corner_paths,
        &corner_paths_r);

    if (NOT_EMPTY(corner_paths)) {
      // If there are escape paths then we set the best value to a losing score.
      // We can only raise the score by finding a move that does not result in
      // an escape.
      // best_value = MIN_SCORE;
      delete_position(positions, position_index);
      return best_value - 1000000;
    }

    if (!layers_generated) {
      layers = generate_black_move_layers(&b);
      layers_generated = true;
    }
    move_layers escape2_layers = layers;
    mask_move_layers(corner_paths, corner_paths_r, &escape2_layers);

    move ms[100];
    int total = 0;
    moves_from_layers(
        &escape2_layers,
        b.black,
        b.black_r,
        ms,
        NULL,
        NULL,
        &total);

    // Subtract these moves from the main layers
    subtract_move_layers(&layers, &escape2_layers);

    // TODO: if total is 0 then ensure there are followup moves otherwise losing
    // score

    // hacky bounds check
    assert(total < 100);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, true);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid escape-in-2 blocking move generated in quiesce_black at "
            "line %d with code: %d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_black_move_m(b, orig, dest);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      score_state new_score_state = update_score_state_black_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
      i32 score = -quiesce_white(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          quiesce_depth - 1,
          -beta,
          -alpha,
          statistics);

      if (score >= beta) {
        statistics->quiencence_beta_cutoff_black++;
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(pv_data, ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }

    // If we're still facing a king escape then we do a broader search to try to
    // find more complex sequences that might avoid the escape.
    // Potential optimization:
    // - skip unless there is exactly one open branch. I can't block more than
    // one branch with a 2-move sequence.
    // - limit move generation to captures and destinations from which we can
    // block the escape stem.
    // TODO: integrate better with the capture generation below.
    if (best_value < (MIN_SCORE + 100)) {
      if (!layers_generated) {
        layers = generate_black_move_layers(&b);
        layers_generated = true;
      }
      move ms[400];
      int total = 0;
      moves_from_layers(&layers, b.black, b.black_r, ms, NULL, NULL, &total);

      // iterate
      for (int i = 0; i < total; i++) {
        u8 orig = ms[i].orig;
        u8 dest = ms[i].dest;

#ifndef NDEBUG
        // Validate move before applying
        struct move m = ms[i];
        move_error error = validate_move(b, m, true);
        if (error != move_error_no_error) {
          print_board(b);
          print_move(m.orig, m.dest);
          printf(
              "invalid broader search move generated in quiesce_black at line "
              "%d with code: %d\n",
              __LINE__,
              error);
          exit(1);
        }
#endif

        board new_b = apply_black_move_m(b, orig, dest);
        u64 new_position_hash = next_hash_black(position_hash, orig, dest);
        layer captures =
            apply_captures_z_black(&new_b, &new_position_hash, dest);
        score_state new_score_state = update_score_state_black_move_and_capture(
            w,
            &s,
            orig,
            dest,
            captures);
        i32 score = -quiesce_white(
            pv_data,
            positions,
            w,
            new_score_state,
            new_b,
            new_position_hash,
            ply + 1,
            quiesce_depth - 1,
            -beta,
            -alpha,
            statistics);

        if (score >= beta) {
          statistics->quiencence_beta_cutoff_black++;
          delete_position(positions, position_index);
          return score;
        }
        if (score > best_value) {
          best_value = score;
          update_pv(pv_data, ply, ms[i]);
        }
        if (score > alpha) {
          alpha = score;
        }
      }
    }
  }

  // ---------------------------------------------------------------------------

  // Stand pat. This is sort of like a null move score.
  //
  // It's important that this evaluation comes _after_ exploration of king
  // blocking moves above. This is because what we're essentially asking here is
  // "does white have access to another line that yields an equal or better
  // score for white than the static evaluation score of this position?" If the
  // answer is yes then way may as well stop now, because when black responds
  // the score for this line will only get worse for white (assuming black is
  // not in zugzwang). The problem with this logic in the context of following
  // king escape sequences to their conclusion is that it wants every white move
  // to result in a better static evaluation (indeed, it was devised in the
  // context of chess quiescence in which material is typically being traded
  // each iteration), but a move which puts the king within 1 or 2 moves of
  // escape does not result in a better _static_ score; the move only pays off
  // score-wise if the king manages to escape as a result of subsequent
  // moves. So if this function is evaluating a position in which the king has
  // advanced towards an escape, but we do stand-pat at the beginning, we'll
  // more than likely bail out before we see if subsequent moves would result in
  // an escape. By delaying stand-pat evaluation to after black responds to any
  // escape threats, we ensure that escape-in-progress lines are explored.

  if (best_value >= beta) {
    delete_position(positions, position_index);
    return best_value;
  }
  if (best_value > alpha) {
    alpha = best_value;
  }

  // ---------------------------------------------------------------------------
  // pawn capture moves

  {
    // generate capture moves for pawns
    layer capture_dests = black_capture_destinations(&b);
    layer capture_dests_r = black_capture_destinations_r(&b);

    if (!layers_generated) {
      layers = generate_black_move_layers(&b);
      layers_generated = true;
    }
    move_layers capture_layers = layers;
    mask_move_layers(capture_dests, capture_dests_r, &capture_layers);

    move ms[100];
    int total = 0;
    moves_from_layers(
        &capture_layers,
        b.black,
        b.black_r,
        ms,
        NULL,
        NULL,
        &total);

    // Subtract these moves from the main layers
    subtract_move_layers(&layers, &capture_layers);

    // hacky bounds check
    assert(total < 100);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, true);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid pawn capture move generated in quiesce_black at line %d "
            "with code: %d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_black_move_m(b, orig, dest);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      score_state new_score_state = update_score_state_black_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
      i32 score = -quiesce_white(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          quiesce_depth - 1,
          -beta,
          -alpha,
          statistics);

      if (score >= beta) {
        statistics->quiencence_beta_cutoff_black++;
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(pv_data, ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }
  }

  // ---------------------------------------------------------------------------

  // remove the position from the set as we exit
  delete_position(positions, position_index);
  return best_value;
}

/*
  Potential areas of improvement:
  - short circuit in quiet positions earlier
*/
i32 quiesce_white(
    pv *pv_data,
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    int quiesce_depth,
    i32 alpha,
    i32 beta,
    stats *statistics) {

  // Increment white position evaluation count
  statistics->quiescence_positions_white++;

  pv_data->pv_length[ply] = ply;

  // We only need to check for a black_victory because the previous move will
  // have been black.
  if (black_victory(&b)) {
    return LOSS_SCORE(ply);
  }

  // If we can't find a quiet position in 6 moves we consider the line
  // unstable and score it as a draw, as we can't tell who comes out on top.
  if (quiesce_depth <= 0) {
    statistics->quiescence_limit_reached++;
    return 0;
  }

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // Return MAX_SCORE for draws to symmetrically discourage both sides from
    // creating draw opportunities. See comment in quiesce_black for details.
    statistics->repeat_moves_encountered++;
    return MAX_SCORE;
  }

  // white to move, so we score for white
  i32 best_value = white_score(w, &s, &b);

  // ---------------------------------------------------------------------------
  // Stand pat
  if (best_value >= beta) {
    delete_position(positions, position_index);
    return best_value;
  }
  if (best_value > alpha) {
    alpha = best_value;
  }
  // ---------------------------------------------------------------------------

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  // Generate move layers once for reuse throughout function
  move_layers layers = generate_white_move_layers(&b);

  // ---------------------------------------------------------------------------
  // king escape moves in 2

  // generate king escapes
  int dests[8];
  int corner_move_count = 0;
  bool single_move_escape = corner_moves_2(
      LAYER_OR(b.black, b.white),
      LAYER_OR(b.black_r, b.white_r),
      king_rank,
      king_file,
      dests,
      &corner_move_count);

  if (single_move_escape) {
    delete_position(positions, position_index);
    return INEVITABLE_VICTORY_SCORE(ply);
  } else if (corner_move_count > 0) {
    delete_position(positions, position_index);
    return best_value + (1000000 * corner_move_count);
  }

  // iterate
  for (int i = 0; i < corner_move_count; i++) {
    u8 orig = king_pos;
    u8 dest = dests[i];
    move m = {orig, dest};

#ifndef NDEBUG
    // Validate move before applying
    move_error error = validate_move(b, m, false);
    if (error != move_error_no_error) {
      print_board(b);
      print_move(m.orig, m.dest);
      printf(
          "invalid king escape move generated in quiesce_white at line %d "
          "with code: %d\n",
          __LINE__,
          error);
      exit(1);
    }
#endif

    board new_b = b;
    CLEAR_INDEX(new_b.king, orig);
    CLEAR_INDEX(new_b.king_r, rotate_right[orig]);
    SET_INDEX(new_b.king, dest);
    SET_INDEX(new_b.king_r, rotate_right[dest]);

#ifndef NDEBUG
    if (!validate_board_state(new_b)) {
      printf(
          "Board corrupted after king move in quiesce_white at line %d, ply "
          "%d\n",
          __LINE__,
          ply);
      print_move(orig, dest);
      exit(1);
    }
#endif

    u64 new_position_hash = next_hash_king(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
#ifndef NDEBUG
    if (!validate_board_state(new_b)) {
      printf(
          "Board corrupted after apply_captures_z_white (king) in "
          "quiesce_white at line %d, ply %d\n",
          __LINE__,
          ply);
      print_move(orig, dest);
      exit(1);
    }
#endif
    score_state new_score_state =
        update_score_state_king_move_and_capture(w, &s, orig, dest, captures);
    i32 score = -quiesce_black(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        quiesce_depth - 1,
        -beta,
        -alpha,
        statistics);

    if (score >= beta) {
      statistics->quiencence_beta_cutoff_white++;
      delete_position(positions, position_index);
      return score;
    }
    if (score > best_value) {
      best_value = score;
      update_pv(pv_data, ply, m);
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // If we have any corner moves but we've not managed to secure an escape,
  // generate all possible moves to see if there's a more complex sequence
  // that can capitalize on the open escape path. Could be optimized similarly
  // to the fallback all-move generation done to prevent 2-move escapes in black
  // quiescence.
  if (corner_move_count && best_value < (MAX_SCORE - 100)) {
    move ms[400];
    int total = 0;
    moves_from_layers(&layers, b.white, b.white_r, ms, NULL, NULL, &total);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, false);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid fallback move generated in quiesce_white at line %d with "
            "code: %d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_white_move_m(b, orig, dest);
#ifndef NDEBUG
      if (!validate_board_state(new_b)) {
        printf(
            "Board corrupted after apply_white_move (fallback) in "
            "quiesce_white at line %d, ply %d\n",
            __LINE__,
            ply);
        print_move(orig, dest);
        exit(1);
      }
#endif
      u64 new_position_hash = next_hash_white(position_hash, orig, dest);
      layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
#ifndef NDEBUG
      if (!validate_board_state(new_b)) {
        printf(
            "Board corrupted after apply_captures_z_white (fallback) in "
            "quiesce_white at line %d, ply %d\n",
            __LINE__,
            ply);
        print_move(orig, dest);
        exit(1);
      }
#endif
      score_state new_score_state = update_score_state_white_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
      i32 score = -quiesce_black(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          quiesce_depth - 1,
          -beta,
          -alpha,
          statistics);

      if (score >= beta) {
        statistics->quiencence_beta_cutoff_white++;
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(pv_data, ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }
  }

  // ---------------------------------------------------------------------------
  // generate capture moves
  // TODO: remove throne from dests before finding pawn captures
  // actually maybe not necessary, it's covered by the blockers in moves_to
  layer capture_dests = white_capture_destinations(&b);
  layer capture_dests_r = white_capture_destinations_r(&b);

  // ---------------------------------------------------------------------------
  // king capture moves
  {

    // generate capture moves for king
    move ms[100];
    int total = 0;
    moves_to_king_impl(
        LAYER_AND(capture_dests, LAYER_NOT(corners)),
        LAYER_AND(capture_dests_r, LAYER_NOT(corners)),
        b.king,
        b.king_r,
        king_board_occ(b),
        king_board_occ_r(b),
        ms,
        NULL,
        NULL,
        &total);

    // hacky bounds check
    assert(total < 100);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, false);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid king capture move generated in quiesce_white at line %d "
            "with code: %d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_king_move_m(b, orig, dest);
#ifndef NDEBUG
      if (!validate_board_state(new_b)) {
        printf(
            "Board corrupted after apply_king_move (capture) in quiesce_white "
            "at line %d, ply %d\n",
            __LINE__,
            ply);
        print_move(orig, dest);
        exit(1);
      }
#endif
      u64 new_position_hash = next_hash_king(position_hash, orig, dest);
      layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
#ifndef NDEBUG
      if (!validate_board_state(new_b)) {
        printf(
            "Board corrupted after apply_captures_z_white (king capture) in "
            "quiesce_white at line %d, ply %d\n",
            __LINE__,
            ply);
        print_move(orig, dest);
        exit(1);
      }
#endif
      score_state new_score_state =
          update_score_state_king_move_and_capture(w, &s, orig, dest, captures);

      i32 score = -quiesce_black(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          quiesce_depth - 1,
          -beta,
          -alpha,
          statistics);

      if (score >= beta) {
        statistics->quiencence_beta_cutoff_white++;
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(pv_data, ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }
  }
  // ---------------------------------------------------------------------------
  // pawn capture moves
  {
    move ms[100];
    int total = 0;

    move_layers capture_layers = layers;
    mask_move_layers(capture_dests, capture_dests_r, &capture_layers);

    total = 0;
    moves_from_layers(
        &capture_layers,
        b.white,
        b.white_r,
        ms,
        NULL,
        NULL,
        &total);

    // hacky bounds check
    assert(total < 100);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, false);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid pawn capture move generated in quiesce_white at line %d "
            "with code: %d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_white_move_m(b, orig, dest);
#ifndef NDEBUG
      if (!validate_board_state(new_b)) {
        printf(
            "Board corrupted after apply_white_move (pawn capture) in "
            "quiesce_white at line %d, ply %d\n",
            __LINE__,
            ply);
        print_board(b);
        print_move(orig, dest);
        exit(1);
      }
#endif
      u64 new_position_hash = next_hash_white(position_hash, orig, dest);
      layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
#ifndef NDEBUG
      if (!validate_board_state(new_b)) {
        printf(
            "Board corrupted after apply_captures_z_white (pawn capture) in "
            "quiesce_white at line %d, ply %d\n",
            __LINE__,
            ply);
        print_move(orig, dest);
        exit(1);
      }
#endif
      score_state new_score_state = update_score_state_white_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
      i32 score = -quiesce_black(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          quiesce_depth - 1,
          -beta,
          -alpha,
          statistics);

      if (score >= beta) {
        statistics->quiencence_beta_cutoff_white++;
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(pv_data, ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }
  }
  // ---------------------------------------------------------------------------

  delete_position(positions, position_index);
  return best_value;
}

pv_line quiesce_white_runner(board b) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, false);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  stats statistics = {0};
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = quiesce_white(
      &pv_data,
      positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      7,
      alpha,
      beta,
      &statistics);
  destroy_position_set(positions);
  return create_pv_line(&pv_data, false, result);
}

pv_line quiesce_black_runner(board b) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, true);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  stats statistics = {0};
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = quiesce_black(
      &pv_data,
      positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      7,
      alpha,
      beta,
      &statistics);
  destroy_position_set(positions);
  return create_pv_line(&pv_data, true, result);
}

search_result search_runner_generic(
    board b,
    int depth,
    _Atomic bool *should_stop,
    search_func search_fn,
    bool is_black,
    transposition_table *tt) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, is_black);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  stats statistics = {0};

  // Run the search
  i32 result = search_fn(
      &pv_data,
      positions,
      tt,
      &weights,
      s,
      b,
      position_hash,
      ply,
      depth,
      alpha,
      beta,
      &statistics,
      should_stop,
      true);

  destroy_position_set(positions);
  return (search_result){create_pv_line(&pv_data, is_black, result),
                         statistics};
}

search_result search_black_runner(
    board b,
    int depth,
    _Atomic bool *should_stop,
    transposition_table *tt) {
  return search_runner_generic(b, depth, should_stop, search_black, true, tt);
}

search_result search_white_runner(
    board b,
    int depth,
    _Atomic bool *should_stop,
    transposition_table *tt) {
  return search_runner_generic(b, depth, should_stop, search_white, false, tt);
}

search_result search_with_timeout(
    search_runner_func runner,
    board b,
    int depth,
    int time_limit,
    transposition_table *tt) {
  _Atomic bool should_stop = false;
  _Atomic bool search_finished = false;

  // Set up timer thread if time_limit > 0
  pthread_t timer_thread_id;
  int thread_result = -1;
  timer_data timer_info;

  if (time_limit > 0) {
    timer_info = (timer_data){.should_stop = &should_stop,
                              .search_finished = &search_finished,
                              .time_limit_ms = time_limit};

    thread_result =
        pthread_create(&timer_thread_id, NULL, timer_thread, &timer_info);

    if (thread_result != 0) {
      // Handle thread creation error - continue without timer
      fprintf(stderr, "Warning: Failed to create timer thread\n");
    }
  }

  // Run the search
  search_result result = runner(b, depth, &should_stop, tt);

  // Signal that search is finished
  atomic_store(&search_finished, true);

  // Wait for timer thread to finish (should exit quickly now)
  if (time_limit > 0 && thread_result == 0) {
    pthread_join(timer_thread_id, NULL);
  }

  return result;
}

search_result search_white_with_timeout(
    board b,
    int depth,
    int time_limit,
    transposition_table *tt) {
  return search_with_timeout(search_white_runner, b, depth, time_limit, tt);
}

search_result search_black_with_timeout(
    board b,
    int depth,
    int time_limit,
    transposition_table *tt) {
  return search_with_timeout(search_black_runner, b, depth, time_limit, tt);
}

// Build the search's initial position set: the subset of the given
// history hashes that appear two or more times. The search's
// repetition handling functions such that a position encountered
// twice is treated as an illegal reptition; this is for efficiency in
// the search algorithm. If we seeded the position set with positions
// encountered once in move history we'd erroneously error on/avoid
// those positions: by providing only positions encountered twice we
// correctly avoid threefold repetition only.
static position_set *
create_position_set_with_duplicates(u64 *zobrist_hashes, int hash_count) {
  if (hash_count <= 0) {
    return create_position_set(100);
  }

  position_set *first_ps = create_position_set(hash_count + 100);
  position_set *duplicates = create_position_set(hash_count + 100);
  for (int i = 0; i < hash_count; i++) {
    int deletion_index;
    if (insert_position(first_ps, zobrist_hashes[i], &deletion_index) != 0) {
      insert_position(duplicates, zobrist_hashes[i], &deletion_index);
    }
  }
  destroy_position_set(first_ps);

  return duplicates;
}

#ifndef NDEBUG
// Helper function to validate a sequence of moves in a PV by applying them
// sequentially and checking each move against the updated board state
static void validate_pv_sequence(
    board initial_board,
    move *moves,
    int length,
    bool initial_turn_is_black,
    int iteration) {
  board current_board = initial_board;
  bool is_black_turn = initial_turn_is_black;

  for (int i = 0; i < length; i++) {
    struct move m = moves[i];
    move_error error = validate_move(current_board, m, is_black_turn);
    if (error != move_error_no_error) {
      print_board(current_board);
      print_move(m.orig, m.dest);
      printf(
          "invalid move at index %d in pv after iteration %d with code: %d\n",
          i,
          iteration,
          error);
      exit(1);
    }

    // Apply the move to get the next board state
    // For white, we need to check if it's a king move
    if (is_black_turn) {
      current_board = apply_black_move_m(current_board, m.orig, m.dest);
      // We don't need the zobrist hash for validation, so we can use a dummy
      u64 dummy_hash = 0;
      apply_captures_z_black(&current_board, &dummy_hash, m.dest);
    } else {
      // Check if this is a king move
      int king_pos = LOWEST_INDEX(current_board.king);
      if (m.orig == king_pos) {
        current_board = apply_king_move_m(current_board, m.orig, m.dest);
      } else {
        current_board = apply_white_move_m(current_board, m.orig, m.dest);
      }
      u64 dummy_hash = 0;
      apply_captures_z_white(&current_board, &dummy_hash, m.dest);
    }

    // Alternate turn
    is_black_turn = !is_black_turn;
  }
}
#endif

search_result search_runner_iterative_generic(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    search_func search_fn,
    bool is_black,
    u64 *zobrist_hashes,
    int hash_count,
    transposition_table *tt) {
  pv pv_data = {0};

  u64 position_hash = hash_for_board(b, is_black);
  // Exclude the last hash — it represents the current position, which
  // the search itself inserts at ply 0.
  int history_count = hash_count > 0 ? hash_count - 1 : 0;
  position_set *positions =
      create_position_set_with_duplicates(zobrist_hashes, history_count);

  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  stats statistics = {0};

  search_result result = {0};
  _Atomic bool dummy_stop = false;

  // Always do depth 1 first to guarantee we have a result
  i32 search_result_score = search_fn(
      &pv_data,
      positions,
      tt,
      &weights,
      s,
      b,
      position_hash,
      ply,
      1, // depth = 1
      alpha,
      beta,
      &statistics,
      &dummy_stop, // Use dummy so depth-1 always completes
      true);

  result.pv = create_pv_line(&pv_data, is_black, search_result_score);
  result.statistics = statistics;

#ifndef NDEBUG
  // Check if we got an empty PV after depth 1 search
  if (pv_data.pv_length[0] == 0) {
    printf("ERROR: Empty PV after depth 1 search!\n");
    printf("is_black: %s\n", is_black ? "true" : "false");
    printf("search_result_score: %d\n", search_result_score);
    printf("Board state:\n");
    print_board(b);

    // Debug king position
    int king_pos = LOWEST_INDEX(b.king);
    printf(
        "King position index: %d (rank %d, file %d)\n",
        king_pos,
        RANK(king_pos),
        FILE(king_pos));
    printf("king._[0]: %lu\n", b.king._[0]);
    printf("king._[1]: %lu\n", b.king._[1]);
    printf("king_effectively_escaped: %d\n", king_effectively_escaped(&b));
    printf("white_victory: %d\n", white_victory(&b));
    printf("exit_fort: %d\n", exit_fort(&b));
  }
#endif

  // If max_depth is 1, we're done
  if (max_depth == 1) {
    destroy_position_set(positions);
    return result;
  }

#ifndef NDEBUG
  validate_pv_sequence(
      b,
      pv_data.pv_table[0],
      pv_data.pv_length[0],
      is_black,
      1);
#endif

  // Continue with iterative deepening from depth 2 to max_depth
  for (int depth = 2; depth <= max_depth; depth++) {
    // Check if we should stop before starting this iteration
    if (atomic_load(should_stop)) {
      break;
    }

    // Increment TT generation so entries from previous iterations are
    // considered older and can be preferentially replaced
    tt_new_generation(tt);

    // Reset position set for each iteration (TT persists across iterations)
    destroy_position_set(positions);
    positions =
        create_position_set_with_duplicates(zobrist_hashes, history_count);

#ifndef NDEBUG
    validate_pv_sequence(
        b,
        pv_data.pv_table[0],
        pv_data.pv_length[0],
        is_black,
        depth);
#endif

    search_result_score = search_fn(
        &pv_data,
        positions,
        tt,
        &weights,
        s,
        b,
        position_hash,
        ply,
        depth,
        alpha,
        beta,
        &statistics,
        should_stop,
        true);

    // Only update result if iteration completed (not stopped)
    if (!atomic_load(should_stop)) {
      destroy_pv_line(&result.pv);
      result.pv = create_pv_line(&pv_data, is_black, search_result_score);
      result.statistics = statistics;
    } else {
      // If stopped, break without updating result
      break;
    }
  }

  destroy_position_set(positions);
  return result;
}

search_result search_black_runner_iterative(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    transposition_table *tt) {
  return search_runner_iterative_generic(
      b,
      max_depth,
      should_stop,
      search_black,
      true,
      NULL,
      0,
      tt);
}

search_result search_white_runner_iterative(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    transposition_table *tt) {
  return search_runner_iterative_generic(
      b,
      max_depth,
      should_stop,
      search_white,
      false,
      NULL,
      0,
      tt);
}

search_result search_runner_iterative_trusted(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    bool is_black_turn,
    u64 *zobrist_hashes,
    int hash_count,
    transposition_table *tt) {
  search_func search_fn = is_black_turn ? search_black : search_white;
  return search_runner_iterative_generic(
      b,
      max_depth,
      should_stop,
      search_fn,
      is_black_turn,
      zobrist_hashes,
      hash_count,
      tt);
}

search_result search_white_with_timeout_iterative(
    board b,
    int max_depth,
    int time_limit,
    transposition_table *tt) {
  return search_with_timeout(
      search_white_runner_iterative,
      b,
      max_depth,
      time_limit,
      tt);
}

search_result search_black_with_timeout_iterative(
    board b,
    int max_depth,
    int time_limit,
    transposition_table *tt) {
  return search_with_timeout(
      search_black_runner_iterative,
      b,
      max_depth,
      time_limit,
      tt);
}

/* We try moves in this order:
- TT move (hash move from transposition table)
- capture moves.
- remaining quiet moves
*/
i32 search_black(
    pv *pv_data,
    position_set *positions,
    transposition_table *tt,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,   // distance from the root
    int depth, // depth remaining
    i32 alpha,
    i32 beta,
    stats *statistics,
    _Atomic bool *should_stop,
    bool allow_null_move) {

  if (atomic_load(should_stop)) {
    return alpha;
  }

  // Increment black position evaluation count
  statistics->search_positions_black++;

  pv_data->pv_length[ply] = ply;

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (white_victory(&b)) {
    return LOSS_SCORE(ply);
  }

#ifndef NDEBUG
  // Debug: Check if we should have detected a victory
  if (king_effectively_escaped(&b)) {
    printf(
        "ERROR: King effectively escaped but white_victory returned false!\n");
    printf("king_effectively_escaped: %d\n", king_effectively_escaped(&b));
    printf("exit_fort: %d\n", exit_fort(&b));
    printf("white_victory: %d\n", white_victory(&b));
    print_board(b);
    exit(1);
  }
#endif

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // Return MAX_SCORE for draws to symmetrically discourage both sides from
    // creating draw opportunities. See comment in quiesce_black for details.
    statistics->repeat_moves_encountered++;
    return MAX_SCORE;
  }

  // TT probe
  move tt_move;
  i32 tt_score;
  bool tt_hit =
      tt_probe(tt, position_hash, depth, alpha, beta, &tt_score, &tt_move, ply);
  if (tt_hit) {
    statistics->tt_hits++;
    statistics->tt_cutoffs++;
    delete_position(positions, position_index);
    return tt_score;
  }
  if (tt_move.orig != 0 || tt_move.dest != 0) {
    statistics->tt_hits++;
  }

  if (depth <= 0) {
    delete_position(positions, position_index);
    return quiesce_black(
        pv_data,
        positions,
        w,
        s,
        b,
        position_hash,
        ply,
        7,
        alpha,
        beta,
        statistics);
  }

  // Null move pruning
  // If we give the opponent a free move and they still can't beat beta,
  // the position is so good we can prune.
  if (allow_null_move && depth >= 3 && ply > 0) {
    statistics->null_move_attempts++;
    int R = 2 + depth / 6;
    if (R > depth - 1)
      R = depth - 1;

    // Toggle side-to-move in hash (skip black's turn)
    u64 null_hash = position_hash ^ is_black_hash;

    i32 null_score = -search_white(
        pv_data,
        positions,
        tt,
        w,
        s,
        b,
        null_hash,
        ply + 1,
        depth - 1 - R,
        -beta,
        -beta + 1,
        statistics,
        should_stop,
        false);

    if (null_score >= beta) {
      statistics->null_move_cutoffs++;
      delete_position(positions, position_index);
      return beta;
    }
  }

  i32 best_value = MIN_SCORE;
  i32 original_alpha = alpha;
  move best_move = {0, 0};

  // TT move (best move from previous search/iteration)
  if (tt_move.orig != 0 || tt_move.dest != 0) {
#ifndef NDEBUG
    move_error error = validate_move(b, tt_move, true);
    if (error != move_error_no_error) {
      print_move(tt_move.orig, tt_move.dest);
      printf(
          "invalid TT move pulled at ply %d, line %d, with code: %d",
          ply,
          __LINE__,
          error);
      exit(1);
    }
#endif
    u8 orig = tt_move.orig;
    u8 dest = tt_move.dest;

    board new_b = apply_black_move_m(b, orig, dest);
    u64 new_position_hash = next_hash_black(position_hash, orig, dest);
    layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_black_move_and_capture(w, &s, orig, dest, captures);

    i32 score = -search_white(
        pv_data,
        positions,
        tt,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        depth - 1,
        -beta,
        -alpha,
        statistics,
        should_stop,
        true);

    if (score > best_value) {
      best_value = score;
      best_move = tt_move;
    }
    if (score > alpha) {
      alpha = score;
      update_pv(pv_data, ply, tt_move);
    }
    if (score >= beta) {
      statistics->search_beta_cutoff_black++;
      tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, tt_move, ply);
      delete_position(positions, position_index);
      return score;
    }
  }

  // ---------------------------------------------------------------------------
  // Destinations

  move_layers layers = generate_black_move_layers(&b);

  // ---------------------------------------------------------------------------
  // capture moves

  // generate capture moves for pawns
  layer capture_dests = black_capture_destinations(&b);
  layer capture_dests_r = black_capture_destinations_r(&b);
  move_layers capture_layers = layers;
  mask_move_layers(capture_dests, capture_dests_r, &capture_layers);

  {
    move ms[400];
    int total = 0;
    moves_from_layers(
        &capture_layers,
        b.black,
        b.black_r,
        ms,
        NULL,
        NULL,
        &total);

    // hacky bounds check
    assert(total < 400);

    // iterate
    for (int i = 0; i < total; i++) {
      // Skip TT move (already searched)
      if (ms[i].orig == tt_move.orig && ms[i].dest == tt_move.dest)
        continue;

      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, true);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid capture move generated in search_black at line %d with "
            "code: "
            "%d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_black_move_m(b, orig, dest);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      score_state new_score_state = update_score_state_black_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
      i32 score = -search_white(
          pv_data,
          positions,
          tt,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          depth - 1,
          -beta,
          -alpha,
          statistics,
          should_stop,
          true);

      if (score >= beta) {
        statistics->search_beta_cutoff_black++;
        tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, ms[i], ply);
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        best_move = ms[i];
      }
      if (score > alpha) {
        alpha = score;
        update_pv(pv_data, ply, ms[i]);
      }
    }
  }

  // clear the capture destinations for the remaining destinations

  subtract_move_layers(&layers, &capture_layers);

  // ---------------------------------------------------------------------------
  // Remaining

  move ms[400];
  int total = 0;
  moves_from_layers(&layers, b.black, b.black_r, ms, NULL, NULL, &total);

  // hacky bounds check
  assert(total < 400);

  // Score quiet moves by PST delta for move ordering
  i32 move_scores[400];
  for (int i = 0; i < total; i++) {
    move_scores[i] =
        w->psts.black_pst._[ms[i].dest] - w->psts.black_pst._[ms[i].orig];
  }

  // iterate
  for (int i = 0; i < total; i++) {
    // Skip TT move (already searched)
    if (ms[i].orig == tt_move.orig && ms[i].dest == tt_move.dest)
      continue;

    // Selection sort: find best-scoring remaining move
    int best = i;
    for (int j = i + 1; j < total; j++) {
      if (move_scores[j] > move_scores[best])
        best = j;
    }
    if (best != i) {
      move tmp_m = ms[i];
      ms[i] = ms[best];
      ms[best] = tmp_m;
      i32 tmp_s = move_scores[i];
      move_scores[i] = move_scores[best];
      move_scores[best] = tmp_s;
    }

    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;

#ifndef NDEBUG
    // Validate move before applying
    struct move m = ms[i];
    move_error error = validate_move(b, m, true);
    if (error != move_error_no_error) {
      print_board(b);
      print_move(m.orig, m.dest);
      printf(
          "invalid remaining move generated in search_black at line %d with "
          "code: %d\n",
          __LINE__,
          error);
      exit(1);
    }
#endif

    board new_b = apply_black_move_m(b, orig, dest);
    u64 new_position_hash = next_hash_black(position_hash, orig, dest);
    layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_black_move_and_capture(w, &s, orig, dest, captures);
    i32 score = -search_white(
        pv_data,
        positions,
        tt,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        depth - 1,
        -beta,
        -alpha,
        statistics,
        should_stop,
        true);

    if (score >= beta) {
      statistics->search_beta_cutoff_black++;
      tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, ms[i], ply);
      delete_position(positions, position_index);
      return score;
    }
    if (score > best_value) {
      best_value = score;
      best_move = ms[i];
    }
    if (score > alpha) {
      alpha = score;
      update_pv(pv_data, ply, ms[i]);
    }
  }

  // Store result in TT
  tt_node_type node_type =
      best_value > original_alpha ? TT_EXACT : TT_UPPER_BOUND;
  tt_store(tt, position_hash, best_value, node_type, depth, best_move, ply);

  delete_position(positions, position_index);
  return best_value;
}

i32 search_white(
    pv *pv_data,
    position_set *positions,
    transposition_table *tt,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,   // distance from the root
    int depth, // depth remaining
    i32 alpha,
    i32 beta,
    stats *statistics,
    _Atomic bool *should_stop,
    bool allow_null_move) {

  if (atomic_load(should_stop)) {
    return alpha;
  }

  // Increment white position evaluation count
  statistics->search_positions_white++;

  pv_data->pv_length[ply] = ply;

  // We only need to check for a black_victory because the previous move will
  // have been black.
  if (black_victory(&b)) {
    return LOSS_SCORE(ply);
  }

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // Return MAX_SCORE for draws to symmetrically discourage both sides from
    // creating draw opportunities. See comment in quiesce_black for details.
    statistics->repeat_moves_encountered++;
    return MAX_SCORE;
  }

  // TT probe
  move tt_move;
  i32 tt_score;
  bool tt_hit =
      tt_probe(tt, position_hash, depth, alpha, beta, &tt_score, &tt_move, ply);
  if (tt_hit) {
    statistics->tt_hits++;
    statistics->tt_cutoffs++;
    delete_position(positions, position_index);
    return tt_score;
  }
  if (tt_move.orig != 0 || tt_move.dest != 0) {
    statistics->tt_hits++;
  }

  if (depth <= 0) {
    delete_position(positions, position_index);
    return quiesce_white(
        pv_data,
        positions,
        w,
        s,
        b,
        position_hash,
        ply,
        7,
        alpha,
        beta,
        statistics);
  }

  // Null move pruning
  if (allow_null_move && depth >= 3 && ply > 0) {
    statistics->null_move_attempts++;
    int R = 2 + depth / 6;
    if (R > depth - 1)
      R = depth - 1;

    u64 null_hash = position_hash ^ is_black_hash;

    i32 null_score = -search_black(
        pv_data,
        positions,
        tt,
        w,
        s,
        b,
        null_hash,
        ply + 1,
        depth - 1 - R,
        -beta,
        -beta + 1,
        statistics,
        should_stop,
        false);

    if (null_score >= beta) {
      statistics->null_move_cutoffs++;
      delete_position(positions, position_index);
      return beta;
    }
  }

  i32 best_value = MIN_SCORE;
  i32 original_alpha = alpha;
  move best_move = {0, 0};

  int king_pos = LOWEST_INDEX(b.king);

  // TT move (best move from previous search/iteration)
  if (tt_move.orig != 0 || tt_move.dest != 0) {
#ifndef NDEBUG
    move_error error = validate_move(b, tt_move, false);
    if (error != move_error_no_error) {
      print_move(tt_move.orig, tt_move.dest);
      printf(
          "invalid TT move pulled at ply %d, line %d, with code: %d",
          ply,
          __LINE__,
          error);
      exit(1);
    }
#endif
    u8 orig = tt_move.orig;
    u8 dest = tt_move.dest;

    board new_b;
    u64 new_position_hash;
    layer captures;
    score_state new_score_state;

    if (orig == king_pos) {
      new_b = apply_king_move_m(b, orig, dest);
      new_position_hash = next_hash_king(position_hash, orig, dest);
      captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
      new_score_state =
          update_score_state_king_move_and_capture(w, &s, orig, dest, captures);
    } else {
      new_b = apply_white_move_m(b, orig, dest);
      new_position_hash = next_hash_white(position_hash, orig, dest);
      captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
      new_score_state = update_score_state_white_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
    }

    i32 score = -search_black(
        pv_data,
        positions,
        tt,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        depth - 1,
        -beta,
        -alpha,
        statistics,
        should_stop,
        true);

    if (score > best_value) {
      best_value = score;
      best_move = tt_move;
    }
    if (score > alpha) {
      alpha = score;
      update_pv(pv_data, ply, tt_move);
    }
    if (score >= beta) {
      statistics->search_beta_cutoff_white++;
      tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, tt_move, ply);
      delete_position(positions, position_index);
      return score;
    }
  }

  // ---------------------------------------------------------------------------
  // King destinations

  layer all_king_destinations = king_destinations(b);
  layer all_king_destinations_r = king_destinations_r(b);

  {
    move ms[20];
    int total = 0;
    moves_to_king_impl(
        all_king_destinations,
        all_king_destinations_r,
        b.king,
        b.king_r,
        king_board_occ(b),
        king_board_occ_r(b),
        ms,
        NULL,
        NULL,
        &total);

    // hacky bounds check
    assert(total < 21);

    // iterate
    for (int i = 0; i < total; i++) {
      // Skip TT move (already searched)
      if (ms[i].orig == tt_move.orig && ms[i].dest == tt_move.dest)
        continue;

      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, false);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid king move generated in search_white at line %d with code: "
            "%d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_king_move_m(b, orig, dest);
      u64 new_position_hash = next_hash_king(position_hash, orig, dest);
      layer captures = apply_captures_z_king(&new_b, &new_position_hash, dest);
      score_state new_score_state =
          update_score_state_king_move_and_capture(w, &s, orig, dest, captures);
      i32 score = -search_black(
          pv_data,
          positions,
          tt,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          depth - 1,
          -beta,
          -alpha,
          statistics,
          should_stop,
          true);

      if (score >= beta) {
        statistics->search_beta_cutoff_white++;
        tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, ms[i], ply);
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        best_move = ms[i];
      }
      if (score > alpha) {
        alpha = score;
        update_pv(pv_data, ply, ms[i]);
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Pawn moves using moves_from_layers approach

  move_layers layers = generate_white_move_layers(&b);

  // ---------------------------------------------------------------------------
  // capture moves

  // generate capture moves for pawns
  layer capture_dests = white_capture_destinations(&b);
  layer capture_dests_r = white_capture_destinations_r(&b);

  if (NOT_EMPTY(capture_dests)) {
    move_layers capture_layers = layers;
    mask_move_layers(capture_dests, capture_dests_r, &capture_layers);

    move ms[400];
    int total = 0;
    moves_from_layers(
        &capture_layers,
        b.white,
        b.white_r,
        ms,
        NULL,
        NULL,
        &total);

    // hacky bounds check
    assert(total < 400);

    // iterate
    for (int i = 0; i < total; i++) {
      // Skip TT move (already searched)
      if (ms[i].orig == tt_move.orig && ms[i].dest == tt_move.dest)
        continue;

      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;

#ifndef NDEBUG
      // Validate move before applying
      struct move m = ms[i];
      move_error error = validate_move(b, m, false);
      if (error != move_error_no_error) {
        print_board(b);
        print_move(m.orig, m.dest);
        printf(
            "invalid pawn capture move generated in search_white at line %d "
            "with code: %d\n",
            __LINE__,
            error);
        exit(1);
      }
#endif

      board new_b = apply_white_move_m(b, orig, dest);
      u64 new_position_hash = next_hash_white(position_hash, orig, dest);
      layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
      score_state new_score_state = update_score_state_white_move_and_capture(
          w,
          &s,
          orig,
          dest,
          captures);
      i32 score = -search_black(
          pv_data,
          positions,
          tt,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          depth - 1,
          -beta,
          -alpha,
          statistics,
          should_stop,
          true);

      if (score >= beta) {
        statistics->search_beta_cutoff_white++;
        tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, ms[i], ply);
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        best_move = ms[i];
      }
      if (score > alpha) {
        alpha = score;
        update_pv(pv_data, ply, ms[i]);
      }
    }

    // clear the capture destinations for the remaining destinations
    subtract_move_layers(&layers, &capture_layers);
  }

  // ---------------------------------------------------------------------------
  // Remaining pawn moves

  move ms[400];
  int total = 0;
  moves_from_layers(&layers, b.white, b.white_r, ms, NULL, NULL, &total);

  // hacky bounds check
  assert(total < 400);

  // // Score quiet moves by PST delta for move ordering
  // i32 move_scores[400];
  // for (int i = 0; i < total; i++) {
  //   move_scores[i] =
  //       w->psts.white_pst._[ms[i].dest] - w->psts.white_pst._[ms[i].orig];
  // }

  // iterate
  for (int i = 0; i < total; i++) {
    // Skip TT move (already searched)
    if (ms[i].orig == tt_move.orig && ms[i].dest == tt_move.dest)
      continue;

    // // Selection sort: find best-scoring remaining move
    // int best = i;
    // for (int j = i + 1; j < total; j++) {
    //   if (move_scores[j] > move_scores[best])
    //     best = j;
    // }
    // if (best != i) {
    //   move tmp_m = ms[i];
    //   ms[i] = ms[best];
    //   ms[best] = tmp_m;
    //   i32 tmp_s = move_scores[i];
    //   move_scores[i] = move_scores[best];
    //   move_scores[best] = tmp_s;
    // }

    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;

#ifndef NDEBUG
    // Validate move before applying
    struct move m = ms[i];
    move_error error = validate_move(b, m, false);
    if (error != move_error_no_error) {
      print_board(b);
      print_move(m.orig, m.dest);
      printf(
          "invalid remaining pawn move generated in search_white at line %d "
          "with code: %d\n",
          __LINE__,
          error);
      exit(1);
    }
#endif

    board new_b = apply_white_move_m(b, orig, dest);
    u64 new_position_hash = next_hash_white(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_white_move_and_capture(w, &s, orig, dest, captures);
    i32 score = -search_black(
        pv_data,
        positions,
        tt,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        depth - 1,
        -beta,
        -alpha,
        statistics,
        should_stop,
        true);

    if (score >= beta) {
      statistics->search_beta_cutoff_white++;
      tt_store(tt, position_hash, score, TT_LOWER_BOUND, depth, ms[i], ply);
      delete_position(positions, position_index);
      return score;
    }
    if (score > best_value) {
      best_value = score;
      best_move = ms[i];
    }
    if (score > alpha) {
      alpha = score;
      update_pv(pv_data, ply, ms[i]);
    }
  }

  // Store result in TT
  tt_node_type node_type =
      best_value > original_alpha ? TT_EXACT : TT_UPPER_BOUND;
  tt_store(tt, position_hash, best_value, node_type, depth, best_move, ply);

  delete_position(positions, position_index);
  return best_value;
}
