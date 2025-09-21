#include "search.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "king_mobility.h"
#include "layer.h"
#include "limits.h"
#include "move.h"
#include "position_set.h"
#include "score.h"
#include "stdlib.h"
#include "string.h"
#include "victory.h"
#include "x86intrin.h" // IWYU pragma: export
#include "zobrist.h"

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

#define MAX_DEPTH 32

typedef struct pv {
  int pv_length[MAX_DEPTH];
  move pv_table[MAX_DEPTH][MAX_DEPTH];
  move prev_pv[MAX_DEPTH];
  int prev_pv_length;
} pv;

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

i32 quiesce_white(
    pv *pv_data,
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta,
    stats *statistics);

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
    i32 alpha,
    i32 beta,
    stats *statistics) {

  // Increment black position evaluation count
  statistics->quiescence_positions_black++;

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (white_victory(&b)) {
    return MIN_SCORE;
  }

  // If we can't find a quiet position in 6 moves we consider the line unstable
  // and score it as a draw for each player, as we can't tell who comes out on
  // top.
  if (ply > 6) {
    statistics->quiescence_limit_reached++;
    return 0;
  }

  pv_data->pv_length[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    statistics->repeat_moves_encountered++;
    delete_position(positions, position_index);
    return MIN_SCORE;
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

      move ms[100] = {0};
      layer ls[100] = {0};
      layer ls_r[100] = {0};
      int total = 0;
      moves_to(
          corner_paths,
          corner_paths_r,
          b.black,
          b.black_r,
          board_occ(b),
          board_occ_r(b),
          ms,
          ls,
          ls_r,
          &total);

      // If black can't block an escape in one then they've lost.
      if (!total) {
        delete_position(positions, position_index);
        return MIN_SCORE;
      }

      // hacky bounds check
      assert(total < 100);

      // iterate
      for (int i = 0; i < total; i++) {
        u8 orig = ms[i].orig;
        u8 dest = ms[i].dest;
        layer move = ls[i];
        layer move_r = ls_r[i];

        board new_b = apply_black_move(b, move, move_r);
        u64 new_position_hash = next_hash_black(position_hash, orig, dest);
        layer captures =
            apply_captures_z_black(&new_b, &new_position_hash, dest);
        score_state new_score_state = update_score_state_black_move_and_capture(
            w,
            s,
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

    corner_paths_2(
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
    }

    move ms[100] = {0};
    layer ls[100] = {0};
    layer ls_r[100] = {0};
    int total = 0;
    moves_to(
        corner_paths,
        corner_paths_r,
        b.black,
        b.black_r,
        board_occ(b),
        board_occ_r(b),
        ms,
        ls,
        ls_r,
        &total);

    // TODO: if total is 0 then ensure there are followup moves otherwise losing
    // score

    // hacky bounds check
    assert(total < 100);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;
      layer move = ls[i];
      layer move_r = ls_r[i];

      board new_b = apply_black_move(b, move, move_r);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      score_state new_score_state =
          update_score_state_black_move_and_capture(w, s, orig, dest, captures);
      i32 score = -quiesce_white(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
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
      layer destinations = pawn_destinations(b);
      layer destinations_r = pawn_destinations_r(b);

      move ms[400] = {0};
      layer ls[400] = {0};
      layer ls_r[400] = {0};
      int total = 0;
      moves_to(
          destinations,
          destinations_r,
          b.black,
          b.black_r,
          board_occ(b),
          board_occ_r(b),
          ms,
          ls,
          ls_r,
          &total);

      // iterate
      for (int i = 0; i < total; i++) {
        u8 orig = ms[i].orig;
        u8 dest = ms[i].dest;
        layer move = ls[i];
        layer move_r = ls_r[i];

        board new_b = apply_black_move(b, move, move_r);
        u64 new_position_hash = next_hash_black(position_hash, orig, dest);
        layer captures =
            apply_captures_z_black(&new_b, &new_position_hash, dest);
        score_state new_score_state = update_score_state_black_move_and_capture(
            w,
            s,
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
    layer capture_dests =
        find_capture_destinations(b.black, b.white, board_occ(b));
    layer capture_dests_r =
        find_capture_destinations(b.black_r, b.white_r, board_occ_r(b));

    move ms[100] = {0};
    layer ls[100] = {0};
    layer ls_r[100] = {0};
    int total = 0;
    moves_to(
        capture_dests,
        capture_dests_r,
        b.black,
        b.black_r,
        board_occ(b),
        board_occ_r(b),
        ms,
        ls,
        ls_r,
        &total);

    // hacky bounds check
    assert(total < 100);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;
      layer move = ls[i];
      layer move_r = ls_r[i];

      board new_b = apply_black_move(b, move, move_r);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      score_state new_score_state =
          update_score_state_black_move_and_capture(w, s, orig, dest, captures);
      i32 score = -quiesce_white(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
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
    i32 alpha,
    i32 beta,
    stats *statistics) {

  // Increment white position evaluation count
  statistics->quiescence_positions_white++;

  // We only need to check for a black_victory because the previous move will
  // have been black.
  if (black_victory(&b)) {
    return MIN_SCORE;
  }

  // If we can't find a quiet position in 6 moves we consider the line
  // unstable and score it as a draw, as we can't tell who comes out on top.
  if (ply > 6) {
    statistics->quiescence_limit_reached++;
    return 0;
  }

  pv_data->pv_length[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    statistics->repeat_moves_encountered++;
    return MIN_SCORE;
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
    return MAX_SCORE;
  }

  // iterate
  for (int i = 0; i < corner_move_count; i++) {
    u8 orig = king_pos;
    u8 dest = dests[i];
    move m = {orig, dest};

    board new_b = b;
    CLEAR_INDEX(new_b.king, orig);
    CLEAR_INDEX(new_b.king_r, rotate_right[orig]);
    SET_INDEX(new_b.king, dest);
    SET_INDEX(new_b.king_r, rotate_right[dest]);

    u64 new_position_hash = next_hash_king(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_king_move_and_capture(w, s, orig, dest, captures);
    i32 score = -quiesce_black(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
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
    layer destinations = pawn_destinations(b);
    layer destinations_r = pawn_destinations_r(b);
    move ms[400] = {0};
    layer ls[400] = {0};
    layer ls_r[400] = {0};
    int total = 0;
    moves_to(
        destinations,
        destinations_r,
        b.white,
        b.white_r,
        board_occ(b),
        board_occ_r(b),
        ms,
        ls,
        ls_r,
        &total);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;
      layer move = ls[i];
      layer move_r = ls_r[i];

      board new_b = apply_white_move(b, move, move_r);
      u64 new_position_hash = next_hash_white(position_hash, orig, dest);
      layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
      score_state new_score_state =
          update_score_state_white_move_and_capture(w, s, orig, dest, captures);
      i32 score = -quiesce_black(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
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
  layer capture_dests = find_capture_destinations(
      LAYER_OR(LAYER_OR(b.white, b.king), corners),
      b.black,
      king_board_occ(b));
  layer capture_dests_r = find_capture_destinations(
      LAYER_OR(LAYER_OR(b.white_r, b.king_r), corners),
      b.black_r,
      king_board_occ_r(b));

  // ---------------------------------------------------------------------------
  // king capture moves

  // generate capture moves for king
  move ms[100] = {0};
  layer ls[100] = {0};
  layer ls_r[100] = {0};
  int total = 0;
  moves_to_king_impl(
      capture_dests,
      capture_dests_r,
      b.king,
      b.king_r,
      king_board_occ(b),
      king_board_occ_r(b),
      ms,
      ls,
      ls_r,
      &total);

  // hacky bounds check
  assert(total < 100);

  // iterate
  for (int i = 0; i < total; i++) {
    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;
    layer move = ls[i];
    layer move_r = ls_r[i];

    board new_b = apply_king_move(b, move, move_r);
    u64 new_position_hash = next_hash_king(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_king_move_and_capture(w, s, orig, dest, captures);

    i32 score = -quiesce_black(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
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

  // ---------------------------------------------------------------------------
  // pawn capture moves

  total = 0;
  moves_to(
      capture_dests,
      capture_dests_r,
      b.white,
      b.white_r,
      board_occ(b),
      board_occ_r(b),
      ms,
      ls,
      ls_r,
      &total);

  // hacky bounds check
  assert(total < 100);

  // iterate
  // iterate
  for (int i = 0; i < total; i++) {
    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;
    layer move = ls[i];
    layer move_r = ls_r[i];

    board new_b = apply_white_move(b, move, move_r);
    u64 new_position_hash = next_hash_white(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_white_move_and_capture(w, s, orig, dest, captures);
    i32 score = -quiesce_black(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
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

  for (int i = 0; i < total; i++) {
    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;
    layer move = ls[i];
    layer move_r = ls_r[i];

    board new_b = apply_white_move(b, move, move_r);
    u64 new_position_hash = next_hash_white(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_white_move_and_capture(w, s, orig, dest, captures);
    i32 score = -quiesce_black(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
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
      alpha,
      beta,
      &statistics);
  destroy_position_set(positions);
  return create_pv_line(&pv_data, true, result);
}

pv_line quiesce_white_runner_with_stats(
    board b,
    stats *statistics,
    position_set *positions) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, false);
  position_set *local_positions = positions;
  bool created_positions = false;
  if (local_positions == NULL) {
    local_positions = create_position_set(100);
    created_positions = true;
  }
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = quiesce_white(
      &pv_data,
      local_positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      alpha,
      beta,
      statistics);
  if (created_positions) {
    destroy_position_set(local_positions);
  }
  return create_pv_line(&pv_data, false, result);
}

pv_line quiesce_black_runner_with_stats(
    board b,
    stats *statistics,
    position_set *positions) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, true);
  position_set *local_positions = positions;
  bool created_positions = false;
  if (local_positions == NULL) {
    local_positions = create_position_set(100);
    created_positions = true;
  }
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = quiesce_black(
      &pv_data,
      local_positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      alpha,
      beta,
      statistics);
  if (created_positions) {
    destroy_position_set(local_positions);
  }
  return create_pv_line(&pv_data, true, result);
}

i32 search_white(
    pv *pv_data,
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,   // distance from the root
    int depth, // depth remaining
    i32 alpha,
    i32 beta,
    stats *statistics,
    bool is_pv);

/* We try moves in this order:
- PV move
- king escape blockers (1 or 2 moves?)
- capture moves.
- killer move (if legal)
- remaining

we use the is_pv flag to tell us if we're currently in the previously
established principle variation. This is chiefly used to let us know if we can
search the PV move for the current ply before generating other moves. When using
a transposition table this flag isn't necessary; instead we can always try to
pull a hash move, which should include the PV. Some engines will insert the PV
into the TT between iterative deepening stages on the off chance it's been
overwritten.
*/
i32 search_black(
    pv *pv_data,
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,   // distance from the root
    int depth, // depth remaining
    i32 alpha,
    i32 beta,
    stats *statistics,
    bool is_pv) {

  // Increment black position evaluation count
  statistics->search_positions_black++;

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (white_victory(&b)) {
    return MIN_SCORE;
  }

  pv_data->pv_length[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    statistics->repeat_moves_encountered++;
    delete_position(positions, position_index);
    return MIN_SCORE;
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
        alpha,
        beta,
        statistics);
  }

  i32 best_value = MIN_SCORE;

  // TODO: null move pruning

  // PV move
  if (is_pv) {
    move m = pv_data->prev_pv[ply];
    move m_r = ROTATE_MOVE(m);
    u8 orig = m.orig;
    u8 dest = m.dest;
    layer move_layer = move_as_layer(m);
    layer move_layer_r = move_as_layer(m_r);

    board new_b = apply_black_move(b, move_layer, move_layer_r);
    u64 new_position_hash = next_hash_black(position_hash, orig, dest);
    layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_black_move_and_capture(w, s, orig, dest, captures);

    i32 score = -search_white(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        depth - 1,
        -beta,
        -alpha,
        statistics,
        (is_pv && ply < pv_data->prev_pv_length));

    if (score > best_value) {
      best_value = score;
    }
    if (score > alpha) {
      alpha = score;
      update_pv(pv_data, ply, m);
    }
    if (alpha > beta) {
      statistics->search_beta_cutoff_black++;
      delete_position(positions, position_index);
      return best_value;
    }
  }

  // ---------------------------------------------------------------------------
  // Destinations

  layer remaining_destinations = pawn_destinations(b);
  layer remaining_destinations_r = pawn_destinations_r(b);

  // ---------------------------------------------------------------------------
  // capture moves

  // generate capture moves for pawns
  layer capture_dests =
      find_capture_destinations(b.black, b.white, board_occ(b));
  layer capture_dests_r =
      find_capture_destinations(b.black_r, b.white_r, board_occ_r(b));

  {
    move ms[400] = {0};
    layer ls[400] = {0};
    layer ls_r[400] = {0};
    int total = 0;
    moves_to(
        capture_dests,
        capture_dests_r,
        b.black,
        b.black_r,
        board_occ(b),
        board_occ_r(b),
        ms,
        ls,
        ls_r,
        &total);

    // hacky bounds check
    assert(total < 400);

    // iterate
    for (int i = 0; i < total; i++) {
      u8 orig = ms[i].orig;
      u8 dest = ms[i].dest;
      layer move = ls[i];
      layer move_r = ls_r[i];

      board new_b = apply_black_move(b, move, move_r);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      score_state new_score_state =
          update_score_state_black_move_and_capture(w, s, orig, dest, captures);
      i32 score = -search_white(
          pv_data,
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          depth - 1,
          -beta,
          -alpha,
          statistics,
          (is_pv && ply < pv_data->prev_pv_length));

      if (score >= beta) {
        statistics->search_beta_cutoff_black++;
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

  // clear the capture destinations for the remaining destinations
  LAYER_XOR_ASSG(remaining_destinations, capture_dests);
  LAYER_XOR_ASSG(remaining_destinations_r, capture_dests_r);

  // ---------------------------------------------------------------------------
  // Remaining

  move ms[400] = {0};
  layer ls[400] = {0};
  layer ls_r[400] = {0};
  int total = 0;
  moves_to(
      remaining_destinations,
      remaining_destinations_r,
      b.black,
      b.black_r,
      board_occ(b),
      board_occ_r(b),
      ms,
      ls,
      ls_r,
      &total);

  // hacky bounds check
  assert(total < 400);

  // iterate
  for (int i = 0; i < total; i++) {
    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;
    layer move = ls[i];
    layer move_r = ls_r[i];

    board new_b = apply_black_move(b, move, move_r);
    u64 new_position_hash = next_hash_black(position_hash, orig, dest);
    layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
    score_state new_score_state =
        update_score_state_black_move_and_capture(w, s, orig, dest, captures);
    i32 score = -search_white(
        pv_data,
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        depth - 1,
        -beta,
        -alpha,
        statistics,
        (is_pv && ply < pv_data->prev_pv_length));

    if (score >= beta) {
      statistics->search_beta_cutoff_black++;
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

i32 search_white(
    pv *pv_data,
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,   // distance from the root
    int depth, // depth remaining
    i32 alpha,
    i32 beta,
    stats *statistics,
    bool is_pv) {
  (void)positions;
  (void)w;
  (void)s;
  (void)b;
  (void)position_hash;
  (void)ply;
  (void)depth;
  (void)alpha;
  (void)beta;
  (void)statistics;
  (void)is_pv;

  // Increment black position evaluation count
  statistics->search_positions_white++;

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (black_victory(&b)) {
    return MIN_SCORE;
  }

  pv_data->pv_length[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    statistics->repeat_moves_encountered++;
    delete_position(positions, position_index);
    return MIN_SCORE;
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
        alpha,
        beta,
        statistics);
  }

  return 0;
}

pv_line search_black_runner_with_stats(
    board b,
    int depth,
    bool is_pv,
    stats *statistics,
    position_set *positions) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, true);
  position_set *local_positions = positions;
  bool created_positions = false;
  if (local_positions == NULL) {
    local_positions = create_position_set(100);
    created_positions = true;
  }
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = search_black(
      &pv_data,
      local_positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      depth,
      alpha,
      beta,
      statistics,
      is_pv);
  if (created_positions) {
    destroy_position_set(local_positions);
  }
  return create_pv_line(&pv_data, true, result);
}

pv_line search_white_runner_with_stats(
    board b,
    int depth,
    bool is_pv,
    stats *statistics,
    position_set *positions) {
  pv pv_data = {0};
  u64 position_hash = hash_for_board(b, false);
  position_set *local_positions = positions;
  bool created_positions = false;
  if (local_positions == NULL) {
    local_positions = create_position_set(100);
    created_positions = true;
  }
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = search_white(
      &pv_data,
      local_positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      depth,
      alpha,
      beta,
      statistics,
      is_pv);
  if (created_positions) {
    destroy_position_set(local_positions);
  }
  return create_pv_line(&pv_data, false, result);
}
