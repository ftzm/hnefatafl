#include "position_set.h"
#include "board.h"
#include "layer.h"
#include "move.h"
#include "stdbool.h"
#include "score.h"

// typedef struct negamax_ab_result {
//   move _move;
//   board _board;
//   i32 _score;
//   score_state ss;
// } negamax_ab_result;
// 
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
//   PV_LENGTH[ply] = ply;
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
//       get_team_moves<false>(b, &total, moves_table, cap_counts, boards_table);
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
//       combi[i].ss.pst_black_score -= ai_settings.black_pst[combi[i]._move.orig];
//       combi[i].ss.pst_black_score += ai_settings.black_pst[combi[i]._move.dest];
//     } else if (combi[i]._move.orig == !king_pos) {
//       combi[i].ss.pst_white_score -= ai_settings.white_pst[combi[i]._move.orig];
//       combi[i].ss.pst_white_score += ai_settings.white_pst[combi[i]._move.dest];
//     } else {
//       combi[i].ss.pst_king_score -= ai_settings.king_pst[combi[i]._move.orig];
//       combi[i].ss.pst_king_score += ai_settings.king_pst[combi[i]._move.dest];
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
//     // if (depth == 1 && (m.orig == 115 && m.dest == 114)) printf("score: %d\n",
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
//       // --------------------------------------------------------------- assign
//       // move discovered here
//       PV_TABLE[ply][ply] = combi[i]._move;
//       PV_TABLE_BOARDS[ply][ply] = combi[i]._board; // me only
//       // copy up moves discovered at lower depths
//       for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
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
// 
// i32 quiesce(
//     const move m,
//     const board b,
//     const team_repetitions r,
//     const bool is_black_turn,
//     // const int depth,
//     const int ply,
//     i32 alpha,
//     const i32 beta,
//     int *tally,
//     // const bool is_pv,
//     const score_state ss,
//     // const bool allow_null_move,
//     const struct ai_settings &ai_settings) {
//   // base implementation from:
//   // https://www.chessprogramming.org/Quiescence_Search
// 
//   // TODO: implement delta pruning
// 
//   // assert we don't exceed a generous ply limit to guard against infinite loops
//   assert(ply < 20);
// 
//   // int stand_pat = Evaluate();
//   // if( stand_pat >= beta )
//   //     return beta;
//   // if( alpha < stand_pat )
//   //     alpha = stand_pat;
//   int stand_pat = score_board(b, is_black_turn) + ss.get_score(is_black_turn);
//   if (stand_pat >= beta)
//     return beta;
//   if (alpha < stand_pat)
//     alpha = stand_pat;
// 
//   // Gen moves
//   // 1. a) if black, moves that block escape paths
//   //    b) if white, escape moves
//   // 2. capture moves
// 
//   // until( every_capture_has_been_examined )  {
//   //     MakeCapture();
//   //     score = -Quiesce( -beta, -alpha );
//   //     TakeBackMove();
// 
//   //     if( score >= beta )
//   //         return beta;
//   //     if( score > alpha )
//   //        alpha = score;
//   // }
//   // return alpha;
// }
// 
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
//         INT_MIN,
//         INT_MAX,
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

i32 quiesce_white(position_set *positions, score_weights *w, score_state *s, board *b) {
  // check for repetition

// white to move, so we score for white
// This is sort of like a null move score
  i32 stand_pat = white_score(w, s, b);
  
}


