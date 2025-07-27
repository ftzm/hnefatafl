#include "search.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "king_mobility.h"
#include "layer.h"
#include "limits.h"
#include "move.h"
#include "move_legacy.h"
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
//   // assert we don't exceed a generous ply limit to guard against infinite
//   loops assert(ply < 20);
//
//   // int stand_pat = Evaluate();
//   // if( stand_pat >= beta )
//   //     return beta;
//   // if( alpha < stand_pat )
//   //     alpha = stand_pat;
//   int stand_pat = score_board(b, is_black_turn) +
//   ss.get_score(is_black_turn); if (stand_pat >= beta)
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
int PV_LENGTH[MAX_DEPTH];
move PV_TABLE[MAX_DEPTH][MAX_DEPTH];
board PV_TABLE_BOARDS[MAX_DEPTH][MAX_DEPTH];
move PREV_PV[MAX_DEPTH];
int PREV_PV_LENGTH;

pv_line create_pv_line(bool is_black_turn, i32 result) {
  move *moves = malloc(sizeof(move) * PV_LENGTH[0]);
  memcpy(moves, PV_TABLE[0], sizeof(move) * PV_LENGTH[0]);
  return (pv_line){is_black_turn, moves, PV_LENGTH[0], result};
}

void destroy_pv_line(pv_line *line) { free(line->moves); }

i32 quiesce_white(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta);

i32 quiesce_black(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta) {

  // printf("ply: %d\n", ply);
  // // printf("hash_for_board %juULL\n", hash_for_board(b, true));
  // printf("in quiesce black\n");
  // print_board(b);

  if (LAYER_POPCOUNT(b.king) > 1) {
    print_layer(b.king);
  }
  // printf("king\n");
  // print_layer(b.king);
  // printf("white\n");
  // print_layer(b.white);
  // printf("black\n");
  // print_layer(b.black);

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (king_effectively_escaped(&b)) {
    return MIN_SCORE;
  }

  // assert we don't exceed a generous ply limit to guard against infinite loops
  assert(ply < 20);

  PV_LENGTH[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    return MIN_SCORE;
  }

  // generate capture moves for pawns
  layer corner_paths = EMPTY_LAYER;
  layer corner_paths_r = EMPTY_LAYER;
  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  i32 static_eval = black_score(w, &s, &b);

  if (ply == 0) {
    // printf("initial static eval for black: %d\n", static_eval);
  }

  // stand pat
  // This is sort of like a null move score
  i32 best_value = static_eval;
  if (best_value >= beta) {
    return best_value;
  }
  if (best_value > alpha) {
    alpha = best_value;
  }

  move ms[100];
  layer ls[100];
  layer ls_r[100];
  int total;

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

  // ---------------------------------------------------------------------------
  // escape-in-1 blocking dests
  // TODO: if there are escape paths we should set

  corner_paths_1(
      LAYER_OR(b.black, b.white),
      LAYER_OR(b.black_r, b.white_r),
      king_rank,
      king_file,
      &corner_paths,
      &corner_paths_r);

  if (NOT_EMPTY(corner_paths)) {
    // If there are escape paths then we return a losing score unless we can
    // raise the score with a blocking move
    best_value = MIN_SCORE;

    total = 0;
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
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      // printf("new_postition_hash %juULL\n", new_position_hash);
      // print_board_move(new_b, orig, dest, captures);
      score_state new_score_state =
          update_score_state_black_move_and_capture(w, s, orig, dest, captures);
      i32 score = -quiesce_white(
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          -beta,
          -alpha);

      if (score >= beta) {
        return score;
      }
      if (score > best_value) {
        best_value = score;

        PV_TABLE[ply][ply] = ms[i];
        // copy up moves discovered at lower depths
        for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1];
             next_ply++) {
          PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
        }
        // adjust pv length
        PV_LENGTH[ply] = PV_LENGTH[ply + 1];
      }
      if (score > alpha) {
        alpha = score;
      }
    }

    return best_value;
  }

  // ---------------------------------------------------------------------------
  // escape-in-2 blocking dests
  // TODO: if there are escape paths we should set

  corner_paths_2(
      LAYER_OR(b.black, b.white),
      LAYER_OR(b.black_r, b.white_r),
      king_rank,
      king_file,
      &corner_paths,
      &corner_paths_r);

  if (NOT_EMPTY(corner_paths)) {
    best_value = MIN_SCORE;
  }

  total = 0;

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
    // printf("new_postition_hash %juULL\n", new_position_hash);
    // print_board_move(new_b, orig, dest, captures);
    score_state new_score_state =
        update_score_state_black_move_and_capture(w, s, orig, dest, captures);
    i32 score = -quiesce_white(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      return score;
    }
    if (score > best_value) {
      best_value = score;

      PV_TABLE[ply][ply] = ms[i];
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
        PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // ---------------------------------------------------------------------------
  // pawn capture moves

  // generate capture moves for pawns
  layer capture_dests =
      find_capture_destinations(b.black, b.white, board_occ(b));
  layer capture_dests_r =
      find_capture_destinations(b.black_r, b.white_r, board_occ_r(b));

  total = 0;
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
    // printf("new_postition_hash %juULL\n", new_position_hash);
    // print_board_move(new_b, orig, dest, captures);
    score_state new_score_state =
        update_score_state_black_move_and_capture(w, s, orig, dest, captures);
    i32 score = -quiesce_white(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      return score;
    }
    if (score > best_value) {
      best_value = score;

      PV_TABLE[ply][ply] = ms[i];
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
        PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // ---------------------------------------------------------------------------

  // remove the position from the set as we exit
  delete_position(positions, position_index);

  // printf("best value from black: %d\n", best_value);
  return best_value;
}

i32 quiesce_white(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta) {

  // printf("ply: %d\n", ply);
  // // printf("hash_for_board %juULL\n", hash_for_board(b, false));
  // printf("in quiesce white\n");
  // print_board(b);

  if (LAYER_POPCOUNT(b.king) > 1) {
    print_layer(b.king);
  }

  // We only need to check for a king capture because the previous move will
  // have been black.
  if (king_captured(&b)) {
    return MIN_SCORE;
  }

  // assert we don't exceed a generous ply limit to guard against infinite loops
  assert(ply < 20);

  PV_LENGTH[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    return MIN_SCORE;
  }

  // white to move, so we score for white
  i32 static_eval = white_score(w, &s, &b);

  if (ply == 0) {
    // printf("initial static eval for white: %d\n", static_eval);
  }

  // stand pat
  // This is sort of like a null move score
  i32 best_value = static_eval;
  if (best_value >= beta) {
    return best_value;
  }
  if (best_value > alpha) {
    alpha = best_value;
  }

  // generate capture moves
  // TODO: remove throne from dests before finding pawn captures
  // actually maybe not necessary, it's covered by the blockers in moves_to
  layer capture_dests = find_capture_destinations(
      LAYER_OR(b.white, b.king),
      b.black,
      king_board_occ(b));
  layer capture_dests_r = find_capture_destinations(
      LAYER_OR(b.white_r, b.king_r),
      b.black_r,
      king_board_occ_r(b));

  // shared move memory
  // 100 is an arbitrary number but almost certainly sufficient.
  // TODO: find the "correct" size for this based on the maximum capture count,
  // either theoretical or statistical
  move ms[100];
  layer ls[100];
  layer ls_r[100];

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);
  // printf("king rank: %d\n", king_rank);
  // printf("king file: %d\n", king_file);

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
    return MAX_SCORE;
  }

  // iterate
  for (int i = 0; i < corner_move_count; i++) {
    u8 orig = king_pos;
    u8 dest = dests[i];
    move m = {orig, dest};
    // print_move(orig, dest);

    board new_b = b;
    CLEAR_INDEX(new_b.king, orig);
    CLEAR_INDEX(new_b.king_r, rotate_right[orig]);
    SET_INDEX(new_b.king, dest);
    SET_INDEX(new_b.king_r, rotate_right[dest]);

    u64 new_position_hash = next_hash_king(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    // printf("new_postition_hash %juULL\n", new_position_hash);
    // print_board_move(new_b, orig, dest, captures);
    score_state new_score_state =
        update_score_state_king_move_and_capture(w, s, orig, dest, captures);
    // printf("from king escape\n");
    i32 score = -quiesce_black(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      return score;
    }
    if (score > best_value) {
      best_value = score;

      PV_TABLE[ply][ply] = m;
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
        PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // ---------------------------------------------------------------------------
  // king capture moves

  // generate capture moves for king
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
    // printf("new_postition_hash %juULL\n", new_position_hash);
    score_state new_score_state =
        update_score_state_king_move_and_capture(w, s, orig, dest, captures);

    // printf("from king capture\n");
    // print_layer(capture_dests);
    // print_layer(capture_dests_r);
    // print_layer(board_occ(b));
    // print_layer(board_occ_r(b));
    i32 score = -quiesce_black(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      return score;
    }
    if (score > best_value) {
      best_value = score;

      PV_TABLE[ply][ply] = ms[i];
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
        PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // ---------------------------------------------------------------------------
  // pawn capture moves

  total = 0;
  // generate capture moves for pawns
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
  for (int i = 0; i < total; i++) {
    u8 orig = ms[i].orig;
    u8 dest = ms[i].dest;
    layer move = ls[i];
    layer move_r = ls_r[i];

    board new_b = apply_white_move(b, move, move_r);
    u64 new_position_hash = next_hash_white(position_hash, orig, dest);
    layer captures = apply_captures_z_white(&new_b, &new_position_hash, dest);
    // printf("new_postition_hash %juULL\n", new_position_hash);
    score_state new_score_state =
        update_score_state_white_move_and_capture(w, s, orig, dest, captures);
    i32 score = -quiesce_black(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      return score;
    }
    if (score > best_value) {
      best_value = score;

      PV_TABLE[ply][ply] = ms[i];
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
        PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // remove the position from the set as we exit
  delete_position(positions, position_index);

  // printf("best value from white: %d\n", best_value);
  return best_value;
}

pv_line quiesce_white_runner(board b) {
  u64 position_hash = hash_for_board(b, false);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = MIN_SCORE;
  i32 beta = MAX_SCORE;
  i32 result =
      quiesce_white(positions, &weights, s, b, position_hash, ply, alpha, beta);
  destroy_position_set(positions);
  return create_pv_line(false, result);
}

pv_line quiesce_black_runner(board b) {
  u64 position_hash = hash_for_board(b, true);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = MIN_SCORE;
  i32 beta = MAX_SCORE;
  i32 result =
      quiesce_black(positions, &weights, s, b, position_hash, ply, alpha, beta);
  destroy_position_set(positions);
  return create_pv_line(true, result);
}
