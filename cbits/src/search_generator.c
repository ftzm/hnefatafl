#include "search_generator.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "io.h"
#include "king_mobility.h"
#include "layer.h"
#include "move.h"
#include "move_legacy.h"
#include "position_set.h"
#include "score.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "victory.h"
#include "x86intrin.h"
#include "zobrist.h"

// Constants from search.h
#define INFINITY 2147483647
#define MAX_SCORE 2147483646
#define MIN_SCORE -2147483646
#define MAX_DEPTH 32

// External functions and variables from search.c
extern int PV_LENGTH[MAX_DEPTH];
extern move PV_TABLE[MAX_DEPTH][MAX_DEPTH];
extern void clear_pv_memory();
extern pv_line create_pv_line(bool is_black_turn, i32 result);
extern void destroy_pv_line(pv_line *line);

// Local stub for inline function
static void update_pv(int ply, move m) {
  PV_TABLE[ply][ply] = m;

  // copy up moves discovered at lower depths
  for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
    PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
  }

  // adjust pv length
  PV_LENGTH[ply] = PV_LENGTH[ply + 1];
}

i32 quiesce_black_generator(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta) {

  // We only need to check for a king escape because the previous move will
  // have been white.
  if (king_effectively_escaped(&b)) {
    return MIN_SCORE;
  }

  // assert we don't exceed a generous ply limit to guard against infinite loops
  if (ply > 5) {
    return 0;
  }

  PV_LENGTH[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    // return 0;
    return MIN_SCORE;
  }

  // Start with a static eval as best_value
  i32 best_value = black_score(w, &s, &b);

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  // ---------------------------------------------------------------------------
  // escape-in-1 blocking dests
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
      // If there are escape paths then we return a losing score unless we can
      // raise the score with a blocking move
      best_value = alpha = MIN_SCORE;

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

        i32 score = -quiesce_white_generator(
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
          update_pv(ply, ms[i]);
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
      best_value = alpha = MIN_SCORE;
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
      i32 score = -quiesce_white_generator(
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          -beta,
          -alpha);

      if (score >= beta) {
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }
  }

  // ---------------------------------------------------------------------------
  // stand pat
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

      // print_layer(move);
      // print_layer(move_r);
      board new_b = apply_black_move(b, move, move_r);
      u64 new_position_hash = next_hash_black(position_hash, orig, dest);
      layer captures = apply_captures_z_black(&new_b, &new_position_hash, dest);
      // print_layer(captures);
      // printf("black_capture");
      // printf("new_postition_hash %juULL\n", new_position_hash);
      // print_board_move(new_b, orig, dest, captures);
      score_state new_score_state =
          update_score_state_black_move_and_capture(w, s, orig, dest, captures);
      i32 score = -quiesce_white_generator(
          positions,
          w,
          new_score_state,
          new_b,
          new_position_hash,
          ply + 1,
          -beta,
          -alpha);

      if (score >= beta) {
        delete_position(positions, position_index);
        return score;
      }
      if (score > best_value) {
        best_value = score;
        update_pv(ply, ms[i]);
      }
      if (score > alpha) {
        alpha = score;
      }
    }
  }

  // remove the position from the set as we exit
  delete_position(positions, position_index);

  return best_value;
}

i32 quiesce_white_generator(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta) {

  // We only need to check for a king capture because the previous move will
  // have been black.
  if (king_captured(&b)) {
    return MIN_SCORE;
  }

  // assert we don't exceed a generous ply limit to guard against infinite loops
  if (ply > 6) {
    return 0;
  }

  PV_LENGTH[ply] = ply;

  // check for repetition
  int position_index;
  int collision = insert_position(positions, position_hash, &position_index);
  if (collision) {
    // we consider the position a draw, and thus score it 0
    return MIN_SCORE;
  }

  // white to move, so we score for white
  i32 best_value = white_score(w, &s, &b);

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

    i32 score = -quiesce_black_generator(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      delete_position(positions, position_index);
      return score;
    }
    if (score > best_value) {
      best_value = score;
      update_pv(ply, m);
    }
    if (score > alpha) {
      alpha = score;
    }
  }

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
  // generate capture moves

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
  move ms[100];
  layer ls[100];
  layer ls_r[100];
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

    i32 score = -quiesce_black_generator(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      delete_position(positions, position_index);
      return score;
    }
    if (score > best_value) {
      best_value = score;
      update_pv(ply, ms[i]);
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
    i32 score = -quiesce_black_generator(
        positions,
        w,
        new_score_state,
        new_b,
        new_position_hash,
        ply + 1,
        -beta,
        -alpha);

    if (score >= beta) {
      delete_position(positions, position_index);
      return score;
    }
    if (score > best_value) {
      best_value = score;
      update_pv(ply, ms[i]);
    }
    if (score > alpha) {
      alpha = score;
    }
  }

  // remove the position from the set as we exit
  delete_position(positions, position_index);

  return best_value;
}

pv_line quiesce_white_runner_generator(board b) {
  clear_pv_memory();
  u64 position_hash = hash_for_board(b, false);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = quiesce_white_generator(
      positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      alpha,
      beta);
  destroy_position_set(positions);
  return create_pv_line(false, result);
}

pv_line quiesce_black_runner_generator(board b) {
  clear_pv_memory();
  u64 position_hash = hash_for_board(b, true);
  position_set *positions = create_position_set(100);
  score_weights weights = init_default_weights();
  score_state s = init_score_state(&weights, &b);
  int ply = 0;
  i32 alpha = -INFINITY;
  i32 beta = INFINITY;
  i32 result = quiesce_black_generator(
      positions,
      &weights,
      s,
      b,
      position_hash,
      ply,
      alpha,
      beta);
  destroy_position_set(positions);
  return create_pv_line(true, result);
}
