#include "score.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "layer.h"
#include "limits.h"
#include "move.h"
#include "x86intrin.h" // IWYU pragma: export

#define MAP_INDICES(_l, _f)                                                    \
  while (_l._[0]) {                                                            \
    int i = _tzcnt_u64(_l._[0]);                                               \
    _f;                                                                        \
    _l._[0] = _blsr_u64(_l._[0]);                                              \
  }                                                                            \
  while (_l._[1]) {                                                            \
    int i = 64 + _tzcnt_u64(_l._[1]);                                          \
    _f;                                                                        \
    _l._[1] = _blsr_u64(_l._[1]);                                              \
  }

/*
typedef enum piece_type : u8 {
  black_type = 1,
  white_type = 2,
  king_type = 3,
} piece_type;
*/

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// corner guard

inline u32 score_for_count(u8 count) {
  switch (count) {
  // case 0: return 0;
  case 1:
    return 1;
  case 2:
    return 20;
  case 3:
    return 400;
  default:
    return 0;
  }
}

inline u32 score_adjustment_for_count(u8 count) {
  switch (count) {
  // case 0: return 0;
  case 1:
    return 1;
  case 2:
    return 19;
  case 3:
    return 380;
  default:
    return 0;
  }
}

corner_guard_state
init_state_corner_guard(const board *b, const i32 weight, i32 *score) {
  u8 nw = __builtin_popcountll(corner_guard_nw._[1] & b->black._[1]);
  u8 ne = __builtin_popcountll(corner_guard_ne._[1] & b->black._[1]);
  u8 sw = __builtin_popcountll(corner_guard_sw._[0] & b->black._[0]);
  u8 se = __builtin_popcountll(corner_guard_se._[0] & b->black._[0]);
  *score += (score_for_count(nw) + score_for_count(ne) + score_for_count(sw) +
            score_for_count(se)) *
           weight;
  return (corner_guard_state){nw, ne, sw, se};
}

i32 corner_guard_stateless(const board *b, const i32 weight) {
  return ((score_for_count(
               __builtin_popcountll(corner_guard_nw._[1] & b->black._[1])) +
           score_for_count(
               __builtin_popcountll(corner_guard_ne._[1] & b->black._[1])) +
           score_for_count(
               __builtin_popcountll(corner_guard_sw._[0] & b->black._[0])) +
           score_for_count(
               __builtin_popcountll(corner_guard_se._[0] & b->black._[0])))) *
         weight;
}

i32 handle_arrival(corner_guard_state *state, int dest) {
  switch (dest) {
  case 118:
  case 108:
  case 98:
    return score_adjustment_for_count(++state->nw_guard_count);
  case 112:
  case 100:
  case 88:
    return score_adjustment_for_count(++state->ne_guard_count);
  case 32:
  case 20:
  case 8:
    return score_adjustment_for_count(++state->sw_guard_count);
  case 22:
  case 12:
  case 2:
    return score_adjustment_for_count(++state->se_guard_count);
  }
  return 0;
}

i32 handle_departure(corner_guard_state *state, int orig) {
  switch (orig) {
  case 118:
  case 108:
  case 98:
    return score_adjustment_for_count(state->nw_guard_count--);
  case 112:
  case 100:
  case 88:
    return score_adjustment_for_count(state->ne_guard_count--);
  case 32:
  case 20:
  case 8:
    return score_adjustment_for_count(state->sw_guard_count--);
  case 22:
  case 12:
  case 2:
    return score_adjustment_for_count(state->se_guard_count--);
  }
  return 0;
}

inline void corner_guard_handle_black_move(
    const i32 weight,
    corner_guard_state *state,
    i32 *score,
    int orig,
    int dest) {
  *score -= handle_departure(state, orig) * weight;
  *score += handle_arrival(state, dest) * weight;
}

inline void corner_guard_capture_adjust(
    const i32 weight, layer captures, corner_guard_state *state, i32 *score) {
  MAP_INDICES(captures, *score -= handle_departure(state, i) * weight);
}

// -----------------------------------------------------------------------------
// pawn count

i32 init_pawn_count_score(
    const board *b, const i32 black_weight, const i32 white_weight) {
  return (
      (black_pawn_count(b) * black_weight) - (white_pawn_count(b) * white_weight));
}

i32 pawn_count_capture_adjust(const i32 weight, layer captures) {
  i32 count = 0;
  MAP_INDICES(captures, count += 1);
  return count * weight;
}

// -----------------------------------------------------------------------------
// piece square table
//
// Note: pst doesn't have any weights associated with it. If I want to
// weight it against other score factors I can do so at construction
// time so that the values of the table itself are adjusted with the
// weight; I don't need to apply the weight at every incremental score
// update.
piece_square_table quarter_to_pst(u32 quarter[29]) {
  static int indices[29] = {1,  2,  3,  4,  5,  11, 12, 13, 14, 15,
                            16, 22, 23, 24, 25, 26, 27, 33, 34, 35,
                            36, 37, 38, 44, 45, 46, 47, 48, 49};

  piece_square_table pst = {0};

  for (int i = 0; i < 29; i++) {
    i32 val = quarter[i];
    int index = indices[i];
    pst._[index] = val;
    index = rotate_right[index];
    pst._[index] = val;
    index = rotate_right[index];
    pst._[index] = val;
    index = rotate_right[index];
    pst._[index] = val;
  }
  return pst;
}

u32 black_niave_pst_quarter[29] = {
    0, // 1
    0, // 2
    0, // 3
    0, // 4
    0, // 5
    0, // 11
    0, // 12
    0, // 13
    0, // 14
    0, // 15
    0, // 16
    0, // 22
    0, // 23
    0, // 24
    0, // 25
    0, // 26
    0, // 27
    0, // 33
    0, // 34
    0, // 35
    0, // 36
    0, // 37
    0, // 38
    0, // 44
    0, // 45
    0, // 46
    0, // 47
    0, // 48
    0  // 49
};

u32 white_niave_pst_quarter[29] = {
    0, // 1
    0, // 2
    0, // 3
    0, // 4
    0, // 5
    0, // 11
    0, // 12
    0, // 13
    0, // 14
    0, // 15
    0, // 16
    0, // 22
    0, // 23
    0, // 24
    0, // 25
    0, // 26
    0, // 27
    0, // 33
    0, // 34
    0, // 35
    0, // 36
    0, // 37
    0, // 38
    0, // 44
    0, // 45
    0, // 46
    0, // 47
    0, // 48
    0  // 49
};

u32 king_niave_pst_quarter[29] = {
    0,   // 1
    500, // 2
    0,   // 3
    0,   // 4
    0,   // 5
    0,   // 11
    500, // 12
    500, // 13
    0,   // 14
    0,   // 15
    0,   // 16
    500, // 22
    500, // 23
    500, // 24
    0,   // 25
    0,   // 26
    0,   // 27
    0,   // 33
    0,   // 34
    0,   // 35
    0,   // 36
    0,   // 37
    -5,  // 38
    0,   // 44
    0,   // 45
    0,   // 46
    0,   // 47
    -10, // 48
    -15  // 49
};

psts init_psts() {
  return (psts){
      quarter_to_pst(black_niave_pst_quarter),
      quarter_to_pst(white_niave_pst_quarter),
      quarter_to_pst(king_niave_pst_quarter),
  };
}

int pst_handle_move(const piece_square_table *pst, int orig, int dest) {
  return pst->_[dest] - pst->_[orig];
}

i32 init_pst_score(const psts *psts, const board *input) {
  board b = *input;
  i32 score = 0;
  MAP_INDICES(b.black, score += psts->black_pst._[i]);
  MAP_INDICES(b.white, score -= psts->white_pst._[i]);
  MAP_INDICES(b.king, score -= psts->king_pst._[i]);
  return score;
}

i32 pst_capture_handler(const piece_square_table *pst, layer captures) {
  i32 adjust = 0;
  MAP_INDICES(captures, adjust += pst->_[i]);
  return adjust;
}

// -----------------------------------------------------------------------------
// Full setup

score_state init_score_state(score_weights *weights, const board *b) {
  i32 score =
      init_pawn_count_score(b, weights->black_pawn, weights->white_pawn) +
      init_pst_score(&weights->psts, b);
  corner_guard_state cgs =
      init_state_corner_guard(b, weights->corner_guard, &score);
  return (score_state){cgs, score};
}

// -----------------------------------------------------------------------------
// King surrounders

i32 king_surrounder_score(const board *b, const i32 weight) {
  int king_pos = LOWEST_INDEX(b->king);
  layer surround_mask = surround_masks[king_pos];
  layer black_surrounders = LAYER_AND(surround_mask, b->black);
  u8 black_count = __builtin_popcountll(black_surrounders._[0] | black_surrounders._[1]);
  return black_count * weight;
}

// -----------------------------------------------------------------------------
// Top-level usage
// NOTE: TODO: remember that I specifically generate capture moves first, which
// means that I know which moves lead to captures and which don't ahead of time.
// I can skip computing:
// - corner guard capture adjust
// - pawn count
// for non-capture moves.
// Might make sense to have one non-capture state update functon for each color,
// and one additional function for each colow that only does the
// capture-specific updates. getting a score out of the state can be shared.

void update_score_state_black_no_capture(
    const score_weights *weights, score_state *s, int orig, int dest) {
  s->score += pst_handle_move(&weights->psts.black_pst, orig, dest);
  corner_guard_handle_black_move(
      weights->corner_guard, &s->corner_guard, &s->score, orig, dest);
}

void update_score_state_black_capture(
    const score_weights *weights, score_state *s, const layer captures) {
  s->score += pst_capture_handler(&weights->psts.white_pst, captures);
  s->score += pawn_count_capture_adjust(weights->white_pawn, captures);
}

void update_score_state_white_no_capture(
    const score_weights *weights, score_state *s, int orig, int dest) {
  s->score -= pst_handle_move(&weights->psts.white_pst, orig, dest);
}

void update_score_state_white_capture(
    const score_weights *weights, score_state *s, const layer captures) {
  s->score -= pst_capture_handler(&weights->psts.black_pst, captures);
  s->score -= pawn_count_capture_adjust(weights->black_pawn, captures);
  corner_guard_capture_adjust(
      weights->corner_guard, captures, &s->corner_guard, &s->score);
}

void update_score_state_king_no_capture(
    const score_weights *weights, score_state *s, int orig, int dest) {
  s->score -= pst_handle_move(&weights->psts.king_pst, orig, dest);
}

void update_score_state_king_capture(
    const score_weights *weights, score_state *s, const layer captures) {
  s->score -= pst_capture_handler(&weights->psts.black_pst, captures);
  s->score -= pawn_count_capture_adjust(weights->black_pawn, captures);
  corner_guard_capture_adjust(
      weights->corner_guard, captures, &s->corner_guard, &s->score);
}

i32 black_score(score_weights *w, score_state *s, board *b) {
  i32 black_moves = black_moves_count(b) * w->black_moves;
  i32 white_moves = white_moves_count(b) * w->white_moves;
  i32 king_moves = king_moves_count(b) * w->king_moves;
  i32 king_surrounders = king_surrounder_score(b, w->king_surrounders);
  return s->score + black_moves - white_moves - king_moves + king_surrounders;
}

i32 white_score(score_weights *w, score_state *s, board *b) {
  return -black_score(w, s, b);
}
