#pragma once

#include "stdbool.h"
#include "types.h"

bool king_captured(const board *b);

typedef struct corner_guard_state {
  u8 nw_guard_count;
  u8 ne_guard_count;
  u8 sw_guard_count;
  u8 se_guard_count;
} corner_guard_state;

typedef struct piece_square_table {
  u32 _[121];
} piece_square_table;

typedef struct psts {
  piece_square_table black_pst;
  piece_square_table white_pst;
  piece_square_table king_pst;
} psts;

typedef struct score_weights {
  i32 black_pawn;
  i32 white_pawn;
  i32 corner_guard;
  i32 black_moves;
  i32 white_moves;
  i32 king_moves;
  i32 king_surrounders;
  psts psts;
} score_weights;

typedef struct score_state {
  corner_guard_state corner_guard;
  i32 score;
} score_state;

score_state init_score_state(score_weights *weights, const board *b);
score_state update_score_state_white_move(
    const score_weights *weights,
    const score_state *old,
    int orig,
    int dest);
score_state update_score_state_black_move(
    const score_weights *weights,
    const score_state *old,
    int orig,
    int dest);
score_state update_score_state_white_move_and_capture(
    const score_weights *weights,
    const score_state *old,
    int orig,
    int dest,
    const layer captures);
score_state update_score_state_black_move_and_capture(
    const score_weights *weights,
    const score_state *old,
    int orig,
    int dest,
    const layer captures);
score_state update_score_state_king_move(
    const score_weights *weights,
    const score_state *old,
    int orig,
    int dest);
score_state update_score_state_king_move_and_capture(
    const score_weights *weights,
    const score_state *old,
    int orig,
    int dest,
    const layer captures);
i32 black_score(score_weights *w, score_state *s, board *b);
i32 white_score(score_weights *w, score_state *s, board *b);
i32 king_score(score_weights *w, score_state *s, board *b);

psts init_default_psts(void);
score_weights init_default_weights(void);
