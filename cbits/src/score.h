#pragma once

#include "board.h"
#include "stdbool.h"

bool king_captured(const board *b);

// Forward declarations
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
  psts psts;
} score_weights;

typedef struct score_state {
  corner_guard_state corner_guard;
  i32 score;
} score_state;

// Function declarations
psts init_psts(void);
score_state init_score_state(score_weights *weights, const board *b);
void update_score_state_white_no_capture(const score_weights *weights, score_state *s, int orig, int dest);
void update_score_state_black_no_capture(const score_weights *weights, score_state *s, int orig, int dest);
void update_score_state_white_capture(const score_weights *weights, score_state *s, const layer captures);
void update_score_state_black_capture(const score_weights *weights, score_state *s, const layer captures);
void update_score_state_king_no_capture(const score_weights *weights, score_state *s, int orig, int dest);
void update_score_state_king_capture(const score_weights *weights, score_state *s, const layer captures);
i32 black_score(score_weights *w, score_state *s, board *b);
i32 white_score(score_weights *w, score_state *s, board *b);
i32 king_score(score_weights *w, score_state *s, board *b);
