#pragma once

#include "move.h"
#include "score.h"
#include "stdbool.h"

// Forward declarations
typedef struct position_set position_set;

// From search.h to avoid duplicate includes
struct pv_line {
  bool is_black_turn;
  move *moves;
  int length;
  i32 score;
} typedef pv_line;

void destroy_pv_line(pv_line *line);

// Generator-based versions of quiescence functions
i32 quiesce_white_generator(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta);

i32 quiesce_black_generator(
    position_set *positions,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    i32 alpha,
    i32 beta);

// Generator-based runner functions
pv_line quiesce_white_runner_generator(board b);
pv_line quiesce_black_runner_generator(board b);