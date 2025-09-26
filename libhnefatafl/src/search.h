#include "move.h"
#include "score.h"
#include "stdbool.h"

typedef struct position_set position_set;

// INT_MIN == -2147483648, and -INT_MIN == INT_MIN due to integer overflow, so
// it's important that we define this value that can safely be negated without
#define INFINITY 2147483647

// We define these constants to be 1 less than INFINITY so that we can return a
// maximum/minimum score that does not equal the starting alpha/beta values and
// trigger the beta cutoff branch at the root.
#define MAX_SCORE 2147483646
#define MIN_SCORE -2147483646

typedef struct pv_line {
  bool is_black_turn;
  move *moves;
  int length;
  i32 score;
} pv_line;

#define MAX_DEPTH 32

typedef struct pv {
  int pv_length[MAX_DEPTH];
  move pv_table[MAX_DEPTH][MAX_DEPTH];
  move prev_pv[MAX_DEPTH];
  int prev_pv_length;
} pv;

typedef struct stats {
  int search_positions_black;
  int search_positions_white;
  int search_beta_cutoff_black;
  int search_beta_cutoff_white;
  int quiescence_positions_black;
  int quiescence_positions_white;
  int quiencence_beta_cutoff_black;
  int quiencence_beta_cutoff_white;
  int quiescence_limit_reached;
  int repeat_moves_encountered;
} stats;

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
    stats *statistics);

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

pv_line quiesce_white_runner(board b);
pv_line quiesce_black_runner(board b);

void destroy_pv_line(pv_line *line);

// Helper macro to create an empty pv struct
#define CREATE_EMPTY_PV()                                                      \
  (&(pv){                                                                      \
      .pv_length = {0},                                                        \
      .pv_table = {{0}},                                                       \
      .prev_pv = {0},                                                          \
      .prev_pv_length = 0})

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
    bool is_pv);

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
