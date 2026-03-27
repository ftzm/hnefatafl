#include "score.h"
#include "stdatomic.h"
#include "stdbool.h"
#include "types.h"

typedef struct position_set position_set;
typedef struct transposition_table transposition_table;

// INT_MIN == -2147483648, and -INT_MIN == INT_MIN due to integer overflow, so
// it's important that we define this value that can safely be negated without
#define INFINITY 2147483647

// We define these constants to be 1 less than INFINITY so that we can return a
// maximum/minimum score that does not equal the starting alpha/beta values and
// trigger the beta cutoff branch at the root.
#define MAX_SCORE 2147483646
#define MIN_SCORE -2147483646

// Maximum ply depth for PV tables and score adjustment
#define MAX_PLY 32

// Base score for positions where victory is guaranteed but not yet realized
#define INEVITABLE_VICTORY_BASE (MAX_SCORE - MAX_PLY - 1)

// Base score for positions where loss is guaranteed but not yet realized
#define INEVITABLE_LOSS_BASE (MIN_SCORE + MAX_PLY + 1)

// Score for actual victory, adjusted by ply to prefer earlier wins
#define VICTORY_SCORE(ply) (MAX_SCORE - (ply))

// Score for actual loss, adjusted by ply to prefer later losses
#define LOSS_SCORE(ply) (MIN_SCORE + (ply))

// Score for inevitable victory, adjusted by ply to prefer earlier inevitability
#define INEVITABLE_VICTORY_SCORE(ply) (INEVITABLE_VICTORY_BASE - (ply))

// Score for inevitable loss, adjusted by ply to prefer later inevitability
#define INEVITABLE_LOSS_SCORE(ply) (INEVITABLE_LOSS_BASE + (ply))

typedef struct pv_line {
  bool is_black_turn;
  move *moves;
  int length;
  i32 score;
} pv_line;

typedef struct pv {
  int pv_length[MAX_PLY];
  move pv_table[MAX_PLY][MAX_PLY];
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
  int tt_hits;
  int tt_cutoffs;
  int null_move_attempts;
  int null_move_cutoffs;
} stats;

typedef struct search_result {
  pv_line pv;
  stats statistics;
} search_result;

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
    stats *statistics);

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
    stats *statistics);

pv_line quiesce_white_runner(board b);

pv_line quiesce_black_runner(board b);

search_result search_white_runner(
    board b,
    int depth,
    _Atomic bool *should_stop,
    transposition_table *tt);

search_result search_black_runner(
    board b,
    int depth,
    _Atomic bool *should_stop,
    transposition_table *tt);

search_result search_white_runner_iterative(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    transposition_table *tt);

search_result search_black_runner_iterative(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    transposition_table *tt);

search_result search_runner_iterative_trusted(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    bool is_black_turn,
    u64 *zobrist_hashes,
    int hash_count,
    transposition_table *tt);

// Search function pointer type
typedef i32 (*search_func)(
    pv *pv_data,
    position_set *positions,
    transposition_table *tt,
    score_weights *w,
    score_state s,
    board b,
    u64 position_hash,
    int ply,
    int depth,
    i32 alpha,
    i32 beta,
    stats *statistics,
    _Atomic bool *should_stop,
    bool allow_null_move);

// Generic runner functions
search_result search_runner_generic(
    board b,
    int depth,
    _Atomic bool *should_stop,
    search_func search_fn,
    bool is_black,
    transposition_table *tt);

// Generic iterative deepening search runner
//
// Parameters:
//   zobrist_hashes: Array of zobrist hashes representing PAST positions in the
//                   game history. Must NOT include the current board position.
//                   The current position hash will be calculated internally.
//                   Pass NULL if no history tracking is needed.
//   hash_count: Number of hashes in zobrist_hashes array
//
// The function will calculate the current position hash from the board and
// is_black parameters, and use it for repetition detection along with the
// provided history hashes.
search_result search_runner_iterative_generic(
    board b,
    int max_depth,
    _Atomic bool *should_stop,
    search_func search_fn,
    bool is_black,
    u64 *zobrist_hashes,
    int hash_count,
    transposition_table *tt);

// Generic wrapper function for search with timeout
typedef search_result (*search_runner_func)(
    board, int, _Atomic bool *, transposition_table *);
search_result search_with_timeout(
    search_runner_func runner,
    board b,
    int depth,
    int time_limit,
    transposition_table *tt);

search_result
search_white_with_timeout(board b, int depth, int time_limit,
                          transposition_table *tt);

search_result
search_black_with_timeout(board b, int depth, int time_limit,
                          transposition_table *tt);

search_result
search_white_with_timeout_iterative(board b, int max_depth, int time_limit,
                                    transposition_table *tt);

search_result
search_black_with_timeout_iterative(board b, int max_depth, int time_limit,
                                    transposition_table *tt);

void destroy_pv_line(pv_line *line);

// Helper macro to create an empty pv struct
#define CREATE_EMPTY_PV()                                                      \
  (&(pv){.pv_length = {0},                                                     \
         .pv_table = {{0}}})

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
    bool allow_null_move);

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
    bool allow_null_move);
