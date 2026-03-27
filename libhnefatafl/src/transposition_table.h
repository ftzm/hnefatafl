#pragma once

#include "types.h"
#include <stdbool.h>
#include <stddef.h>

typedef enum {
  TT_EXACT = 0,
  TT_LOWER_BOUND = 1,
  TT_UPPER_BOUND = 2,
} tt_node_type;

// 16 bytes, which means 4 per 64-byte cache line
typedef struct tt_entry {
  u32 key;        // upper 32 bits of zobrist hash (verification)
  move best_move; // 2 bytes (u8 orig, u8 dest)
  u8 depth;       // search depth remaining
  struct {
    u8 node_type : 2;  // TT_EXACT, TT_LOWER_BOUND, TT_UPPER_BOUND
    u8 generation : 6; // 0-63, wraps; age difference used for replacement
  } flags;
  i32 score; // evaluation score (ply-adjusted for mates)
  // TODO: Convert to i32 static_eval once we implement pruning routines
  // (null move pruning, futility pruning) that use static eval at non-terminal
  // nodes. Store the static eval when storing entries, retrieve it on probe to
  // avoid recomputing for positions we've seen before.
  u32 _padding; // pad to 16 bytes (4 entries per 64-byte cache line)
} tt_entry;

// 4 entries = 64 bytes = 1 cache line
typedef struct tt_bucket {
  tt_entry entries[4];
} tt_bucket;

typedef struct transposition_table {
  tt_bucket *buckets;
  size_t num_buckets; // must be power of 2
  size_t mask;        // num_buckets - 1, for fast module on num_buckets
  u8 generation; // current search generation. Max val 63, so will eventually
                 // wrap around, but low risk of incorrect behaviour
} transposition_table;

transposition_table *tt_create(size_t size_mb);
void tt_destroy(transposition_table *tt);
void tt_clear(transposition_table *tt);
void tt_new_generation(transposition_table *tt);

void tt_store(
    transposition_table *tt,
    u64 hash,
    i32 score,
    tt_node_type node_type,
    u8 depth,
    move best_move,
    int ply);

// tt_probe returns true if the entry produces a score cutoff.
bool tt_probe(
    transposition_table *tt,
    u64 hash,
    u8 depth,
    i32 alpha,
    i32 beta,
    i32 *score,
    move *best_move,
    int ply);
