#include "transposition_table.h"
#include "search.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Mate score adjustment.
// Mate scores are stored relative to the position (distance-to-mate),
// not relative to the root. Adjust by ply when storing and probing.

static i32 score_to_tt(i32 score, int ply) {
  if (score > INEVITABLE_VICTORY_BASE)
    return score + ply;
  if (score < INEVITABLE_LOSS_BASE)
    return score - ply;
  return score;
}

static i32 score_from_tt(i32 score, int ply) {
  if (score > INEVITABLE_VICTORY_BASE)
    return score - ply;
  if (score < INEVITABLE_LOSS_BASE)
    return score + ply;
  return score;
}

// -----------------------------------------------------------------------------
// Replacement priority

static int age_distance(u8 current, u8 entry) {
  return (current - entry + 64) & 0x3F;
}

static int replacement_priority(tt_entry *entry, u8 current_gen) {
  return (int)entry->depth
         - 4
         * age_distance(current_gen, entry->flags.generation);
}

// Round down to power of 2 (isolate highest set bit)
static size_t round_down_pow2(size_t v) {
  if (v == 0)
    return 0;
  return 1ULL << (63 - __builtin_clzll(v));
}

// -----------------------------------------------------------------------------
// Lifecycle

transposition_table *tt_create(size_t size_mb) {
  transposition_table *tt = malloc(sizeof(transposition_table));
  if (!tt) {
    fprintf(stderr, "Failed to allocate transposition table struct\n");
    exit(1);
  }

  size_t total_bytes = size_mb * 1024 * 1024;
  size_t raw_buckets = total_bytes / sizeof(tt_bucket);
  size_t num_buckets = round_down_pow2(raw_buckets);

  if (num_buckets == 0) {
    fprintf(stderr, "Transposition table size too small\n");
    exit(1);
  }

  size_t alloc_size = num_buckets * sizeof(tt_bucket);
  tt->buckets = aligned_alloc(64, alloc_size);
  if (!tt->buckets) {
    fprintf(
        stderr,
        "Failed to allocate %zuMB for transposition table\n",
        alloc_size / (1024 * 1024));
    exit(1);
  }

  memset(tt->buckets, 0, alloc_size);
  tt->num_buckets = num_buckets;
  tt->mask = num_buckets - 1;
  tt->generation = 0;

  return tt;
}

void tt_destroy(transposition_table *tt) {
  if (tt) {
    free(tt->buckets);
    free(tt);
  }
}

void tt_clear(transposition_table *tt) {
  memset(tt->buckets, 0, tt->num_buckets * sizeof(tt_bucket));
  tt->generation = 0;
}

void tt_new_generation(transposition_table *tt) {
  // Wrap at 64 to fit in the 6-bit generation field in tt_entry.flags
  tt->generation = (tt->generation + 1) & 0x3F;
}

// -----------------------------------------------------------------------------
// Store

void tt_store(
    transposition_table *tt,
    u64 hash,
    i32 score,
    tt_node_type node_type,
    u8 depth,
    move best_move,
    int ply) {
  size_t index = hash & tt->mask;
  u32 key = (u32)(hash >> 32);
  tt_bucket *bucket = &tt->buckets[index];

  // Find the best slot: prefer same-key match, then empty, then lowest priority
  int victim = 0;
  int lowest_priority =
      replacement_priority(&bucket->entries[0], tt->generation);

  for (int i = 0; i < 4; i++) {
    tt_entry *e = &bucket->entries[i];

    // Same position: always overwrite
    if (e->key == key) {
      victim = i;
      break;
    }

    // Empty slot
    if (e->key == 0 && e->depth == 0) {
      victim = i;
      break;
    }

    // Track lowest priority for replacement
    int priority = replacement_priority(e, tt->generation);
    if (priority < lowest_priority) {
      lowest_priority = priority;
      victim = i;
    }
  }

  // do the actual update
  tt_entry *target = &bucket->entries[victim];
  target->key = key;
  target->best_move = best_move;
  target->depth = depth;
  target->flags.node_type = node_type;
  target->flags.generation = tt->generation;
  target->score = score_to_tt(score, ply);
  target->_padding = 0;
}

// -----------------------------------------------------------------------------
// Probe

bool tt_probe(
    transposition_table *tt,
    u64 hash,
    u8 depth,
    i32 alpha,
    i32 beta,
    i32 *score,
    move *best_move,
    int ply) {
  size_t index = hash & tt->mask;
  u32 key = (u32)(hash >> 32);
  tt_bucket *bucket = &tt->buckets[index];

  for (int i = 0; i < 4; i++) {
    tt_entry *e = &bucket->entries[i];

    if (e->key != key)
      continue;

    // Key match — always provide best move for ordering
    *best_move = e->best_move;

    // Check if depth is sufficient for a score cutoff
    if (e->depth < depth)
      return false;

    i32 adjusted_score = score_from_tt(e->score, ply);

    switch (e->flags.node_type) {
    case TT_EXACT:
      *score = adjusted_score;
      return true;
    case TT_LOWER_BOUND:
      if (adjusted_score >= beta) {
        *score = adjusted_score;
        return true;
      }
      return false;
    case TT_UPPER_BOUND:
      if (adjusted_score <= alpha) {
        *score = adjusted_score;
        return true;
      }
      return false;
    }
  }

  // No matching entry found
  best_move->orig = 0;
  best_move->dest = 0;
  return false;
}
