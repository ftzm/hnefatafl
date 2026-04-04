#pragma once

#include "board.h"
#include "fixtures.h"
#include "greatest.h"
#include "io.h"
#include "theft.h"
#include "theft_types.h"
#include <stdlib.h>

// ---------------------------------------------------------------------------
// Shared board allocator, printer, and type_info for theft property tests.
// Many test files need to generate random boards; this avoids duplicating
// the same callbacks in every file.

__attribute__((unused)) static enum theft_alloc_res
theft_alloc_board(struct theft *t, void *env, void **instance) {
  (void)env;
  board *b = malloc(sizeof(board));
  if (!b)
    return THEFT_ALLOC_ERROR;
  *b = theft_create_board(t);
  *instance = b;
  return THEFT_ALLOC_OK;
}

__attribute__((unused)) static void
theft_print_board(FILE *f, const void *instance, void *env) {
  (void)env;
  const board *b = (const board *)instance;
  board_string_t s = to_board_string(*b);
  fprintf(f, "%s", s._);
}

__attribute__((unused)) static struct theft_type_info theft_board_info = {
    .alloc = theft_alloc_board,
    .free = theft_generic_free_cb,
    .print = theft_print_board,
    .autoshrink_config = {.enable = false},
};

// Variant with autoshrink enabled
__attribute__((unused)) static struct theft_type_info theft_board_info_shrink = {
    .alloc = theft_alloc_board,
    .free = theft_generic_free_cb,
    .print = theft_print_board,
    .autoshrink_config = {.enable = true},
};

// ---------------------------------------------------------------------------
// PROP_TEST: macro to eliminate the boilerplate of creating a greatest TEST
// that runs a theft property test. Covers the common case of a single
// property function with a single type_info and configurable trial count.
//
// Usage:
//   PROP_TEST(test_name, prop_fn, &type_info, 1000)

#define PROP_TEST(test_name, prop_fn, type_info_ptr, trial_count)              \
  TEST test_name(void) {                                                       \
    theft_seed seed = theft_seed_of_time();                                    \
    struct theft_run_config config = {                                         \
        .name = __func__,                                                      \
        .prop1 = prop_fn,                                                      \
        .type_info = {type_info_ptr},                                          \
        .trials = trial_count,                                                 \
        .seed = seed,                                                          \
    };                                                                         \
    enum theft_run_res res = theft_run(&config);                               \
    ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);          \
    PASS();                                                                    \
  }

// Variant without explicit trial count (uses theft default)
#define PROP_TEST_DEFAULT(test_name, prop_fn, type_info_ptr)                   \
  TEST test_name(void) {                                                       \
    theft_seed seed = theft_seed_of_time();                                    \
    struct theft_run_config config = {                                         \
        .name = __func__,                                                      \
        .prop1 = prop_fn,                                                      \
        .type_info = {type_info_ptr},                                          \
        .seed = seed,                                                          \
    };                                                                         \
    enum theft_run_res res = theft_run(&config);                               \
    ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);          \
    PASS();                                                                    \
  }

// ---------------------------------------------------------------------------
// Print a move in algebraic notation to a FILE. Useful in theft print
// callbacks to report missing/extra moves.

__attribute__((unused)) static inline void
fprint_move(FILE *f, const char *label, move m) {
  struct move_string ms = fmt_move(m.orig, m.dest);
  fprintf(f, "%s: %s\n", label, ms.buf);
}
