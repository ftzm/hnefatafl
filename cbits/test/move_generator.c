#include "move_generator.h"
#include "move.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "fixtures.h"
#include "greatest.h"
#include "io.h"
#include "king_mobility.h"
#include "layer.h"
#include "move_legacy.h"
#include "move_legacy_mm.h"
#include "stdbool.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "theft.h"
#include "theft_types.h"
#include "x86intrin.h"

// -----------------------------------------------------------------------------
// test move generator

struct generator_comparison {
  board b;
  move extra_moves[335];
  int extra_count;
  move missing_moves[335];
  int missing_count;
};

typedef int (*ConstCompareListElements)(const void *, const void *);

static enum theft_alloc_res
move_generator_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  OP_LAYER_BIT(throne_mask, 60, |=);
  layer targets = LAYER_NEG(LAYER_OR(throne_mask, board_occ(b)));
  layer targets_r = LAYER_NEG(LAYER_OR(throne_mask, board_occ_r(b)));

  // Get moves using existing function
  layer ls[335];
  layer ls_r[335];
  move reference_moves[335];
  int reference_total = 0;
  moves_to(
      targets,
      targets_r,
      b.black,
      b.black_r,
      board_occ(b),
      board_occ_r(b),
      reference_moves,
      ls,
      ls_r,
      &reference_total);

  // Get moves using generator
  move generator_moves[335];
  int generator_total = 0;
  move_generator gen;
  init_move_generator(&gen, targets, targets_r, b.black, b.black_r, board_occ(b), board_occ_r(b));
  
  move current_move;
  while (next_move(&gen, &current_move)) {
    generator_moves[generator_total] = current_move;
    generator_total++;
    if (generator_total >= 335) break; // Safety check
  }

  // Sort both arrays for comparison
  qsort(reference_moves, reference_total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(generator_moves, generator_total, sizeof(move), (ConstCompareListElements)cmp_moves);

  // Find differences
  struct generator_comparison comp;
  memset(&comp, 0, sizeof(comp));
  comp.b = b;

  // Find moves in reference but not in generator (missing)
  for (int i = 0; i < reference_total; i++) {
    bool found = false;
    for (int j = 0; j < generator_total; j++) {
      if (MOVES_EQUAL(reference_moves[i], generator_moves[j])) {
        found = true;
        break;
      }
    }
    if (!found && comp.missing_count < 335) {
      comp.missing_moves[comp.missing_count] = reference_moves[i];
      comp.missing_count++;
    }
  }

  // Find moves in generator but not in reference (extra)
  for (int i = 0; i < generator_total; i++) {
    bool found = false;
    for (int j = 0; j < reference_total; j++) {
      if (MOVES_EQUAL(generator_moves[i], reference_moves[j])) {
        found = true;
        break;
      }
    }
    if (!found && comp.extra_count < 335) {
      comp.extra_moves[comp.extra_count] = generator_moves[i];
      comp.extra_count++;
    }
  }

  struct generator_comparison *output = malloc(sizeof(comp));
  *output = comp;
  *instance = output;

  return THEFT_ALLOC_OK;
}

static enum theft_trial_res
prop_generator_comparison_empty(struct theft *t, void *arg1) {
  struct generator_comparison *input = (struct generator_comparison *)arg1;
  if (input->extra_count == 0 && input->missing_count == 0) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

void generator_comparison_print_cb(FILE *f, const void *instance, void *env) {
  struct generator_comparison *comp = (struct generator_comparison *)instance;

  // Print board
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(comp->b, output);
  fprintf(f, "%s\n", output);

  // Print missing moves
  for (int i = 0; i < comp->missing_count; i++) {
    move m = comp->missing_moves[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    fprintf(f, "missing: %s -> %s\n", orig_notation, dest_notation);
  }

  // Print extra moves
  for (int i = 0; i < comp->extra_count; i++) {
    move m = comp->extra_moves[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    fprintf(f, "extra: %s -> %s\n", orig_notation, dest_notation);
  }
}

TEST test_move_generator(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = move_generator_cb,
      .free = theft_generic_free_cb,
      .print = generator_comparison_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_generator_comparison_empty,
      .type_info = {&info},
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test king move generator

static enum theft_alloc_res
king_move_generator_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer targets = LAYER_NEG(king_board_occ(b));
  layer targets_r = LAYER_NEG(king_board_occ_r(b));

  // Get moves using existing moves_to_king_impl function
  layer ls[335];
  layer ls_r[335];
  move reference_moves[335];
  int reference_total = 0;
  moves_to_king_impl(
      targets,
      targets_r,
      b.king,
      b.king_r,
      king_board_occ(b),
      king_board_occ_r(b),
      reference_moves,
      ls,
      ls_r,
      &reference_total);

  // Get moves using king generator
  move generator_moves[335];
  int generator_total = 0;
  move_generator gen;
  init_move_generator_king(&gen, targets, targets_r, b.king, b.king_r, king_board_occ(b), king_board_occ_r(b));
  
  move current_move;
  while (next_move_king(&gen, &current_move)) {
    generator_moves[generator_total] = current_move;
    generator_total++;
    if (generator_total >= 335) break; // Safety check
  }

  // Check if moves are generated in the same order (before sorting)
  bool order_matches = (reference_total == generator_total);
  if (order_matches) {
    for (int i = 0; i < reference_total; i++) {
      if (!MOVES_EQUAL(reference_moves[i], generator_moves[i])) {
        order_matches = false;
        printf("Order mismatch at index %d: ref(%d->%d) vs gen(%d->%d)\n", 
               i, reference_moves[i].orig, reference_moves[i].dest,
               generator_moves[i].orig, generator_moves[i].dest);
        break;
      }
    }
  } else {
    printf("Total count mismatch: ref=%d vs gen=%d\n", reference_total, generator_total);
  }
  
  // Sort both arrays for set comparison
  qsort(reference_moves, reference_total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(generator_moves, generator_total, sizeof(move), (ConstCompareListElements)cmp_moves);

  // Find differences
  struct generator_comparison comp;
  memset(&comp, 0, sizeof(comp));
  comp.b = b;

  // Find moves in reference but not in generator (missing)
  for (int i = 0; i < reference_total; i++) {
    bool found = false;
    for (int j = 0; j < generator_total; j++) {
      if (MOVES_EQUAL(reference_moves[i], generator_moves[j])) {
        found = true;
        break;
      }
    }
    if (!found && comp.missing_count < 335) {
      comp.missing_moves[comp.missing_count] = reference_moves[i];
      comp.missing_count++;
    }
  }

  // Find moves in generator but not in reference (extra)
  for (int i = 0; i < generator_total; i++) {
    bool found = false;
    for (int j = 0; j < reference_total; j++) {
      if (MOVES_EQUAL(generator_moves[i], reference_moves[j])) {
        found = true;
        break;
      }
    }
    if (!found && comp.extra_count < 335) {
      comp.extra_moves[comp.extra_count] = generator_moves[i];
      comp.extra_count++;
    }
  }

  struct generator_comparison *output = malloc(sizeof(comp));
  *output = comp;
  *instance = output;

  return THEFT_ALLOC_OK;
}

TEST test_king_move_generator(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = king_move_generator_cb,
      .free = theft_generic_free_cb,
      .print = generator_comparison_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_generator_comparison_empty,
      .type_info = {&info},
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

SUITE(move_generator_suite) {

  init_move_globals();

  RUN_TEST(test_move_generator);
  RUN_TEST(test_king_move_generator);
}