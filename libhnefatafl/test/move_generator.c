#include "move_generator.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "king_mobility.h"
#include "layer.h"
#include "move.h"
#include "move_legacy.h"
#include "test_util.h"
#include <string.h>

// ---------------------------------------------------------------------------
// Shared comparison infrastructure

struct generator_comparison {
  board b;
  move extra_moves[335];
  int extra_count;
  move missing_moves[335];
  int missing_count;
};

typedef int (*ConstCompareListElements)(const void *, const void *);

// Compare two sorted move arrays and populate a generator_comparison diff.
static struct generator_comparison diff_move_arrays(
    board b,
    move *ref,
    int ref_total,
    move *gen,
    int gen_total) {
  struct generator_comparison comp;
  memset(&comp, 0, sizeof(comp));
  comp.b = b;

  qsort(ref, ref_total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(gen, gen_total, sizeof(move), (ConstCompareListElements)cmp_moves);

  // Moves in reference but not in generator (missing)
  for (int i = 0; i < ref_total; i++) {
    bool found = false;
    for (int j = 0; j < gen_total; j++) {
      if (MOVES_EQUAL(ref[i], gen[j])) {
        found = true;
        break;
      }
    }
    if (!found && comp.missing_count < 335) {
      comp.missing_moves[comp.missing_count++] = ref[i];
    }
  }

  // Moves in generator but not in reference (extra)
  for (int i = 0; i < gen_total; i++) {
    bool found = false;
    for (int j = 0; j < ref_total; j++) {
      if (MOVES_EQUAL(gen[i], ref[j])) {
        found = true;
        break;
      }
    }
    if (!found && comp.extra_count < 335) {
      comp.extra_moves[comp.extra_count++] = gen[i];
    }
  }

  return comp;
}

static enum theft_trial_res
prop_generator_comparison_empty(struct theft *t, void *arg1) {
  (void)t;
  struct generator_comparison *input = (struct generator_comparison *)arg1;
  return (input->extra_count == 0 && input->missing_count == 0)
             ? THEFT_TRIAL_PASS
             : THEFT_TRIAL_FAIL;
}

static void
generator_comparison_print_cb(FILE *f, const void *instance, void *env) {
  (void)env;
  struct generator_comparison *comp = (struct generator_comparison *)instance;

  board_string_t bs = to_board_string(comp->b);
  fprintf(f, "%s\n", bs._);

  for (int i = 0; i < comp->missing_count; i++)
    fprint_move(f, "missing", comp->missing_moves[i]);
  for (int i = 0; i < comp->extra_count; i++)
    fprint_move(f, "extra", comp->extra_moves[i]);
}

// ---------------------------------------------------------------------------
// Pawn move generator

static enum theft_alloc_res
move_generator_cb(struct theft *t, void *env, void **instance) {
  (void)env;
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  OP_LAYER_BIT(throne_mask, 60, |=);
  layer targets = pawn_destinations(b);
  layer targets_r = LAYER_NEG(LAYER_OR(throne_mask, board_occ_r(b)));

  // Reference: bulk move generation
  layer ls[335];
  layer ls_r[335];
  move ref_moves[335];
  int ref_total = 0;
  moves_to(
      targets, targets_r, b.black, b.black_r, board_occ(b), board_occ_r(b),
      ref_moves, ls, ls_r, &ref_total);

  // Under test: streaming generator
  move gen_moves[335];
  int gen_total = 0;
  move_generator gen;
  init_move_generator(
      &gen, targets, targets_r, b.black, b.black_r, board_occ(b),
      board_occ_r(b));

  move current_move;
  while (next_move(&gen, &current_move)) {
    gen_moves[gen_total++] = current_move;
    if (gen_total >= 335)
      break;
  }

  struct generator_comparison comp =
      diff_move_arrays(b, ref_moves, ref_total, gen_moves, gen_total);
  struct generator_comparison *output = malloc(sizeof(comp));
  *output = comp;
  *instance = output;
  return THEFT_ALLOC_OK;
}

// ---------------------------------------------------------------------------
// King move generator

static enum theft_alloc_res
king_move_generator_cb(struct theft *t, void *env, void **instance) {
  (void)env;
  board b = theft_create_board(t);

  layer targets = king_destinations(b);
  layer targets_r = king_destinations_r(b);

  // Reference: bulk move generation
  layer ls[335];
  layer ls_r[335];
  move ref_moves[335];
  int ref_total = 0;
  moves_to_king_impl(
      targets, targets_r, b.king, b.king_r, king_board_occ(b),
      king_board_occ_r(b), ref_moves, ls, ls_r, &ref_total);

  // Under test: streaming king generator
  move gen_moves[335];
  int gen_total = 0;
  move_generator gen;
  init_move_generator_king(
      &gen, targets, targets_r, b.king, b.king_r, king_board_occ(b),
      king_board_occ_r(b));

  move current_move;
  while (next_move_king(&gen, &current_move)) {
    gen_moves[gen_total++] = current_move;
    if (gen_total >= 335)
      break;
  }

  struct generator_comparison comp =
      diff_move_arrays(b, ref_moves, ref_total, gen_moves, gen_total);
  struct generator_comparison *output = malloc(sizeof(comp));
  *output = comp;
  *instance = output;
  return THEFT_ALLOC_OK;
}

// ---------------------------------------------------------------------------
// Test definitions

static struct theft_type_info pawn_gen_info = {
    .alloc = move_generator_cb,
    .free = theft_generic_free_cb,
    .print = generator_comparison_print_cb,
    .autoshrink_config = {.enable = false},
};

static struct theft_type_info king_gen_info = {
    .alloc = king_move_generator_cb,
    .free = theft_generic_free_cb,
    .print = generator_comparison_print_cb,
    .autoshrink_config = {.enable = false},
};

PROP_TEST(test_move_generator, prop_generator_comparison_empty,
          &pawn_gen_info, 100)
PROP_TEST(test_king_move_generator, prop_generator_comparison_empty,
          &king_gen_info, 100)

SUITE(move_generator_suite) {
  init_move_globals();
  RUN_TEST(test_move_generator);
  RUN_TEST(test_king_move_generator);
}
