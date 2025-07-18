#include "king_mobility.h"
#include "assert.h"
#include "fixtures.h"
#include "greatest.h"
#include "io.h"
#include "layer.h"
#include "move.h"
#include "stdio.h"
#include "theft.h"
#include "theft_types.h"
#include "x86intrin.h"

// -----------------------------------------------------------------------------
// corner moves 2

#define ADJACENTS ((layer){2099714ULL, 36204753239146496ULL})

void draw_horizontal(int rank, int file, int dest_file, layer *l) {
  if (dest_file > file) {
    for (int f = file + 1; f <= dest_file; f++) {
      int pos = (rank * 11) + f;
      OP_LAYER_BIT_PTR(l, pos, |=);
    }
  } else if (dest_file < file) {
    for (int f = file - 1; f >= dest_file; f--) {
      int pos = (rank * 11) + f;
      OP_LAYER_BIT_PTR(l, pos, |=);
    }
  }
}

void draw_vertical(int rank, int file, int dest_rank, layer *l) {
  if (dest_rank > rank) {
    for (int r = rank + 1; r <= dest_rank; r++) {
      int pos = (r * 11) + file;
      OP_LAYER_BIT_PTR(l, pos, |=);
    }
  } else if (dest_rank < rank) {
    for (int r = rank - 1; r >= dest_rank; r--) {
      int pos = (r * 11) + file;
      OP_LAYER_BIT_PTR(l, pos, |=);
    }
  }
}

void paths_to(
    layer occ, int rank, int file, int dest_rank, int dest_file, layer *l) {
  // vertical then horizontal
  {
    layer candidate = EMPTY_LAYER;
    draw_vertical(rank, file, dest_rank, &candidate);
    if ((rank != 0 && rank != 10 && file != 0 && file != 10) ||
        IS_EMPTY(LAYER_AND(candidate, ADJACENTS))) {
      if ((dest_rank != 0 && dest_rank != 10) || (file != 1 && file != 9)) {
	draw_horizontal(dest_rank, file, dest_file, &candidate);
      }
      if (IS_EMPTY(LAYER_AND(candidate, occ))) {
        LAYER_OR_ASSG_PTR(l, candidate);
        LAYER_OR_ASSG_PTR(l, candidate);
      }
    }
  }

  // horizontal then vertical
  {
    layer candidate = EMPTY_LAYER;
    draw_horizontal(rank, file, dest_file, &candidate);
    if ((rank != 0 && rank != 10 && file != 0 && file != 10) ||
        IS_EMPTY(LAYER_AND(candidate, ADJACENTS))) {
      if ((dest_file != 0 && dest_file != 10) || (rank != 1 && rank != 9)) {
	draw_vertical(rank, dest_file, dest_rank, &candidate);
      }
      if (IS_EMPTY(LAYER_AND(candidate, occ))) {
        LAYER_OR_ASSG_PTR(l, candidate);
      }
    }
  }
}

/* A reference implementation of corner_moves_2 which is obviously correct but
 * very slow */
void corner_paths_2_ref(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r) {

  paths_to(occ, rank, file, 0, 1, paths);
  paths_to(occ, rank, file, 0, 9, paths);
  paths_to(occ, rank, file, 1, 0, paths);
  paths_to(occ, rank, file, 1, 10, paths);
  paths_to(occ, rank, file, 9, 0, paths);
  paths_to(occ, rank, file, 9, 10, paths);
  paths_to(occ, rank, file, 10, 1, paths);
  paths_to(occ, rank, file, 10, 9, paths);
  *paths_r = rotate_layer_right(*paths);

  // east
  // edge left
  // edge right
  // adjacent left
  // adjacent right

  // west
  // edge left
  // edge right
  // adjacent left
  // adjacent right
}

struct layer_comparison {
  board b;
  layer x;
  layer x_r;
  layer y;
  layer y_r;
};

static enum theft_alloc_res
corner_paths_2_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // implementations intentionally ignore this case; the king has
  // effectively already escaped.
  if (NOT_EMPTY(LAYER_AND(ADJACENTS, b.king))) {
    return THEFT_ALLOC_SKIP;
  }

  layer x = EMPTY_LAYER;
  layer x_r = EMPTY_LAYER;
  layer y = EMPTY_LAYER;
  layer y_r = EMPTY_LAYER;

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  layer occ = LAYER_OR(b.white, b.black);
  layer occ_r = LAYER_OR(b.white_r, b.black_r);

  corner_paths_2(occ, occ_r, king_rank, king_file, &x, &x_r);
  corner_paths_2_ref(
      board_occ(b), board_occ_r(b), king_rank, king_file, &y, &y_r);

  struct layer_comparison c = {b, x, x_r, y, y_r};

  struct layer_comparison *output = malloc(sizeof(c));
  *output = c;
  *instance = output;

  return THEFT_ALLOC_OK;
};

static enum theft_trial_res prop_layers_equal(struct theft *t, void *arg1) {
  struct layer_comparison *input = (struct layer_comparison *)arg1;

  if (LAYERS_EQUAL(input->x, input->y) &&
      LAYERS_EQUAL(input->x_r, input->y_r)) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

void layer_comparison_print_cb(FILE *f, const void *instance, void *env) {
  struct layer_comparison *d = (struct layer_comparison *)instance;

  // print board
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(d->b, output);
  fprintf(f, "%s", output);

  if (!LAYERS_EQUAL(d->x, d->y)) {
    layer_string l = stringify(d->x);
    fprintf(f, "unrotated:\n%s\n\n", l._);
    layer_string l2 = stringify(d->y);
    fprintf(f, "unrotated ref:\n%s\n\n", l2._);
  }

  if (!LAYERS_EQUAL(d->x_r, d->y_r)) {
    layer_string l = stringify(d->x_r);
    fprintf(f, "unrotated:\n%s\n\n", l._);
    layer_string l2 = stringify(d->y_r);
    fprintf(f, "unrotated ref:\n%s\n\n", l2._);
  }
};

TEST prop_test_corner_paths_2(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = corner_paths_2_cb,
      .free = theft_generic_free_cb,
      .print = layer_comparison_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_layers_equal,
      .type_info = {&info},
      .trials = 5000,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// corner moves 2


bool moves_to_ref(
    layer occ, int rank, int file, int dest_rank, int dest_file, layer *l) {
  // vertical then horizontal
  {
    layer candidate = EMPTY_LAYER;
    draw_vertical(rank, file, dest_rank, &candidate);
    // print_layer(occ);
    // print_layer(candidate);
    // print_layer(LAYER_OR(ADJACENTS, corners));
    // printf("rank: %d\n", rank);
    // printf("file: %d\n", file);
    // printf("file: %d\n", dest_rank);
    if (IS_EMPTY(LAYER_AND(candidate, occ))) {
      if (NOT_EMPTY(LAYER_AND(candidate, LAYER_OR(ADJACENTS, corners)))) {
        return true;
      }
      draw_horizontal(dest_rank, file, dest_file, &candidate);
      if (IS_EMPTY(LAYER_AND(candidate, occ))) {
	int i = (dest_rank * 11) + file;
	OP_LAYER_BIT_PTR(l, i, |=);
      }
    }
  }

  // horizontal then vertical
  {
    layer candidate = EMPTY_LAYER;
    draw_horizontal(rank, file, dest_file, &candidate);

    // print_layer(occ);
    // print_layer(candidate);
    // printf("rank: %d\n", rank);
    // printf("file: %d\n", file);
    // printf("file: %d\n", dest_rank);

    if (IS_EMPTY(LAYER_AND(candidate, occ))) {
      if (NOT_EMPTY(LAYER_AND(candidate, LAYER_OR(ADJACENTS, corners)))) {
	// printf("escape");
        return true;
      }
      draw_vertical(rank, dest_file, dest_rank, &candidate);
      if (IS_EMPTY(LAYER_AND(candidate, occ))) {
	int i = (rank * 11) + dest_file;
	OP_LAYER_BIT_PTR(l, i, |=);
      }
    }
  }
  return false;
}

/* A reference implementation of corner_moves_2 which is obviously correct but
 * very slow */
bool corner_moves_2_ref(
    const layer occ,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r) {

  bool escape = false;
  escape |= moves_to_ref(occ, rank, file, 0, 1, paths);
  escape |= moves_to_ref(occ, rank, file, 0, 9, paths);
  escape |= moves_to_ref(occ, rank, file, 1, 0, paths);
  escape |= moves_to_ref(occ, rank, file, 1, 10, paths);
  escape |= moves_to_ref(occ, rank, file, 9, 0, paths);
  escape |= moves_to_ref(occ, rank, file, 9, 10, paths);
  escape |= moves_to_ref(occ, rank, file, 10, 1, paths);
  escape |= moves_to_ref(occ, rank, file, 10, 9, paths);
  *paths_r = rotate_layer_right(*paths);
  return escape;
}



struct moves_2_comparison {
  board b;
  layer x;
  layer x_r;
  bool x_escape;
  layer y;
  layer y_r;
  bool y_escape;
};

static enum theft_alloc_res
corner_moves_2_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer y = EMPTY_LAYER;
  layer y_r = EMPTY_LAYER;

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  layer occ = LAYER_OR(b.white, b.black);
  layer occ_r = LAYER_OR(b.white_r, b.black_r);

  layer paths[8] = {{0}};
  layer paths_r[8] = {{0}};

  int count = 0;
  bool x_escape = corner_moves_2(occ, occ_r, king_rank, king_file, paths, paths_r, &count);
  bool y_escape = corner_moves_2_ref(occ, king_rank, king_file, &y, &y_r);

  layer x = EMPTY_LAYER;
  for (int i = 0; i < count; i++) {
    LAYER_OR_ASSG(x, paths[i]);
  }

  layer x_r = EMPTY_LAYER;
  layer combined_paths_r = EMPTY_LAYER;
  for (int i = 0; i < count; i++) {
    LAYER_OR_ASSG(x_r, paths_r[i]);
  }

  struct moves_2_comparison c = {b, x, x_r, x_escape, y, y_r, y_escape};

  struct moves_2_comparison *output = malloc(sizeof(c));
  *output = c;
  *instance = output;

  return THEFT_ALLOC_OK;
};

static enum theft_trial_res prop_moves_2_comparison_equal(struct theft *t, void *arg1) {
  struct moves_2_comparison *input = (struct moves_2_comparison *)arg1;

  if (input->x_escape != input->y_escape) {
    return THEFT_TRIAL_FAIL;
  } else if (input->x_escape) {
    return THEFT_TRIAL_PASS;
  } else if (LAYERS_EQUAL(input->x, input->y) &&
      LAYERS_EQUAL(input->x_r, input->y_r)) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

void moves_2_comparison_print_cb(FILE *f, const void *instance, void *env) {
  struct moves_2_comparison *input = (struct moves_2_comparison *)instance;

  // print board
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(input->b, output);
  fprintf(f, "%s", output);


  if (input->x_escape != input->y_escape) {
    fprintf(f, "actual escape: %d\n", input->x_escape);
    fprintf(f, "ref escape: %d\n", input->y_escape);
    return;
  }

  if (!LAYERS_EQUAL(input->x, input->y)) {
    layer_string l = stringify(input->x);
    fprintf(f, "unrotated:\n%s\n\n", l._);
    layer_string l2 = stringify(input->y);
    fprintf(f, "unrotated ref:\n%s\n\n", l2._);
  }

  if (!LAYERS_EQUAL(input->x_r, input->y_r)) {
    layer_string l = stringify(input->x_r);
    fprintf(f, "rotated:\n%s\n\n", l._);
    layer_string l2 = stringify(input->y_r);
    fprintf(f, "rotated ref:\n%s\n\n", l2._);
  }
};

TEST prop_test_corner_moves_2(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = corner_moves_2_cb,
      .free = theft_generic_free_cb,
      .print = moves_2_comparison_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_2_comparison_equal,
      .type_info = {&info},
      .trials = 500,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------

TEST test_corner_moves_1(const char *b, bool should_escape) {
  layer occ = read_layer(b, 'X');
  layer occ_r = rotate_layer_right(occ);
  layer l = read_layer(b, '#');
  int king_pos = LOWEST_INDEX(l);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);
  bool escape = corner_moves_1(occ, occ_r, king_rank, king_file);
  ASSERT(escape == should_escape);
  PASS();
}

SUITE(corner_moves_1_suite) {
  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  #  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  #  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  X  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  X  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  X  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  X  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);
}

TEST test_corner_paths_1(const char *b, const char *e) {
  layer expected = read_layer(e, 'X');
  layer expected_r = rotate_layer_right(expected);

  // setup to generate layers
  layer occ = read_layer(b, 'X');
  layer occ_r = rotate_layer_right(occ);

  layer l = read_layer(b, '#');
  int king_pos = LOWEST_INDEX(l);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  int count = 0;
  layer paths = EMPTY_LAYER;
  layer paths_r = EMPTY_LAYER;

  // generate layers
  corner_paths_1(occ, occ_r, king_rank, king_file, &paths, &paths_r);

  {
    // error string
    layer_string result_string = stringify(paths);
    layer_string expected_string = stringify(expected);
    ASSERT_STR_EQ(expected_string._, result_string._);
  }

  {
    // error string r
    layer_string result_string = stringify(paths_r);
    layer_string expected_string = stringify(expected_r);
    ASSERT_STR_EQ(expected_string._, result_string._);
  }

  PASS();
}

SUITE(corner_paths_1_suite) {
  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  X  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  X  X  X  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  X  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  X  X  X  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  X  X  X  X  .  X  X  X  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  X  X  X  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  .  X  X  X  X  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  X  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  X  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  X  X  X  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  X  X  X  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  X  X  X  .  X  X  X  X  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  X  X  X  X"
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  .  X  X  X  X  X"
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_1,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .");
}

TEST test_corner_paths_2(const char *b, const char *e) {
  layer expected = read_layer(e, 'X');
  layer expected_r = rotate_layer_right(expected);

  // setup to generate layers
  layer occ = read_layer(b, 'X');
  layer occ_r = rotate_layer_right(occ);

  layer l = read_layer(b, '#');
  int king_pos = LOWEST_INDEX(l);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  int count = 0;
  layer paths = EMPTY_LAYER;
  layer paths_r = EMPTY_LAYER;

  // generate layers
  corner_paths_2(occ, occ_r, king_rank, king_file, &paths, &paths_r);

  {
    // error string
    layer_string result_string = stringify(paths);
    layer_string expected_string = stringify(expected);
    ASSERT_STR_EQ(expected_string._, result_string._);
  }

  {
    // error string r
    layer_string result_string = stringify(paths_r);
    layer_string expected_string = stringify(expected_r);
    ASSERT_STR_EQ(expected_string._, result_string._);
  }

  PASS();
}

TEST test_corner_moves_2(const char *b, const char *e, bool should_escape) {
  layer expected = read_layer(e, 'X');
  layer expected_r = rotate_layer_right(expected);

  // setup to generate layers
  layer occ = read_layer(b, 'X');
  layer occ_r = rotate_layer_right(occ);

  layer l = read_layer(b, '#');
  int king_pos = LOWEST_INDEX(l);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  int count = 0;
  layer paths[8] = {{0}};
  layer paths_r[8] = {{0}};

  // generate layers
  bool escape =
      corner_moves_2(occ, occ_r, king_rank, king_file, paths, paths_r, &count);

  layer combined_paths = EMPTY_LAYER;
  for (int i = 0; i < count; i++) {
    LAYER_OR_ASSG(combined_paths, paths[i]);
  }

  layer combined_paths_r = EMPTY_LAYER;
  for (int i = 0; i < count; i++) {
    LAYER_OR_ASSG(combined_paths_r, paths_r[i]);
  }

  if (should_escape) {
    if (should_escape != escape) {
      board board = read_board(b);
      print_board(board);
      ASSERT(escape);
    }
  } else {
    {
      // error string
      layer_string result_string = stringify(combined_paths);
      layer_string expected_string = stringify(expected);
      ASSERT_STR_EQ(expected_string._, result_string._);
    }

    {
      // error string r
      layer_string result_string = stringify(combined_paths_r);
      layer_string expected_string = stringify(expected_r);
      ASSERT_STR_EQ(expected_string._, result_string._);
    }
  }

  PASS();
}

SUITE(corner_paths_2_suite) {

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  .",
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      "X  X  X  #  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  X  #  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  #  X  X  X  X  X  X  X"
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  #  X  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  X  X  X  X  X  X  X  ."
      "X  X  X  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  #  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  X  #  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  #  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      "X  X  X  X  .  .  .  .  .  .  ."
      ".  .  .  X  X  X  X  X  X  X  .");

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .");

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .");

  // edge west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  X  .  .  .  .  .  .  X  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  // edge west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "#  .  X  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .");

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  .");

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  .");

  // edge east is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  X  .  #"
      ".  X  .  .  .  .  .  .  .  X  ."
      "X  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  // edge west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  X  .  .  .  .  .  X  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  .  .  .  .  .  .  ."
      ".  X  X  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  #  .  .  .  .  .",

      ".  .  .  .  .  X  X  X  X  X  ."
      "X  X  X  X  X  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      ".  .  .  .  .  .  X  X  X  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  X  X  .  .  .  .  .  .  .  ."
      "X  X  X  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  X  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  X  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      "X  X  X  X  X  X  .  .  .  .  ."
      ".  .  .  .  .  X  X  X  X  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  .  .  .  .  .  .  .  ."
      ".  X  X  X  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  X  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  X  X  X  X  X  ."
      "X  X  X  X  X  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  .  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  X  X  X  .  .  .  .  .  .  ."
      "X  X  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  #  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  X  .  .  .  .  .  .  .  .",

      ".  X  X  X  X  X  X  X  X  X  ."
      ".  .  .  .  .  .  X  X  X  X  X"
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      ".  .  .  .  .  X  .  .  .  X  ."
      "X  X  X  X  X  X  .  .  .  X  ."
      ".  .  .  .  .  X  X  X  X  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      "X  X  X  X  X  X  X  X  X  .  X"
      ".  X  .  .  .  .  .  .  .  X  .");

  RUN_TESTp(
      test_corner_paths_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  #  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      "X  X  X  X  X  X  X  .  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  .");


  RUN_TEST(prop_test_corner_paths_2);
}

SUITE(corner_moves_2_suite) {

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  #  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  X  #  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  #  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_2,
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  #  X  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  #  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      false);

  RUN_TESTp(
      test_corner_moves_2,
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  X  #  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  #  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  .",
      false);

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .",
      true);

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  #  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .",
      true);

  // edge west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "#  .  X  .  .  .  .  .  .  X  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  // edge west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "#  .  X  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  .",
      true);

  // adjacent west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  #  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  .",
      true);

  // edge east is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  X  .  #"
      ".  X  .  .  .  .  .  .  .  X  ."
      "X  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  // edge west is handled by both_sides_gen_stem in the file direction
  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  X  .  .  .  .  .  X  .  #"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  #  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  X  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  X  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  .",

      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  X  #  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  X  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  #  X  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",

      ".  .  .  X  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      true);

  RUN_TESTp(
      test_corner_moves_2,
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  #  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  X  .  .  .  .  .  .  .  .",

      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  .",
      true);

  RUN_TEST(prop_test_corner_moves_2);
}
