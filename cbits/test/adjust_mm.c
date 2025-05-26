#include "util/util.c"
#include "move.h"
#include "move_legacy_mm.h"
#include "assert.h"
#include "board.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "theft.h"
#include "theft_types.h"

struct mm_move_result {
  move m;
  board b;
  struct move_maps mm_adjusted;
  struct move_maps mm_recomputed;
};

struct mm_move_results {
  int len;
  struct mm_move_result moves[400];
};

bool move_results_equal(struct mm_move_results mm) {
  for (int i = 0; i < mm.len; i++) {
    struct mm_move_result res = mm.moves[i];
    if (memcmp(
            &res.mm_adjusted, &res.mm_recomputed, sizeof(struct move_maps))) {
      return false;
    }
  }
  return true;
}


board make_move_white(board b, move m) {
  board b2 = b;
  op_layer_bit(b2.white, m.dest, ^=);
  op_layer_bit(b2.white_r, rotate_right[m.dest], ^=);
  op_layer_bit(b2.white, m.orig, ^=);
  op_layer_bit(b2.white_r, rotate_right[m.orig], ^=);
  return b2;
}

board make_move_black(board b, move m) {
  board b2 = b;
  op_layer_bit(b2.black, m.dest, ^=);
  op_layer_bit(b2.black_r, rotate_right[m.dest], ^=);
  op_layer_bit(b2.black, m.orig, ^=);
  op_layer_bit(b2.black_r, rotate_right[m.orig], ^=);
  return b2;
}

board make_move_king(board b, move m) {
  board b2 = b;
  op_layer_bit(b2.king, m.dest, ^=);
  op_layer_bit(b2.king_r, rotate_right[m.dest], ^=);
  op_layer_bit(b2.king, m.orig, ^=);
  op_layer_bit(b2.king_r, rotate_right[m.orig], ^=);
  return b2;
}

typedef void (*apply_move_func)(
    const uint8_t, const uint8_t, move_map, move_map, move_map);


apply_move_func get_apply_move_func(enum dir d) {
  apply_move_func funcs[4];
  funcs[north] = apply_northward_move;
  funcs[south] = apply_southward_move;
  funcs[east] = apply_eastward_move;
  funcs[west] = apply_westward_move;
  return funcs[d];
}

enum team {
  black,
  white,
  king,
};

const char *team_str(enum team t) {
  const char *team_strs[] = {
      [black] = "black",
      [white] = "white",
      [king] = "king",
  };
  return team_strs[t];
}

typedef board (*make_move_func)(board, move);

make_move_func get_make_move_func(enum team t) {
  make_move_func funcs[3];
  funcs[white] = make_move_white;
  funcs[black] = make_move_black;
  funcs[king] = make_move_king;
  return funcs[t];
}


enum team next_team(enum team t) {
  const enum team teams[] = {
    [black] = white,
    [white] = king,
    [king] = black,
  };
  return teams[t];
}

struct sources *select_mm(struct move_maps *mm, enum team t) {
  if (t == black) {
    return mm->black;
  } else if (t == white) {
    return mm->white;
  } else {
    return mm->king;
  }
}

uint8_t select_dir_orig(struct sources dirs, enum dir d) {
  if (d == north) {
    return dirs.north;
  } else if (d == south) {
    return dirs.south;
  } else if (d == east) {
    return dirs.east;
  } else {
    return dirs.west;
  }
}

void test_move(
    board b,
    layer occ,
    int dest,
    struct move_maps mm,
    int *len,
    struct mm_move_results *res,
    enum team t,
    enum dir d) {
  uint8_t orig = select_dir_orig(select_mm(&mm, t)[dest], d);
  if (!check_index(occ, dest) && orig) {

    move m = {orig, dest};
    board b2 = get_make_move_func(t)(b, m);

    // adjusted mm
    struct move_maps mm2;
    memcpy(&mm2, &mm, sizeof(mm2));
    enum team t2 = next_team(t);
    enum team t3 = next_team(t2);
    get_apply_move_func(other_dir(d))(
        m.orig,
        m.dest,
        select_mm(&mm2, t),
        select_mm(&mm2, t2),
        select_mm(&mm2, t3));

    // recomputed mm
    struct move_maps mm3 = build_mms(b2);

    res->moves[*len] = (struct mm_move_result){m, b2, mm2, mm3};
    (*len)++;
  }
}

struct mm_move_results
gen_mms_moves(board b, struct move_maps mms, enum team t) {
  layer occ = board_occ(b);
  struct mm_move_results res = {0};
  for (int i = 0; i < 121; i++) {
    test_move(b, occ, i, mms, &res.len, &res, t, north);
    test_move(b, occ, i, mms, &res.len, &res, t, south);
    test_move(b, occ, i, mms, &res.len, &res, t, east);
    test_move(b, occ, i, mms, &res.len, &res, t, west);
  }
  return res;
}

static enum theft_alloc_res
gen_mm_move_adjust_white_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);
  struct move_maps mms = build_mms(b);
  struct mm_move_results *output = malloc(sizeof(struct mm_move_results));
  *output = gen_mms_moves(b, mms, white);
  *instance = output;
  return THEFT_ALLOC_OK;
}

void print_mm_move_diff(
    FILE *f, struct mm_move_result res, int dest, enum team t, enum dir d) {
  uint8_t adjusted = select_dir_orig(select_mm(&res.mm_adjusted, t)[dest], d);
  uint8_t recomputed =
      select_dir_orig(select_mm(&res.mm_recomputed, t)[dest], d);
  if (adjusted != recomputed) {
    char dest_notation[] = "   ";
    as_notation(dest, dest_notation);
    char adjusted_notation[] = "   ";
    as_notation(adjusted, adjusted_notation);
    char recomputed_notation[] = "   ";
    as_notation(recomputed, recomputed_notation);
    fprintf(
        f,
        "%s - %s -- dest: %s, recomputed: %s, adjusted: %s\n",
        team_str(t),
        dir_str(d),
        dest_notation,
        recomputed_notation,
        adjusted_notation);
  }
}

void print_mm_diff_cb(FILE *f, const void *instance, void *env) {
  struct mm_move_results *mm = (struct mm_move_results *)instance;
  for (int i = 0; i < mm->len; i++) {
    struct mm_move_result res = mm->moves[i];
    if (memcmp(
            &res.mm_adjusted, &res.mm_recomputed, sizeof(struct move_maps))) {
      print_move(res.m.orig, res.m.dest);
      printf("\n");
      print_board(res.b);
      print_board_move(res.b, res.m.orig, res.m.dest, EMPTY_LAYER);
      printf("unequal at move number: %d\n", i);
      for (int j = 0; j < 121; j++) {
        print_mm_move_diff(f, res, j, white, north);
        print_mm_move_diff(f, res, j, white, south);
        print_mm_move_diff(f, res, j, white, east);
        print_mm_move_diff(f, res, j, white, west);

        print_mm_move_diff(f, res, j, black, north);
        print_mm_move_diff(f, res, j, black, south);
        print_mm_move_diff(f, res, j, black, east);
        print_mm_move_diff(f, res, j, black, west);
      }
    }
  }
}

static enum theft_trial_res
move_results_equal_prop(struct theft *t, void *arg1) {
  struct mm_move_results *mm = (struct mm_move_results *)arg1;
  return (move_results_equal(*mm)) ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
}

static struct theft_type_info mm_adjust_white_info = {
    .alloc = gen_mm_move_adjust_white_cb,
    .free = theft_generic_free_cb,
    .print = print_mm_diff_cb,
    .autoshrink_config = {.enable = false},
};

bool test_mm_move_adjust_white(void) {
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = move_results_equal_prop,
      .type_info = {&mm_adjust_white_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);
  return res == THEFT_RUN_PASS;
}

static enum theft_alloc_res
gen_mm_move_adjust_black_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);
  struct move_maps mms = build_mms(b);
  struct mm_move_results *output = malloc(sizeof(struct mm_move_results));
  *output = gen_mms_moves(b, mms, black);
  *instance = output;
  return THEFT_ALLOC_OK;
}


static struct theft_type_info mm_adjust_black_info = {
    .alloc = gen_mm_move_adjust_black_cb,
    .free = theft_generic_free_cb,
    .print = print_mm_diff_cb,
    .autoshrink_config = {.enable = false},
};


bool test_mm_move_adjust_black(void) {
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = move_results_equal_prop,
      .type_info = {&mm_adjust_black_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);
  return res == THEFT_RUN_PASS;
}

static enum theft_alloc_res
gen_mm_move_adjust_king_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);
  struct move_maps mms = build_mms(b);
  struct mm_move_results *output = malloc(sizeof(struct mm_move_results));
  *output = gen_mms_moves(b, mms, king);
  *instance = output;
  return THEFT_ALLOC_OK;
}


static struct theft_type_info mm_adjust_king_info = {
    .alloc = gen_mm_move_adjust_king_cb,
    .free = theft_generic_free_cb,
    .print = print_mm_diff_cb,
    .autoshrink_config = {.enable = false},
};


bool test_mm_move_adjust_king(void) {
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = move_results_equal_prop,
      .type_info = {&mm_adjust_king_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);
  return res == THEFT_RUN_PASS;
}

int main() {
  test_mm_move_adjust_white();
  test_mm_move_adjust_black();
  test_mm_move_adjust_king();
}
