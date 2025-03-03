#include "move.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "theft.h"

bool board_rotation_correct(board b) {
  bool res = true;

  layer black_unrotated = rotate_layer_left(b.black_r);
  layer white_unrotated = rotate_layer_left(b.white_r);
  layer king_unrotated = rotate_layer_left(b.king_r);

  if (!(LAYERS_EQUAL(b.black, black_unrotated))) {
    layer diff = layer_xor(b.black, black_unrotated);
    print_layer(diff);
    res = false;
  }

  if (!(LAYERS_EQUAL(b.white, white_unrotated))) {
    layer diff = layer_xor(b.white, white_unrotated);
    print_layer(diff);
    res = false;
  }

  if (!(LAYERS_EQUAL(b.king, king_unrotated))) {
    layer diff = layer_xor(b.king, king_unrotated);
    print_layer(diff);
    res = false;
  }

  return res;
}

void test_start_board_moves() {
  const board start_board = read_board(start_board_string);

  board bs[235];
  move ms[235];
  int total = 0;
  uint8_t cap_counts[235] = {0};

  get_team_moves_black(start_board, &total, ms, cap_counts, bs);

  for (int i; i < total; i++) {
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.white, bs[i].white);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }

  total = 0;
  get_team_moves_white(start_board, &total, ms, cap_counts, bs);

  for (int i; i < total; i++) {
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.black, bs[i].black);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }
}

uint64_t theft_random_choice_between(struct theft *t, uint64_t floor,
                                     uint64_t ceil) {
  uint64_t res = 0;
  do {
    ceil -= res;
    res += theft_random_choice(t, ceil);
  } while (res < floor);
  return res;
}

board theft_create_board(struct theft *t) {
  layer occ = corners;
  // set throne in occ
  op_layer_bit(occ, 60, |=);

  layer black = EMPTY_LAYER;
  uint64_t black_count = theft_random_choice_between(t, 1, 25);
  while (black_count) {
    uint64_t index = theft_random_choice(t, 120);
    if (check_index(occ, index)) {
      continue;
    }
    op_layer_bit(black, index, |=);
    op_layer_bit(occ, index, |=);
    black_count--;
  }
  layer black_r = rotate_layer_right(black);

  layer white = EMPTY_LAYER;
  uint64_t white_count = theft_random_choice_between(t, 1, 12);
  while (white_count) {
    uint64_t index = theft_random_choice(t, 120);
    if (check_index(occ, index)) {
      continue;
    }
    op_layer_bit(white, index, |=);
    op_layer_bit(occ, index, |=);
    white_count--;
  }
  layer white_r = rotate_layer_right(white);

  // unset throne in occ
  op_layer_bit(occ, 60, |=);
  layer king = EMPTY_LAYER;
  while (true) {
    uint64_t index = theft_random_choice(t, 120);
    if (check_index(occ, index)) {
      continue;
    }
    op_layer_bit(king, index, |=);
    break;
  }
  layer king_r = rotate_layer_right(king);

  return (board){black, black_r, white, white_r, king, king_r};
}

static enum theft_alloc_res create_board_cb(struct theft *t, void *env,
                                            void **instance) {
  board generated = theft_create_board(t);
  board *b = calloc(1, sizeof(*b));
  *b = generated;
  *instance = b;
  return THEFT_ALLOC_OK;
};

void board_print_cb(FILE *f, const void *instance, void *env) {
  board *b = (board *)instance;
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(*b, output);
  fprintf(f, "%s", output);
};

static struct theft_type_info board_info = {
    .alloc = create_board_cb,
    .free = theft_generic_free_cb,
    .print = board_print_cb,
    .autoshrink_config =
        {
            .enable = true,
        },
};

static enum theft_trial_res prop_board_printable(struct theft *t, void *arg1) {
  board *input = (board *)arg1;
  // [compress & uncompress input, compare output & original input]
  // return THEFT_TRIAL_PASS, FAIL, SKIP, or ERROR
  printf("\n");
  print_board(*input);
  return THEFT_TRIAL_PASS;
}

bool test_board_printable(void) {
  /* Get a seed based on the current time */
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_board_printable,
      .type_info = {&board_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);
  return res == THEFT_RUN_PASS;
}

struct move_set {
  move m;
  board b;
};

// we allow double the maximum valid count of moves in each direction
// to leave space for erroneous extra moves.
struct dir_moves {
  int north_count;
  struct move_set north[20];
  int east_count;
  struct move_set east[20];
  int south_count;
  struct move_set south[20];
  int west_count;
  struct move_set west[20];
};

typedef struct dir_moves move_breakdown[120];

struct move_breakdown_diff {
  board b;
  move_breakdown missing;
  move_breakdown extra;
};

void reference_dir_moves_black(board b, layer occ, int i, move_breakdown r,
                               int dir) {
  int pos = i;
  int rank;
  int file;
  while (true) {
    pos += dir;
    int rank = pos / 11;
    int file = pos % 11;
    if (rank < 0 || rank > 10 || file < 0 || file > 10)
      break;
    if (check_index(occ, pos))
      break;
    r[i].north[r[i].north_count].m = (move){i, pos};
    board b2 = b;
    op_layer_bit(b.black, i, |=);
    op_layer_bit(b.black, rotate_right[i], |=);
    op_layer_bit(b.black, pos, |=);
    op_layer_bit(b.black, rotate_right[pos], |=);
    apply_captures_niave(b2.black, &b2.white, &b2.white_r, pos);
    r[i].north[r[i].north_count].b = b2;
    r[i].north_count++;
  }
}

void gen_reference_move_breakdown_black(board b, move_breakdown r) {
  layer occ = board_occ(b);

  int pos;
  for (int i = 0; i < 121; i++) {
    // north
    reference_dir_moves_black(b, occ, i, r, 11);
    // east
    reference_dir_moves_black(b, occ, i, r, -1);
    // south
    reference_dir_moves_black(b, occ, i, r, -11);
    // west
    reference_dir_moves_black(b, occ, i, r, 1);
  }
}

// TODO: optimize this.
void get_move_set_diff(int expected_count, struct move_set expected[20],
                       int actual_count, struct move_set actual[20],
                       int *missing_count, struct move_set missing[20],
                       int *extra_count, struct move_set extra[20]) {
  for (int i = 0; i < expected_count; i++) {
    bool isPresent = false;
    for (int j = 0; j < actual_count; j++) {
      if (moves_equal(expected[i].m, actual[j].m)) {
        isPresent = true;
        break;
      }
      if (!isPresent) {
        missing[*missing_count] = expected[i];
        (*missing_count)++;
      }
    }
  }

  for (int i = 0; i < actual_count; i++) {
    bool isExtra = true;
    for (int j = 0; j < expected_count; j++) {
      if (moves_equal(actual[i].m, expected[j].m)) {
        isExtra = false;
        break;
      }
      if (isExtra) {
        extra[*extra_count] = actual[i];
        (*extra_count)++;
      }
    }
  }
}

struct move_breakdown_diff get_move_breakdown_diff(board b,
                                                   move_breakdown expected,
                                                   move_breakdown actual) {
  struct move_breakdown_diff diff = {0};
  for (int i = 0; i < 121; i++) {
    get_move_set_diff(expected[i].north_count, expected[i].north,
                      actual[i].north_count, actual[i].north,
                      &diff.missing[i].north_count, diff.missing[i].north,
                      &diff.extra[i].north_count, diff.extra[i].north);
    get_move_set_diff(expected[i].east_count, expected[i].east,
                      actual[i].east_count, actual[i].east,
                      &diff.missing[i].east_count, diff.missing[i].east,
                      &diff.extra[i].east_count, diff.extra[i].east);
    get_move_set_diff(expected[i].south_count, expected[i].south,
                      actual[i].south_count, actual[i].south,
                      &diff.missing[i].south_count, diff.missing[i].south,
                      &diff.extra[i].south_count, diff.extra[i].south);
    get_move_set_diff(expected[i].west_count, expected[i].west,
                      actual[i].west_count, actual[i].west,
                      &diff.missing[i].west_count, diff.missing[i].west,
                      &diff.extra[i].west_count, diff.extra[i].west);
  }
  return diff;
};

void get_team_moves_black_move_breakdown(board b, move_breakdown r) {
  board bs[235];
  move ms[235];
  int total = 0;
  uint8_t cap_counts[235] = {0};

  get_team_moves_black(b, &total, ms, cap_counts, bs);

  for (int i = 0; i < total; i++) {
    move m = ms[i];
    board b = bs[i];
    if (m.dest > (m.orig + 10)) {
      r[m.orig].north[r[m.orig].north_count] = (struct move_set){m, b};
      r[m.orig].north_count++;
    } else if (m.dest > m.orig) {
      r[m.orig].west[r[m.orig].west_count] = (struct move_set){m, b};
      r[m.orig].west_count++;
    } else if (m.dest < (m.orig - 10)) {
      r[m.orig].south[r[m.orig].south_count] = (struct move_set){m, b};
      r[m.orig].south_count++;
    } else if (m.dest < m.orig) {
      r[m.orig].east[r[m.orig].east_count] = (struct move_set){m, b};
      r[m.orig].east_count++;
    }
  }
}



int main() {
  // Setup
  init_move_globals();

  // test_start_board_moves();
  test_board_printable();
}
