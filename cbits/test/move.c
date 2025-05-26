#include "move.h"
#include "move_legacy.h"
#include "move_legacy_mm.h"
#include "assert.h"
#include "board.h"
#include "capture.h"
#include "greatest.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "theft.h"
#include "theft_types.h"
#include "x86intrin.h"
#include "stdint.h"
#include "fixtures.h"

uint64_t rightward_moves_lower(uint64_t gen, uint64_t pro) {
  uint64_t orig = gen;
  pro &= 18428720874809981951ULL;
  gen |= pro & (gen >> 1);
  pro &= (pro >> 1);
  gen |= pro & (gen >> 2);
  pro &= (pro >> 2);
  gen |= pro & (gen >> 4);
  pro &= (pro >> 4);
  gen |= pro & (gen >> 8);
  return gen ^ orig;
}

TEST board_rotation_correct(board b) {
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

  get_team_moves_black(start_board, &total, ms, bs);

  for (int i; i < total; i++) {
    /*
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.white, bs[i].white);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
    */
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }

  total = 0;
  get_team_moves_white(start_board, &total, ms, bs);

  for (int i; i < total; i++) {
    /*
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.black, bs[i].black);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
     */
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }
}

static enum theft_alloc_res
create_board_cb(struct theft *t, void *env, void **instance) {
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
  printf("\n");
  print_board(*input);
  return THEFT_TRIAL_PASS;
}

TEST test_board_printable(void) {
  /* Get a seed based on the current time */
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_board_printable,
      .type_info = {&board_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// Types for reference moves

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

typedef struct dir_moves move_breakdown[121];

struct move_breakdown_diff {
  board b;
  move_breakdown missing;
  move_breakdown extra;
};

// -----------------------------------------------------------------------------
// black

// Alternate approach
void gen_reference_moves_black(board b, int *total, move *ms, board *bs) {
  *total = 0;
  layer occ = board_occ(b);

  const layer capture_dests = find_capture_destinations(b.black, b.white, occ);

  int dest;
  int orig = 0;
  for (int rank = 0; rank < 11; rank++) {
    for (int file = 0; file < 11; file++) {
      orig++;
      if (!check_index(b.black, orig))
        continue;

      // north
      dest = orig;
      for (int north = rank + 1; north < 11; north++) {
        dest += 11;
        if (check_index(occ, dest))
          break;

        board b2 = b;
        op_layer_bit(b2.black, orig, |=);
        op_layer_bit(b2.black_r, rotate_right[orig], |=);
        op_layer_bit(b2.black, dest, |=);
        op_layer_bit(b2.black_r, rotate_right[dest], |=);

        if (check_index(capture_dests, dest))
          apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

        bs[(*total)] = b2;
        ms[(*total)] = (move){orig, dest};
        (*total)++;
      }

      // south
      dest = orig;
      for (int south = rank - 1; south >= 0; south--) {
        dest -= 11;
        if (check_index(occ, dest))
          break;

        board b2 = b;
        op_layer_bit(b2.black, orig, |=);
        op_layer_bit(b2.black_r, rotate_right[orig], |=);
        op_layer_bit(b2.black, dest, |=);
        op_layer_bit(b2.black_r, rotate_right[dest], |=);

        if (check_index(capture_dests, dest))
          apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

        bs[(*total)] = b2;
        ms[(*total)] = (move){orig, dest};
        (*total)++;
      }

      // east
      dest = orig;
      for (int east = file - 1; east >= 0; east--) {
        dest -= 1;
        if (check_index(occ, dest))
          break;

        board b2 = b;
        op_layer_bit(b2.black, orig, |=);
        op_layer_bit(b2.black_r, rotate_right[orig], |=);
        op_layer_bit(b2.black, dest, |=);
        op_layer_bit(b2.black_r, rotate_right[dest], |=);

        if (check_index(capture_dests, dest))
          apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

        bs[(*total)] = b2;
        ms[(*total)] = (move){orig, dest};
        (*total)++;
      }

      // west
      dest = orig;
      for (int west = file + 1; west < 11; west++) {
        dest += 1;
        if (check_index(occ, dest))
          break;

        board b2 = b;
        op_layer_bit(b2.black, orig, |=);
        op_layer_bit(b2.black_r, rotate_right[orig], |=);
        op_layer_bit(b2.black, dest, |=);
        op_layer_bit(b2.black_r, rotate_right[dest], |=);

        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

        bs[(*total)] = b2;
        ms[(*total)] = (move){orig, dest};
        (*total)++;
      }
    }
  }
}

// Alternate approach 2
void gen_reference_moves_black2(board b, int *total, move *ms, board *bs) {
  *total = 0;
  layer occ = board_occ(b);

  const layer capture_dests = find_capture_destinations(b.black, b.white, occ);

  int dest;

  int orig = 0;
  uint64_t pieces = b.black._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = rank(orig);
    for (int north = rank + 1; north < 11; north++) {
      dest += 11;
      if (check_index(occ, dest))
        break;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // south
    dest = orig;
    rank = rank(orig);
    for (int south = rank - 1; south >= 0; south--) {
      dest -= 11;
      if (check_index(occ, dest))
        break;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // east
    dest = orig;
    int file = file(orig);
    for (int east = file - 1; east >= 0; east--) {
      dest -= 1;
      if (check_index(occ, dest))
        break;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // west
    dest = orig;
    file = file(orig);
    for (int west = file + 1; west < 11; west++) {
      dest += 1;
      if (check_index(occ, dest))
        break;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    pieces -= 1;
    pieces >>= to_next;
  }
  if (lower) {
    orig = 64;
    pieces = b.black._[1];
    lower = false;
    goto process;
  }
}

void test_start_board_moves_gen() {
  const board start_board = read_board(start_board_string);

  board bs[235];
  move ms[235];
  int total = 0;

  gen_reference_moves_black2(start_board, &total, ms, bs);

  for (int i; i < total; i++) {
    /*
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(bs[i], output);
    layer captures = layer_xor(start_board.white, bs[i].white);
    overlay_move(output, ms[i].orig, ms[i].dest, captures);
    puts(output);
    */
    bool rot_rus = board_rotation_correct(bs[i]);
    if (!rot_rus) {
      exit(1);
    }
  }

  /*
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
  */
}

void reference_dir_moves_black(
    board b,
    layer occ,
    int i,
    move_breakdown r,
    int dir,
    struct move_set *ms,
    int *cnt) {
  (*cnt) = 0;
  int pos = i;
  int rank;
  int file;
  while (true) {
    pos += dir;
    if (pos == 60)
      continue;
    int rank = pos / 11;
    int file = pos % 11;
    if (pos < 0 || pos > 120)
      break;
    if ((file == 0) && (dir == 1))
      break;
    if ((file == 10) && (dir == -1))
      break;
    if (check_index(occ, pos))
      break;
    // printf("rank: %d\n", rank);
    // printf("pos: %d\n", pos);
    // assert(pos >= 0);
    // assert(pos < 121);
    // printf("pos: %d - rank: %d - file: %d\n", pos, rank, file);
    // printf("cnt: %d\n", *cnt);
    // printf("pre m\n");
    ms[(*cnt)].m = (move){i, pos};
    char orig_notation[] = "   ";
    as_notation(i, orig_notation);
    char dest_notation[] = "   ";
    as_notation(pos, dest_notation);
    // printf("%s -> %s\n", orig_notation, dest_notation);

    board b2 = b;
    op_layer_bit(b2.black, i, |=);
    op_layer_bit(b2.black_r, rotate_right[i], |=);
    op_layer_bit(b2.black, pos, |=);
    op_layer_bit(b2.black_r, rotate_right[pos], |=);
    apply_captures_niave(b2.black, &b2.white, &b2.white_r, pos);
    // printf("pre b\n");
    ms[(*cnt)].b = b2;
    // printf("pre cnt\n");
    (*cnt)++;
  }
}

void gen_reference_move_breakdown_black(board b, move_breakdown r) {
  layer occ = board_occ(b);

  int pos;
  for (int i = 0; i < 121; i++) {

    if (!check_index(b.black, i)) {
      continue;
    }

    // north
    reference_dir_moves_black(
        b, occ, i, r, 11, (r[i].north), &(r[i].north_count));

    // east
    reference_dir_moves_black(
        b, occ, i, r, -1, (r[i].east), &(r[i].east_count));
    // south
    reference_dir_moves_black(
        b, occ, i, r, -11, (r[i].south), &(r[i].south_count));
    // west
    reference_dir_moves_black(b, occ, i, r, 1, (r[i].west), &(r[i].west_count));
  }
}

// TODO: optimize this.
void get_move_set_diff(
    int expected_count,
    struct move_set expected[20],
    int actual_count,
    struct move_set actual[20],
    int *missing_count,
    struct move_set missing[20],
    int *extra_count,
    struct move_set extra[20]) {
  if (actual_count) {
    // printf("actual count: %d\n", actual_count);
  }
  for (int i = 0; i < expected_count; i++) {
    (*missing_count) = 0;
    move expected_move = expected[i].m;
    // printf("\n");
    // printf("expected move: \n");
    // print_move(expected_move);
    // printf("\n");
    bool isPresent = false;
    for (int j = 0; j < actual_count; j++) {
      move actual_move = actual[j].m;
      // printf("actual move: \n");
      // print_move(actual_move);
      if (moves_equal(expected_move, actual_move)) {
        // printf("moves equal\n");
        isPresent = true;
        break;
      }
    }
    if (!isPresent) {
      // printf("found missing\n");
      if (*missing_count < 20) {
        missing[*missing_count] = expected[i];
        (*missing_count)++;
        // printf("missing count: %d\n", *missing_count);
      }
    }
  }

  for (int i = 0; i < actual_count; i++) {
    (*extra_count) = 0;
    bool isExtra = true;
    for (int j = 0; j < expected_count; j++) {
      if (moves_equal(actual[i].m, expected[j].m)) {
        isExtra = false;
        break;
      }
    }
    if (isExtra) {
      if (*extra_count < 20) {
        extra[*extra_count] = actual[i];
        (*extra_count)++;
      }
    }
  }
}
struct move_breakdown_diff get_move_breakdown_diff(
    board b, move_breakdown expected, move_breakdown actual) {
  struct move_breakdown_diff diff;
  memset(&diff, 0, sizeof(diff));
  diff.b = b;
  for (int i = 0; i < 121; i++) {
    get_move_set_diff(
        expected[i].north_count,
        expected[i].north,
        actual[i].north_count,
        actual[i].north,
        &diff.missing[i].north_count,
        diff.missing[i].north,
        &diff.extra[i].north_count,
        diff.extra[i].north);
    get_move_set_diff(
        expected[i].east_count,
        expected[i].east,
        actual[i].east_count,
        actual[i].east,
        &diff.missing[i].east_count,
        diff.missing[i].east,
        &diff.extra[i].east_count,
        diff.extra[i].east);

    if (check_index(b.black, i)) {
      for (int j = 0; j < actual[i].south_count; j++) {
        move m = actual[i].south[j].m;
        char orig_notation[] = "   ";
        as_notation(m.orig, orig_notation);
        char dest_notation[] = "   ";
        as_notation(m.dest, dest_notation);
        // printf("\nsouth: %s -> %s\n", orig_notation, dest_notation);
      }
    }

    get_move_set_diff(
        expected[i].south_count,
        expected[i].south,
        actual[i].south_count,
        actual[i].south,
        &diff.missing[i].south_count,
        diff.missing[i].south,
        &diff.extra[i].south_count,
        diff.extra[i].south);

    get_move_set_diff(
        expected[i].west_count,
        expected[i].west,
        actual[i].west_count,
        actual[i].west,
        &diff.missing[i].west_count,
        diff.missing[i].west,
        &diff.extra[i].west_count,
        diff.extra[i].west);
  }
  /*
   */
  return diff;
};

void get_move_breakdown(
    board b, board bs[235], move ms[235], int total, move_breakdown r) {

  for (int i = 0; i < total; i++) {
    move m = ms[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    assert(m.orig >= 0);
    assert(m.orig < 121);
    assert(m.dest >= 0);
    assert(m.dest < 121);
    assert(m.orig != m.dest);
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

void get_team_moves_black_move_breakdown(board b, move_breakdown r) {
  board bs[235];
  move ms[235];
  int total = 0;
  get_team_moves_black(b, &total, ms, bs);
  get_move_breakdown(b, bs, ms, total, r);
}

bool move_breakdown_diff_empty(struct move_breakdown_diff d) {
  for (int i = 0; i < 121; i++) {
    int totals = d.extra[i].north_count + d.extra[i].east_count +
                 d.extra[i].south_count + d.extra[i].west_count +
                 d.missing[i].north_count + d.missing[i].east_count +
                 d.missing[i].south_count + d.missing[i].west_count;
    if (totals)
      return false;
  }
  return true;
}

static enum theft_trial_res prop_diff_empty(struct theft *t, void *arg1) {
  struct move_breakdown_diff *input = (struct move_breakdown_diff *)arg1;
  if (move_breakdown_diff_empty(*input)) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

static enum theft_alloc_res
create_get_team_moves_black_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  move_breakdown reference = {0};
  gen_reference_move_breakdown_black(b, reference);
  move_breakdown actual = {0};
  get_team_moves_black_move_breakdown(b, actual);
  struct move_breakdown_diff d = get_move_breakdown_diff(b, reference, actual);

  struct move_breakdown_diff *output = calloc(1, sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

void diff_print_cb(FILE *f, const void *instance, void *env) {
  struct move_breakdown_diff *d = (struct move_breakdown_diff *)instance;

  // print board
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(d->b, output);
  fprintf(f, "%s", output);

  // diff
  for (int i = 0; i < 121; i++) {
    for (int j = 0; j < d->extra[i].north_count; j++) {
      move m = d->extra[i].north[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "extra north: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->extra[i].east_count; j++) {
      move m = d->extra[i].east[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "extra east: %s -> %s\n", orig_notation, dest_notation);
      fprintf(f, "extra east: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->extra[i].south_count; j++) {
      move m = d->extra[i].south[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "extra south: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->extra[i].west_count; j++) {
      move m = d->extra[i].west[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "extra west: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->missing[i].north_count; j++) {
      move m = d->missing[i].north[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "missing north: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->missing[i].east_count; j++) {
      move m = d->missing[i].east[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "missing east: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->missing[i].south_count; j++) {
      move m = d->missing[i].south[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "missing south: %s -> %s\n", orig_notation, dest_notation);
    }
    for (int j = 0; j < d->missing[i].west_count; j++) {
      move m = d->missing[i].west[j].m;
      char orig_notation[] = "   ";
      as_notation(m.orig, orig_notation);
      char dest_notation[] = "   ";
      as_notation(m.dest, dest_notation);
      fprintf(f, "missing west: %s -> %s\n", orig_notation, dest_notation);
    }
  }
};

static struct theft_type_info get_team_moves_black_info = {
    .alloc = create_get_team_moves_black_cb,
    .free = theft_generic_free_cb,
    .print = diff_print_cb,
    .autoshrink_config =
        {
            .enable = false,
        },
};

TEST test_get_team_moves_black(void) {
  /* Get a seed based on the current time */
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_diff_empty,
      .type_info = {&get_team_moves_black_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// White

void reference_dir_moves_white(
    board b,
    layer occ,
    int i,
    move_breakdown r,
    int dir,
    struct move_set *ms,
    int *cnt) {
  (*cnt) = 0;
  int pos = i;
  int rank;
  int file;
  while (true) {
    pos += dir;
    if (pos == 60)
      continue;
    int rank = pos / 11;
    int file = pos % 11;
    if (pos < 0 || pos > 120)
      break;
    if ((file == 0) && (dir == 1))
      break;
    if ((file == 10) && (dir == -1))
      break;
    if (check_index(occ, pos))
      break;
    // printf("rank: %d\n", rank);
    // printf("pos: %d\n", pos);
    // assert(pos >= 0);
    // assert(pos < 121);
    // printf("pos: %d - rank: %d - file: %d\n", pos, rank, file);
    // printf("cnt: %d\n", *cnt);
    // printf("pre m\n");
    ms[(*cnt)].m = (move){i, pos};
    char orig_notation[] = "   ";
    as_notation(i, orig_notation);
    char dest_notation[] = "   ";
    as_notation(pos, dest_notation);
    // printf("%s -> %s\n", orig_notation, dest_notation);

    board b2 = b;
    op_layer_bit(b2.white, i, |=);
    op_layer_bit(b2.white_r, rotate_right[i], |=);
    op_layer_bit(b2.white, pos, |=);
    op_layer_bit(b2.white_r, rotate_right[pos], |=);
    apply_captures_niave(b2.white, &b2.black, &b2.black_r, pos);
    // printf("pre b\n");
    ms[(*cnt)].b = b2;
    // printf("pre cnt\n");
    (*cnt)++;
  }
}

void gen_reference_move_breakdown_white(board b, move_breakdown r) {
  layer occ = board_occ(b);

  int pos;
  for (int i = 0; i < 121; i++) {

    if (!check_index(b.white, i)) {
      continue;
    }

    // north
    reference_dir_moves_white(
        b, occ, i, r, 11, (r[i].north), &(r[i].north_count));

    // east
    reference_dir_moves_white(
        b, occ, i, r, -1, (r[i].east), &(r[i].east_count));
    // south
    reference_dir_moves_white(
        b, occ, i, r, -11, (r[i].south), &(r[i].south_count));
    // west
    reference_dir_moves_white(b, occ, i, r, 1, (r[i].west), &(r[i].west_count));
  }
}

void get_team_moves_white_move_breakdown(board b, move_breakdown r) {
  board bs[235];
  move ms[235];
  int total = 0;

  get_team_moves_white(b, &total, ms, bs);

  for (int i = 0; i < total; i++) {
    move m = ms[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    assert(m.orig >= 0);
    assert(m.orig < 121);
    assert(m.dest >= 0);
    assert(m.dest < 121);
    assert(m.orig != m.dest);
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

static enum theft_alloc_res
create_get_team_moves_white_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  move_breakdown reference = {0};
  gen_reference_move_breakdown_white(b, reference);
  move_breakdown actual = {0};
  get_team_moves_white_move_breakdown(b, actual);
  struct move_breakdown_diff d = get_move_breakdown_diff(b, reference, actual);

  struct move_breakdown_diff *output = calloc(1, sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

static struct theft_type_info get_team_moves_white_info = {
    .alloc = create_get_team_moves_white_cb,
    .free = theft_generic_free_cb,
    .print = diff_print_cb,
    .autoshrink_config =
        {
            .enable = false,
        },
};

TEST test_get_team_moves_white(void) {
  /* Get a seed based on the current time */
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_diff_empty,
      .type_info = {&get_team_moves_white_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// King

void reference_dir_moves_king(
    board b,
    layer occ,
    int i,
    move_breakdown r,
    int dir,
    struct move_set *ms,
    int *cnt) {
  (*cnt) = 0;
  int pos = i;
  int rank;
  int file;
  while (true) {
    pos += dir;
    int rank = pos / 11;
    int file = pos % 11;
    if (pos < 0 || pos > 120)
      break;
    if ((file == 0) && (dir == 1))
      break;
    if ((file == 10) && (dir == -1))
      break;
    if (check_index(occ, pos))
      break;
    // printf("rank: %d\n", rank);
    // printf("pos: %d\n", pos);
    // assert(pos >= 0);
    // assert(pos < 121);
    // printf("pos: %d - rank: %d - file: %d\n", pos, rank, file);
    // printf("cnt: %d\n", *cnt);
    // printf("pre m\n");
    ms[(*cnt)].m = (move){i, pos};
    char orig_notation[] = "   ";
    as_notation(i, orig_notation);
    char dest_notation[] = "   ";
    as_notation(pos, dest_notation);
    // printf("%s -> %s\n", orig_notation, dest_notation);

    board b2 = b;
    op_layer_bit(b2.king, i, |=);
    op_layer_bit(b2.king_r, rotate_right[i], |=);
    op_layer_bit(b2.king, pos, |=);
    op_layer_bit(b2.king_r, rotate_right[pos], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, pos);
    // printf("pre b\n");
    ms[(*cnt)].b = b2;
    // printf("pre cnt\n");
    (*cnt)++;
  }
}

void gen_reference_move_breakdown_king(board b, move_breakdown r) {
  layer occ = layer_xor(board_occ(b), corners);

  int pos;
  for (int i = 0; i < 121; i++) {

    if (!check_index(b.king, i)) {
      continue;
    }

    // north
    reference_dir_moves_king(
        b, occ, i, r, 11, (r[i].north), &(r[i].north_count));

    // east
    reference_dir_moves_king(b, occ, i, r, -1, (r[i].east), &(r[i].east_count));
    // south
    reference_dir_moves_king(
        b, occ, i, r, -11, (r[i].south), &(r[i].south_count));
    // west
    reference_dir_moves_king(b, occ, i, r, 1, (r[i].west), &(r[i].west_count));
  }
}

void get_team_moves_king_move_breakdown(board b, move_breakdown r) {
  board bs[235];
  move ms[235];
  int total = 0;

  get_king_moves(b, &total, ms, bs);

  for (int i = 0; i < total; i++) {
    move m = ms[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    assert(m.orig >= 0);
    assert(m.orig < 121);
    assert(m.dest >= 0);
    assert(m.dest < 121);
    assert(m.orig != m.dest);
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

static enum theft_alloc_res
create_get_team_moves_king_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  move_breakdown reference = {0};
  gen_reference_move_breakdown_king(b, reference);
  move_breakdown actual = {0};
  get_team_moves_king_move_breakdown(b, actual);
  struct move_breakdown_diff d = get_move_breakdown_diff(b, reference, actual);

  struct move_breakdown_diff *output = calloc(1, sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

static struct theft_type_info get_team_moves_king_info = {
    .alloc = create_get_team_moves_king_cb,
    .free = theft_generic_free_cb,
    .print = diff_print_cb,
    .autoshrink_config =
        {
            .enable = false,
        },
};

TEST test_get_team_moves_king(void) {
  /* Get a seed based on the current time */
  theft_seed seed = theft_seed_of_time();

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_diff_empty,
      .type_info = {&get_team_moves_king_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// Smaller simpler tests

struct moves_diffs {
  board b;
  move a_excess[235];
  int a_excess_len;
  move b_excess[235];
  int b_excess_len;
};

/* requires input lists to be sorted and unique.
 */
struct moves_diffs compare_moves(
    const move a[235], const int a_len, const move b[235], const int b_len) {
  struct moves_diffs d;
  memset(&d, 0, sizeof(d));

  int a_index = 0;
  int b_index = 0;
  d.a_excess_len = 0;
  d.b_excess_len = 0;

  // while there are elements remaining in both lists we walk through
  // both, incrementing only one if it "falls behind".
  while ((a_index < a_len) && (b_index < b_len)) {
    move move_a = a[a_index];
    move move_b = b[b_index];
    int cmp = cmp_moves(&move_a, &move_b);
    if (cmp == 0) {
      // printf("found match");
      a_index++;
      b_index++;
    } else if (cmp < 0) {
      printf("found missing in a");
      d.a_excess[d.a_excess_len] = move_a;
      d.a_excess_len++;
      a_index++;
    } else if (cmp > 0) {
      printf("found missing in b\n");
      printf("orig: %d\n", move_b.orig);
      printf("dest: %d\n", move_b.dest);
      d.b_excess[d.b_excess_len] = move_b;
      d.b_excess_len++;
      b_index++;
    }
  }

  // once one of the arrays is exhausted the remaining elements in the
  // other must be excess. We collect those here.
  while (a_index < a_len) {
    move move_a = a[a_index];
    d.a_excess[d.a_excess_len] = move_a;
    d.a_excess_len++;
    a_index++;
  }
  while (b_index < b_len) {
    move move_b = b[b_index];
    d.b_excess[d.b_excess_len] = move_b;
    d.b_excess_len++;
    b_index++;
  }

  return d;
}

void moves_diffs_print_cb(FILE *f, const void *instance, void *env) {
  struct moves_diffs *d = (struct moves_diffs *)instance;

  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(d->b, output);
  fprintf(f, "%s", output);

  for (int i = 0; i < d->a_excess_len; i++) {
    move m = d->a_excess[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    printf("missing: %s -> %s\n", orig_notation, dest_notation);
  }

  for (int i = 0; i < d->b_excess_len; i++) {
    move m = d->b_excess[i];
    char orig_notation[] = "   ";
    as_notation(m.orig, orig_notation);
    char dest_notation[] = "   ";
    as_notation(m.dest, dest_notation);
    printf("extra: %s -> %s\n", orig_notation, dest_notation);
  }
}

static enum theft_trial_res
prop_moves_diffs_empty(struct theft *t, void *arg1) {
  struct moves_diffs *input = (struct moves_diffs *)arg1;
  if (!input->a_excess_len && !input->b_excess_len) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

// -----------------------------------------------------------------------------
// test reference moves black

typedef int (*ConstCompareListElements)(const void *, const void *);

static enum theft_alloc_res
create_reference_moves_black_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // orig
  board bs[235];
  move ms[235];
  int total = 0;
  get_team_moves_black(b, &total, ms, bs);

  // to test
  board bs2[235];
  move ms2[235];
  int total2 = 0;
  gen_reference_moves_black3(b, &total2, ms2, bs2);

  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_reference_moves_black(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info get_reference_moves_black_info = {
      .alloc = create_reference_moves_black_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&get_reference_moves_black_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test reference moves white

typedef int (*ConstCompareListElements)(const void *, const void *);

static enum theft_alloc_res
create_reference_moves_white_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // orig
  board bs[235];
  move ms[235];
  int total = 0;
  get_team_moves_white(b, &total, ms, bs);

  // to test
  board bs2[235];
  move ms2[235];
  int total2 = 0;
  gen_reference_moves_white3(b, &total2, ms2, bs2);

  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_reference_moves_white(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info get_reference_moves_white_info = {
      .alloc = create_reference_moves_white_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&get_reference_moves_white_info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test mm white

static enum theft_alloc_res
create_mm_moves_white_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // orig
  board bs[235];
  move ms[235];
  dir ds[235];
  int total = 0;
  get_team_moves_white(b, &total, ms, bs);

  // to test
  board bs2[235];
  move ms2[235];
  dir ds2[235];
  int total2 = 0;

  move_map mm;
  memset(mm, 0, sizeof(mm));
  build_mm(b.white, board_occ(b), mm);
  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(board_occ(b), throne_mask));
  free._[1] &= 144115188075855871;
  gen_moves_from_mm_white(b, free, mm, ms2, ds2, bs2, &total2);

  // compare
  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_mm_moves_white(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = create_mm_moves_white_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test mm black

static enum theft_alloc_res
create_mm_moves_black_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // orig
  board bs[735];
  move ms[735];
  dir ds[735];
  int total = 0;
  get_team_moves_black(b, &total, ms, bs);

  // to test
  board bs2[735];
  move ms2[735];
  dir ds2[735];
  int total2 = 0;

  move_map mm;
  memset(mm, 0, sizeof(mm));
  build_mm(b.black, board_occ(b), mm);
  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(board_occ(b), throne_mask));
  free._[1] &= 144115188075855871;
  gen_moves_from_mm_black(b, free, mm, ms2, ds2, bs2, &total2);

  // compare
  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_mm_moves_black(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = create_mm_moves_black_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test mm black

static enum theft_alloc_res
create_mm_moves_king_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // orig
  board bs[235];
  move ms[235];
  dir ds[235];
  int total = 0;
  get_king_moves(b, &total, ms, bs);

  // to test
  board bs2[235];
  move ms2[235];
  dir ds2[235];
  int total2 = 0;

  struct move_maps mms = build_mms(b);
  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(king_board_occ(b), throne_mask));
  free._[1] &= 144115188075855871;
  uint king_pos =
      b.king._[0] ? _tzcnt_u64(b.king._[0]) : _tzcnt_u64(b.king._[1]) + 64;
  gen_moves_from_mm_king(
      b, king_pos, mms.king, mms.white, mms.black, ms2, ds2, bs2, &total2);

  // compare
  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_mm_moves_king(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = create_mm_moves_king_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&info},
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test moves_to black

typedef int (*ConstCompareListElements)(const void *, const void *);

static enum theft_alloc_res
moves_to_black_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);

  // orig
  board bs[335];
  move ms[335];
  int total = 0;
  get_team_moves_black(b, &total, ms, bs);

  // to test
  layer ls[235];
  layer ls_r[335];
  board bs2[335];
  move ms2[335];
  int total2 = 0;
  moves_to(
      layer_and(layer_neg(board_occ(b)), layer_neg(throne_mask)),
      layer_and(layer_neg(board_occ_r(b)), layer_neg(throne_mask)),
      b.black,
      b.black_r,
      board_occ(b),
      board_occ_r(b),
      ms2,
      ls,
      ls_r,
      &total2);

  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_moves_to_black(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = moves_to_black_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&info},
      .trials = 1000,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test moves_to white

static enum theft_alloc_res
moves_to_white_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);

  // orig
  board bs[335];
  move ms[335];
  int total = 0;
  get_team_moves_white(b, &total, ms, bs);

  // to test
  layer ls[235];
  layer ls_r[335];
  board bs2[335];
  move ms2[335];
  int total2 = 0;
  moves_to(
      layer_and(layer_neg(board_occ(b)), layer_neg(throne_mask)),
      layer_and(layer_neg(board_occ_r(b)), layer_neg(throne_mask)),
      b.white,
      b.white_r,
      board_occ(b),
      board_occ_r(b),
      ms2,
      ls,
      ls_r,
      &total2);

  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(ms2, total2, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, ms2, total2);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_moves_to_white(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = moves_to_white_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&info},
      .trials = 1000,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test moves_to king

static enum theft_alloc_res
moves_to_king_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  // orig
  board bs[335];
  move ms[335];
  int total = 0;
  get_king_moves(b, &total, ms, bs);

  // to test
  moves_to_t r = moves_to_king(
      b, layer_neg(king_board_occ(b)), layer_neg(king_board_occ_r(b)));

  qsort(ms, total, sizeof(move), (ConstCompareListElements)cmp_moves);
  qsort(r.ms, r.total, sizeof(move), (ConstCompareListElements)cmp_moves);

  struct moves_diffs d = compare_moves(ms, total, r.ms, r.total);
  d.b = b;

  struct moves_diffs *output = malloc(sizeof(d));
  *output = d;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_moves_to_king(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = moves_to_king_cb,
      .free = theft_generic_free_cb,
      .print = moves_diffs_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_moves_diffs_empty,
      .type_info = {&info},
      .trials = 1000,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test moves_to layers

struct move_to_entry {
  move m;
  layer l;
  layer l_r;
};

struct move_to_entries {
  struct move_to_entry entries[400];
  int len;
};

static enum theft_alloc_res
test_moves_to_layers(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(board_occ(b), throne_mask));
  free._[1] &= 144115188075855871;
  layer free_r = rotate_layer_right(free);

  layer ls[235] = {0};
  layer ls_r[335] = {0};
  board bs[335] = {0};
  move ms[335] = {0};
  int total = 0;

  moves_to(
      free,
      free_r,
      b.black,
      b.black_r,
      board_occ(b),
      board_occ_r(b),
      ms,
      ls,
      ls_r,
      &total);

  struct move_to_entries incorrect_entries = {{0}, 0};
  for (int i = 0; i < total; i++) {
    move m = ms[i];
    layer correct_layer = EMPTY_LAYER;
    op_layer_bit(correct_layer, m.orig, |=);
    op_layer_bit(correct_layer, m.dest, |=);
    layer correct_layer_r = rotate_layer_right(correct_layer);
    if (!LAYERS_EQUAL(correct_layer, ls[i]) ||
        !LAYERS_EQUAL(correct_layer_r, ls_r[i])) {
      incorrect_entries.entries[incorrect_entries.len] =
          (struct move_to_entry){m, ls[i], ls_r[i]};
      incorrect_entries.len++;
    }
  };

  struct move_to_entries *e = calloc(1, sizeof(*e));
  *e = incorrect_entries;
  *instance = e;

  return THEFT_ALLOC_OK;
}

static enum theft_trial_res move_to_entries_empty(struct theft *t, void *arg1) {
  struct move_to_entries *input = (struct move_to_entries *)arg1;
  if (!input->len) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

void move_to_entries_print_cb(FILE *f, const void *instance, void *env) {
  struct move_to_entries *input = (struct move_to_entries *)instance;

  for (int i = 0; i < input->len; i++) {
    struct move_string ms =
        fmt_move(input->entries[i].m.orig, input->entries[i].m.dest);
    layer_string l = stringify(input->entries[i].l);
    layer_string l_r = stringify(input->entries[i].l_r);
    fprintf(f, "%s\n", ms.buf);
    fprintf(f, "%s\n\n", l._);
    fprintf(f, "%s\n\n", l_r._);
    fprintf(f, "--------------------------------------\n");
  }
}

TEST test_moves_to_layers_correct(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = test_moves_to_layers,
      .free = theft_generic_free_cb,
      .print = move_to_entries_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = move_to_entries_empty,
      .type_info = {&info},
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test white_moves_count

struct move_counts {
  board b;
  int from_move_func;
  int from_move_count;
};

static enum theft_alloc_res
white_moves_count_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);

  moves_to_t r = moves_to_white(
      b,
      layer_neg(layer_or(throne_mask, board_occ(b))),
      layer_neg(layer_or(throne_mask, board_occ_r(b))));

  int move_count = white_moves_count(&b);

  struct move_counts mc = {.b = b, r.total, move_count};

  struct move_counts *output = malloc(sizeof(mc));
  *output = mc;
  *instance = output;

  return THEFT_ALLOC_OK;
};

static enum theft_trial_res
prop_move_counts_equal(struct theft *t, void *arg1) {
  struct move_counts *input = (struct move_counts *)arg1;

  if (input->from_move_func == input->from_move_count) {
    return THEFT_TRIAL_PASS;
  } else {
    return THEFT_TRIAL_FAIL;
  }
}

void move_counts_print_cb(FILE *f, const void *instance, void *env) {
  struct move_counts *input = (struct move_counts *)instance;

  // print board
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(input->b, output);
  fprintf(f, "%s\n", output);

  fprintf(f, "from_move_func: %d\n", input->from_move_func);
  fprintf(f, "from_move_count: %d\n", input->from_move_count);
}

TEST test_white_moves_count(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = white_moves_count_cb,
      .free = theft_generic_free_cb,
      .print = move_counts_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_move_counts_equal,
      .type_info = {&info},
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test black_moves_count

static enum theft_alloc_res
black_moves_count_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  layer throne_mask = EMPTY_LAYER;
  op_layer_bit(throne_mask, 60, |=);

  moves_to_t r = moves_to_black(
      b,
      layer_neg(layer_or(throne_mask, board_occ(b))),
      layer_neg(layer_or(throne_mask, board_occ_r(b))));

  int move_count = black_moves_count(&b);

  struct move_counts mc = {.b = b, r.total, move_count};

  struct move_counts *output = malloc(sizeof(mc));
  *output = mc;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_black_moves_count(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = black_moves_count_cb,
      .free = theft_generic_free_cb,
      .print = move_counts_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_move_counts_equal,
      .type_info = {&info},
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// test king_moves_count

static enum theft_alloc_res
king_moves_count_cb(struct theft *t, void *env, void **instance) {
  board b = theft_create_board(t);

  moves_to_t r = moves_to_king(
      b, layer_neg(king_board_occ(b)), layer_neg(king_board_occ_r(b)));

  int move_count = king_moves_count(&b);

  struct move_counts mc = {.b = b, r.total, move_count};

  struct move_counts *output = malloc(sizeof(mc));
  *output = mc;
  *instance = output;

  return THEFT_ALLOC_OK;
};

TEST test_king_moves_count(void) {
  theft_seed seed = theft_seed_of_time();

  static struct theft_type_info info = {
      .alloc = king_moves_count_cb,
      .free = theft_generic_free_cb,
      .print = move_counts_print_cb,
      .autoshrink_config = {.enable = false},
  };

  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_move_counts_equal,
      .type_info = {&info},
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// corner moves 2

#define ADJACENTS ((layer){2099712ULL, 36204753239146496ULL})

void draw_horizontal(int rank, int file, int dest_file, layer *l) {
  if (dest_file > file) {
    for (int f = file + 1; f <= dest_file; f++) {
      int pos = (rank * 11) + f;
      op_layer_bit_ptr(l, pos, |=);
    }
  } else if (dest_file < file) {
    for (int f = file - 1; f >= dest_file; f--) {
      int pos = (rank * 11) + f;
      op_layer_bit_ptr(l, pos, |=);
    }
  }
}

void draw_vertical(int rank, int file, int dest_rank, layer *l) {
  if (dest_rank > rank) {
    for (int r = rank + 1; r <= dest_rank; r++) {
      int pos = (r * 11) + file;
      op_layer_bit_ptr(l, pos, |=);
    }
  } else if (dest_rank < rank) {
    for (int r = rank - 1; r >= dest_rank; r--) {
      int pos = (r * 11) + file;
      op_layer_bit_ptr(l, pos, |=);
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
      draw_horizontal(dest_rank, file, dest_file, &candidate);
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
      draw_vertical(rank, dest_file, dest_rank, &candidate);
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

  layer x = EMPTY_LAYER;
  layer x_r = EMPTY_LAYER;
  layer y = EMPTY_LAYER;
  layer y_r = EMPTY_LAYER;

  int king_pos = lowest_index(b.king);
  int king_rank = rank(king_pos);
  int king_file = file(king_pos);

  layer occ = layer_or(b.white, b.black);
  layer occ_r = layer_or(b.white_r, b.black_r);

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

TEST test_corner_moves_2(void) {
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
      .trials = 100,
      .seed = seed,
  };

  enum theft_run_res res = theft_run(&config);

  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// -----------------------------------------------------------------------------
// Run

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {
  // Setup
  init_move_globals();

  GREATEST_MAIN_BEGIN();

  // RUN_TEST(test_moves_to_white);
  // RUN_TEST(test_moves_to_black);
  // RUN_TEST(test_moves_to_king);
  // RUN_TEST(test_moves_to_layers_correct);
  // RUN_TEST(test_black_moves_count);
  // RUN_TEST(test_white_moves_count);
  // RUN_TEST(test_king_moves_count);
  RUN_TEST(test_corner_moves_2);

  /*
  test_start_board_moves();
  // test_board_printable();
  test_get_team_moves_black();
  test_get_team_moves_white();
  test_get_team_moves_king();
  test_reference_moves_black();

  test_start_board_moves_gen();

  test_mm_moves_white();
  test_mm_moves_black();
  test_mm_moves_king();

  test_moves_to_black();
  test_moves_to_layers_correct();
  */

  GREATEST_MAIN_END();
}

// MAYBE TODO:
/*
more basic properties:
- moves unique
- moves only along rank and file
- moves within acceptable range
- move orig is occupied position
- move destination is unoccupied position
- do niave capture check for every move

- moves contiguous
*/
