#pragma once

#include "board.h"
#include <stdio.h>

// -----------------------------------------------------------------------------
// dir

typedef enum dir {
  north,
  south,
  east,
  west,
} dir;


// -----------------------------------------------------------------------------

typedef struct move {
  u8 orig;
  u8 dest;
} move;

#define ROTATE_MOVE(_m) ((move){rotate_right[m.orig], rotate_right[m.dest]})

inline layer move_as_layer(move m) {
  layer l = EMPTY_LAYER;
  SET_INDEX(l, m.orig);
  SET_INDEX(l, m.dest);
  return l;
}

int cmp_moves(const move *a, const move *b);

#define MOVES_EQUAL(a, b) (a.orig == b.orig && a.dest == b.dest)

void moves_to(
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r,
    move *ms,
    layer *ls,
    layer *ls_r,
    int *total);

void moves_to_king_impl(
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r,
    move *ms,
    layer *ls,
    layer *ls_r,
    int *total);

typedef struct moves_to {
  move ms[335];
  layer ls[335];
  layer ls_r[335];
  int total;
} moves_to_t;

static inline moves_to_t
moves_to_black(board b, layer targets, layer targets_r) {
  moves_to_t results = {0};
  results.total = 0;
  moves_to(
      targets,
      targets_r,
      b.black,
      b.black_r,
      board_occ(b),
      board_occ_r(b),
      results.ms,
      results.ls,
      results.ls_r,
      &results.total);
  return results;
}

static inline moves_to_t
moves_to_white(board b, layer targets, layer targets_r) {
  moves_to_t results = {0};
  results.total = 0;
  moves_to(
      targets,
      targets_r,
      b.white,
      b.white_r,
      board_occ(b),
      board_occ_r(b),
      results.ms,
      results.ls,
      results.ls_r,
      &results.total);
  return results;
}

static inline moves_to_t
moves_to_king(board b, layer targets, layer targets_r) {
  moves_to_t results = {0};
  results.total = 0;
  moves_to_king_impl(
      targets,
      targets_r,
      b.king,
      b.king_r,
      king_board_occ(b),
      king_board_occ_r(b),
      results.ms,
      results.ls,
      results.ls_r,
      &results.total);
  return results;
}

int black_moves_count(const board *b);
int white_moves_count(const board *b);
int king_moves_count(const board *b);

inline move read_move(char *s) {
  int input_orig_rank;
  char input_orig_file;
  int input_dest_rank;
  char input_dest_file;
  sscanf(
      s,
      "%c%d%c%d",
      &input_orig_file,
      &input_orig_rank,
      &input_dest_file,
      &input_dest_rank);
  char orig_rank = (input_orig_rank - 1) * 11;
  char orig_file = 10 - (input_orig_file - 97);
  char orig = orig_rank + orig_file;
  char dest_rank = (input_dest_rank - 1) * 11;
  char dest_file = 10 - (input_dest_file - 97);
  char dest = dest_rank + dest_file;
  // print_move(orig, dest);
  return (move){orig, dest};
}
