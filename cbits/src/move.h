#pragma once

#include "board.h"
#include "stdbool.h"
#include <stdio.h>

// -----------------------------------------------------------------------------
// dir

typedef enum dir {
  north,
  south,
  east,
  west,
} dir;

static const char *dir_str(enum dir d) {
  const char *dir_strs[] = {
      [north] = "north",
      [south] = "south",
      [east] = "east",
      [west] = "west",
  };
  return dir_strs[d];
}

static enum dir other_dir(enum dir d) {
  enum dir others[4];
  others[north] = south;
  others[south] = north;
  others[east] = west;
  others[west] = east;
  return others[d];
}

// -----------------------------------------------------------------------------

typedef struct move {
  u8 orig;
  u8 dest;
} move;

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
  moves_to_t results = {{0}};
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
  moves_to_t results = {{0}};
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
  moves_to_t results = {{0}};
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

// layer rightward_moves_layer(layer movers, layer occ);

int black_moves_count(const board *b);
int white_moves_count(const board *b);
int king_moves_count(const board *b);
