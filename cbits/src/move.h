#pragma once

#include "board.h"
#include "stdbool.h"

// -----------------------------------------------------------------------------
// dir

typedef enum dir {
  north,
  south,
  east,
  west,
} dir;

const char *dir_str(enum dir d) {
  const char *dir_strs[] = {
      [north] = "north",
      [south] = "south",
      [east] = "east",
      [west] = "west",
  };
  return dir_strs[d];
}

enum dir other_dir(enum dir d) {
  enum dir others[4];
  others[north] = south;
  others[south] = north;
  others[east] = west;
  others[west] = east;
  return others[d];
}

// -----------------------------------------------------------------------------

typedef struct move {
  uint8_t orig;
  uint8_t dest;
} move;

int cmp_moves(const move *a, const move *b);

#define moves_equal(a, b) (a.orig == b.orig && a.dest == b.dest)

void get_team_moves_black(
    const board current, int *total, move *moves, board *boards);

void get_team_moves_white(
    const board current, int *total, move *moves, board *boards);

void get_king_moves(
    const board current, int *total, move *moves, board *boards);

void init_move_globals();

layer find_capture_destinations(
    const layer allies, const layer foes, const layer occ);

void gen_reference_moves_black3(const board b, int *total, move *ms, board *bs);

void gen_reference_moves_white3(const board b, int *total, move *ms, board *bs);

struct sources {
  uint8_t north;
  uint8_t south;
  uint8_t east;
  uint8_t west;
};

typedef struct sources move_map[121];

struct move_maps {
  move_map black;
  move_map white;
  move_map king;
};

void build_mm(layer movers, const layer occ, move_map mm);

void apply_southward_move(
    uint8_t src, uint8_t dest, move_map allies, move_map foes, move_map king);

void apply_northward_move(
    uint8_t src, uint8_t dest, move_map allies, move_map foes, move_map king);

void apply_eastward_move(
    uint8_t src, uint8_t dest, move_map allies, move_map foes, move_map king);

void apply_westward_move(
    uint8_t src, uint8_t dest, move_map allies, move_map foes, move_map king);

void gen_king_mm(board b, layer occ, int orig, move_map mm);

struct move_maps build_mms(board b);

void gen_moves_from_mm_white(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total);

void gen_moves_from_mm_black(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total);

void gen_moves_from_mm_king(
    const board b,
    const int orig,
    const move_map allies,
    const move_map them1,
    const move_map them2,
    move *ms,
    dir *ds,
    board *bs,
    int *total);

void gen_moves_from_mm_white_capture(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total);

void gen_moves_from_mm_black_capture(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total);

void gen_moves_from_mm_king_capture(
    const board b,
    const int orig,
    const move_map allies,
    const move_map them1,
    const move_map them2,
    move *ms,
    dir *ds,
    board *bs,
    int *total);

bool corner_moves_1(
    const layer occ, const layer occ_r, const int rank, const int file);

layer corner_paths_1(
    const layer occ, const layer occ_r, const int rank, const int file);

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
