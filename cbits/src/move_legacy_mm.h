#pragma once

#include "board.h"
#include "move.h"
#include "x86intrin.h" // IWYU pragma: export

struct sources {
  u8 north;
  u8 south;
  u8 east;
  u8 west;
};

typedef struct sources move_map[121];

struct move_maps {
  move_map black;
  move_map white;
  move_map king;
};

void build_mm(layer movers, const layer occ, move_map mm);

void apply_southward_move(
    u8 src,
    u8 dest,
    move_map allies,
    move_map foes,
    move_map king);

void apply_northward_move(
    u8 src,
    u8 dest,
    move_map allies,
    move_map foes,
    move_map king);

void apply_eastward_move(
    u8 src,
    u8 dest,
    move_map allies,
    move_map foes,
    move_map king);

void apply_westward_move(
    u8 src,
    u8 dest,
    move_map allies,
    move_map foes,
    move_map king);

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
