#include "move_legacy_mm.h"

const uint8_t rank_mod[121] = {
    0,   0,   0,   0,   0,   0,   0,   0,   0,  0,  0,  11, 11, 11, 11,  11,
    11,  11,  11,  11,  11,  11,  22,  22,  22, 22, 22, 22, 22, 22, 22,  22,
    22,  33,  33,  33,  33,  33,  33,  33,  33, 33, 33, 33, 44, 44, 44,  44,
    44,  44,  44,  44,  44,  44,  44,  55,  55, 55, 55, 55, 55, 55, 55,  55,
    55,  55,  66,  66,  66,  66,  66,  66,  66, 66, 66, 66, 66, 77, 77,  77,
    77,  77,  77,  77,  77,  77,  77,  77,  88, 88, 88, 88, 88, 88, 88,  88,
    88,  88,  88,  99,  99,  99,  99,  99,  99, 99, 99, 99, 99, 99, 110, 110,
    110, 110, 110, 110, 110, 110, 110, 110, 110};


void gen_king_mm(board b, layer occ, int orig, move_map mm) {
  int dest;

  // north
  dest = orig;
  int rank = rank(orig);
  int remaining_north = 10 - rank;
  while (remaining_north--) {
    dest += 11;

    mm[dest].south = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }

  // south
  dest = orig;
  while (rank--) {
    dest -= 11;

    mm[dest].north = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }

  // west
  dest = orig;
  int file = file(orig);
  int remaining_south = 10 - file;
  while (remaining_south--) {
    dest += 1;

    mm[dest].east = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }

  // east
  dest = orig;
  // file = file(orig);
  while (file--) {
    dest -= 1;

    mm[dest].west = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }
}

#define FALLBACK(_x, _y) (_x ? _x : _y)

/*
Macros to get the position closest to the edge in a given direction
relative to the position provided.
*/

#define DIR_EDGE_north(_pos) (file(_pos) + 110)
#define DIR_EDGE_south(_pos) (file(_pos))
#define DIR_EDGE_east(_pos) (rank_mod[_pos])
#define DIR_EDGE_west(_pos) (rank_mod[_pos] + 10)
#define DIR_EDGE(_pos, _dir) DIR_EDGE_##_dir(_pos)

/*
Macros to get the index of the closest piece in a given direction, or
if no such piece exists, the index at the edge of the board in that
direction.
*/

#define ALL_DIR(_x, _dir) (allies[_x]._dir | them1[_x]._dir | them2[_x]._dir)
#define DIRMOST(_pos, _dir)                                                    \
  (FALLBACK(ALL_DIR(_pos, _dir), DIR_EDGE(_pos, _dir)))
#define NORTHMOST(_pos) DIRMOST(_pos, north)
#define SOUTHMOST(_pos) DIRMOST(_pos, south)
#define EASTMOST(_pos) DIRMOST(_pos, east)
#define WESTMOST(_pos) DIRMOST(_pos, west)

inline void departure_rank_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the east
  for (int i = EASTMOST(pos); i < pos; i++) {
    allies[i].west = allies[pos].west;
    them1[i].west = them1[pos].west;
    them2[i].west = them2[pos].west;
  }

  // correct the rank to the west
  for (int i = WESTMOST(pos); i > pos; i--) {
    allies[i].east = allies[pos].east;
    them1[i].east = them1[pos].east;
    them2[i].east = them2[pos].east;
  }
}

inline void arrival_rank_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the east
  for (int i = EASTMOST(pos); i < pos; i++) {
    allies[i].west = pos;
    them1[i].west = 0;
    them2[i].west = 0;
  }

  // correct the rank to the west
  for (int i = WESTMOST(pos); i > pos; i--) {
    allies[i].east = pos;
    them1[i].east = 0;
    them2[i].east = 0;
  }
}

inline void departure_file_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the south
  for (int i = SOUTHMOST(pos); i < pos; i += 11) {
    allies[i].north = allies[pos].north;
    them1[i].north = them1[pos].north;
    them2[i].north = them2[pos].north;
  }

  // correct the rank to the north
  for (int i = NORTHMOST(pos); i > pos; i -= 11) {
    allies[i].south = allies[pos].south;
    them1[i].south = them1[pos].south;
    them2[i].south = them2[pos].south;
  }
}

inline void arrival_file_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the south
  for (int i = SOUTHMOST(pos); i < pos; i += 11) {
    allies[i].north = pos;
    them1[i].north = 0;
    them2[i].north = 0;
  }

  // correct the rank to the north
  for (int i = NORTHMOST(pos); i > pos; i -= 11) {
    allies[i].south = pos;
    them1[i].south = 0;
    them2[i].south = 0;
  }
}

inline void capture_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  departure_file_correction(pos, allies, them1, them2);
  departure_rank_correction(pos, allies, them1, them2);
}

void apply_southward_move(
    const uint8_t orig,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {
  // correct file positions north of the src
  const uint8_t north_stop = FALLBACK(ALL_DIR(orig, north), 120);
  for (int i = orig + 11; i <= north_stop; i += 11) {
    allies[i].south = dest;
  }

  // correct file positions south of the destination
  const uint8_t south_occ =
      allies[orig].south | them1[orig].south | them2[orig].south;
  for (int i = dest - 11; i >= (south_occ ? south_occ : 0); i -= 11) {
    allies[i].north = dest;
  }

  // correct file positions between src and dest
  for (int i = orig - 11; i > dest; i -= 11) {
    allies[i].north = allies[orig].north;
    allies[i].south = dest;
    them1[i].north = them1[orig].north;
    them1[i].south = 0;
    them2[i].north = them2[orig].north;
    them2[i].south = 0;
  }

  // correct dest north
  allies[dest].north = allies[orig].north;
  them1[dest].north = them1[orig].north;
  them2[dest].north = them2[orig].north;

  // correct src south
  allies[orig].south = dest;
  them1[orig].south = 0;
  them2[orig].south = 0;

  departure_rank_correction(orig, allies, them1, them2);
  arrival_rank_correction(dest, allies, them1, them2);
}

void apply_northward_move(
    const uint8_t src,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {
  // correct file positions north of the dest
  const uint8_t north_occ =
      allies[src].north | them1[src].north | them2[src].north;
  for (int i = dest + 11; i <= (north_occ ? north_occ : 120); i += 11) {
    allies[i].south = dest;
  }

  // correct file positions south of the src
  const uint8_t south_occ =
      allies[src].south | them1[src].south | them2[src].south;
  for (int i = src - 11; i >= (south_occ ? south_occ : 0); i -= 11) {
    allies[i].north = dest;
  }

  // correct file positions between src and dest
  for (int i = src + 11; i < dest; i += 11) {
    allies[i].north = dest;
    allies[i].south = allies[src].south;
    them1[i].north = 0;
    them1[i].south = them1[src].south;
    them2[i].north = 0;
    them2[i].south = them2[src].south;
  }

  // correct dest south
  allies[dest].south = allies[src].south;
  them1[dest].south = them1[src].south;
  them2[dest].south = them2[src].south;

  // correct src north
  allies[src].north = dest;
  them1[src].north = 0;
  them2[src].north = 0;

  departure_rank_correction(src, allies, them1, them2);
  arrival_rank_correction(dest, allies, them1, them2);
}

void apply_eastward_move(
    const uint8_t orig,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {
  // correct rank positions west of the orig
  for (int i = WESTMOST(orig); i > orig; i--) {
    allies[i].east = dest;
  }

  // correct rank positions east of the dest
  for (int i = EASTMOST(dest); i < dest; i++) {
    allies[i].west = dest;
  }

  // correct rank positions between src and dest
  for (int i = dest + 1; i < orig; i++) {
    allies[i].west = allies[orig].west;
    allies[i].east = dest;
    them1[i].west = them1[orig].west;
    them1[i].east = 0;
    them2[i].west = them2[orig].west;
    them2[i].east = 0;
  }

  // correct dest west
  allies[dest].west = allies[orig].west;
  them1[dest].west = them1[orig].west;
  them2[dest].west = them2[orig].west;

  // correct orig east
  allies[orig].east = dest;
  them1[orig].east = 0;
  them2[orig].east = 0;

  departure_file_correction(orig, allies, them1, them2);
  arrival_file_correction(dest, allies, them1, them2);
}

void apply_westward_move(
    const uint8_t orig,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {

  // correct rank positions west of the dest
  for (int i = WESTMOST(dest); i > dest; i--) {
    allies[i].east = dest;
  }

  // correct rank positions east of the orig
  for (int i = EASTMOST(orig); i < orig; i++) {
    allies[i].west = dest;
  }

  // correct rank positions between src and dest
  for (int i = orig + 1; i < dest; i++) {
    allies[i].west = dest;
    allies[i].east = allies[orig].east;
    them1[i].west = 0;
    them1[i].east = them1[orig].east;
    them2[i].west = 0;
    them2[i].east = them2[orig].east;
  }

  // correct dest east
  allies[dest].east = allies[orig].east;
  them1[dest].east = them1[orig].east;
  them2[dest].east = them2[orig].east;

  // correct orig west
  allies[orig].west = dest;
  them1[orig].west = 0;
  them2[orig].west = 0;

  departure_file_correction(orig, allies, them1, them2);
  arrival_file_correction(dest, allies, them1, them2);
}

void build_mm(layer movers, const layer occ, move_map mm) {

  int dest;

  int orig = 0;
  u64 pieces = movers._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = rank(orig);
    int remaining_north = 10 - rank;
    while (remaining_north--) {
      dest += 11;
      mm[dest].south = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    // south
    dest = orig;
    while (rank--) {
      dest -= 11;
      mm[dest].north = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    // west
    dest = orig;
    int file = file(orig);
    int remaining_south = 10 - file;
    while (remaining_south--) {
      dest += 1;
      mm[dest].east = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    // east
    dest = orig;
    // file = file(orig);
    while (file--) {
      dest -= 1;
      mm[dest].west = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    pieces -= 1;
    pieces >>= to_next;
  }
  if (lower) {
    orig = 64;
    pieces = movers._[1];
    lower = false;
    goto process;
  }
}

struct move_maps build_mms(board b) {
  struct move_maps mms;
  memset(&mms, 0, sizeof(mms));

  layer occ = board_occ(b);
  build_mm(b.white, occ, mms.white);
  build_mm(b.black, occ, mms.black);
  build_mm(b.king, occ, mms.king);

  return mms;
}

void gen_moves_from_mm_white(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  u64 remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.white, dest, |=);
    op_layer_bit(departed.white_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].north, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].north], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].south, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].south], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].east, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].east], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].west, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].west], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_white_capture(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  u64 remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.white, dest, |=);
    op_layer_bit(departed.white_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].north, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].north], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].south, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].south], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].east, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].east], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].west, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].west], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_black(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  u64 remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.black, dest, |=);
    op_layer_bit(departed.black_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].north, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].north], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].south, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].south], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].east, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].east], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].west, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].west], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_black_capture(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  u64 remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.black, dest, |=);
    op_layer_bit(departed.black_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].north, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].north], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].south, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].south], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].east, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].east], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].west, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].west], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_king(
    const board b,
    const int orig,
    const move_map allies,
    const move_map them1,
    const move_map them2,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;

  // north moves
  int north_occ = FALLBACK(them1[orig].north | them2[orig].north, 121);
  for (int dest = orig + 11; dest < north_occ; dest += 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = north;
    (*total)++;
  }

  // south moves
  int south_occ = FALLBACK(them1[orig].south | them2[orig].south, -1);
  for (int dest = orig - 11; dest > south_occ; dest -= 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = south;
    (*total)++;
  }

  // east moves
  int east_occ =
      FALLBACK(them1[orig].east | them2[orig].east, rank_mod[orig] - 1);
  for (int dest = orig - 1; dest > east_occ; dest--) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = east;
    (*total)++;
  }

  // west moves
  int west_occ =
      FALLBACK(them1[orig].west | them2[orig].west, rank_mod[orig] + 11);
  for (int dest = orig + 1; dest < west_occ; dest++) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = west;
    (*total)++;
  }
}

void gen_moves_from_mm_king_capture(
    const board b,
    const int orig,
    const move_map allies,
    const move_map them1,
    const move_map them2,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;

  // north moves
  int north_occ = FALLBACK(them1[orig].north | them2[orig].north, 121);
  for (int dest = orig + 11; dest < north_occ; dest += 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = north;
    (*total)++;
  }

  // south moves
  int south_occ = FALLBACK(them1[orig].south | them2[orig].south, -1);
  for (int dest = orig - 11; dest > south_occ; dest -= 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = south;
    (*total)++;
  }

  // east moves
  int east_occ =
      FALLBACK(them1[orig].east | them2[orig].east, rank_mod[orig] - 1);
  for (int dest = orig - 1; dest > east_occ; dest--) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = east;
    (*total)++;
  }

  // west moves
  int west_occ =
      FALLBACK(them1[orig].west | them2[orig].west, rank_mod[orig] + 11);
  for (int dest = orig + 1; dest < west_occ; dest++) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = west;
    (*total)++;
  }
}
