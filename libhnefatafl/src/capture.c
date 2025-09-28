#include "capture.h"
#include "assert.h"
#include "board.h"
#include "constants.h"
#include "layer.h"
#include "stdbool.h"
#include "x86intrin.h" // IWYU pragma: export
#include "zobrist.h"

layer foe_masks[120];
layer foe_masks_r[120];
layer ally_masks[120];
layer ally_masks_r[120];
layer surround_masks[120];
layer surround_masks_r[120];

//******************************************************************************

void gen_foe_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 11;
      // foe_masks[i]._[SUB_LAYER(target)] |=
      //     ((u64)1 << sub_layer_offset_direct[target]);
      // target_r = rotate_right[target];
      // foe_masks_r[i]._[SUB_LAYER(target_r)] |=
      //     ((u64)1 << sub_layer_offset_direct[target_r]);
      OP_LAYER_BIT(foe_masks[i], target, |=);
      target_r = rotate_right[target];
      OP_LAYER_BIT(foe_masks_r[i], target_r, |=);
    }
    if (i > 21) {
      target = i - 11;
      foe_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      foe_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 1;
      foe_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      foe_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 1;
      foe_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      foe_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

void gen_surround_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 110) {
      target = i + 11;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 10) {
      target = i - 11;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 10) {
      target = i + 1;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 0) {
      target = i - 1;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

void gen_ally_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 22;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 21) {
      target = i - 22;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 2;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 2;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

//******************************************************************************

// inline __attribute__((always_inline)) u8
u8 apply_captures_niave(
    const layer friends,
    layer *foes,
    layer *foes_r,
    int dest) {
  u8 count = 0;

  int modDest = dest % 11;
  int target;
  int behind;

  // northCapture
  target = dest + 11;
  behind = dest + 22;
  if (dest < 99 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    count++;
  }

  // southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 21 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    count++;
  }

  // westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 9 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    count++;
  }

  // eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 1 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    count++;
  }

  return count;
}

layer apply_captures_niave_z(
    const layer friends,
    layer *foes,
    layer *foes_r,
    u64 *z,
    u64 hash_table[121],
    int dest) {
  layer output = EMPTY_LAYER;

  int modDest = dest % 11;
  int target;
  int behind;

  // northCapture
  target = dest + 11;
  behind = dest + 22;
  if (dest < 99 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    *z ^= hash_table[target];
    SET_INDEX(output, target);
  }

  // southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 21 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    *z ^= hash_table[target];
    SET_INDEX(output, target);
  }

  // westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 9 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    *z ^= hash_table[target];
    SET_INDEX(output, target);
  }

  // eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 1 && CHECK_INDEX_PTR(foes, target) &&
      CHECK_INDEX(friends, behind)) {
    CLEAR_INDEX_PTR(foes, target);
    int target_r = rotate_right[target];
    CLEAR_INDEX_PTR(foes_r, target_r);
    *z ^= hash_table[target];
    SET_INDEX(output, target);
  }

  return output;
}

layer apply_captures_z_black(board *b, u64 *z, u8 dest) {
  layer captures = apply_captures_niave_z(
      LAYER_OR(b->black, corners),
      &b->white,
      &b->white_r,
      z,
      white_hashes,
      dest);

  // TODO: benchmark if it's faster to take a layer representing potential shild
  // wall captures and check against that before running the shield wall
  // function.
  layer shield_wall_captures = shield_wall_black(b, z, dest);
  return LAYER_OR(captures, shield_wall_captures);
}

layer apply_captures_z_white(board *b, u64 *z, u8 dest) {
  layer captures = apply_captures_niave_z(
      LAYER_OR(LAYER_OR(b->white, b->king), corners),
      &b->black,
      &b->black_r,
      z,
      black_hashes,
      dest);

  layer shield_wall_captures = shield_wall_white(b, z, dest);
  return LAYER_OR(captures, shield_wall_captures);
}

//******************************************************************************

/* Generate a layer representing squares which, when arrived at, will definitely
 * trigger simple captures.*/
layer simple_capture_destinations(
    const layer allies,
    const layer foes,
    const layer occ) {
  layer north =
      LAYER_SHIFTL_SHORT(LAYER_AND(LAYER_SHIFTL_SHORT(allies, 11), foes), 11);
  layer south = LAYER_SHIFTR(LAYER_AND(LAYER_SHIFTR(allies, 11), foes), 11);
  layer east = LAYER_SHIFTR(
      LAYER_AND(LAYER_SHIFTR(LAYER_AND(allies, drop_2_east), 1), foes),
      1);
  layer west = LAYER_SHIFTL_SHORT(
      LAYER_AND(LAYER_SHIFTL_SHORT(LAYER_AND(allies, drop_2_west), 1), foes),
      1);
  return (layer){
      {(north._[0] | south._[0] | east._[0] | west._[0]) & (~occ._[0]),
       (north._[1] | south._[1] | east._[1] | west._[1]) & (~occ._[1])}};
}

/* Generate a layer of locations which, when landed upon, _may_
trigger shield wall captures.

Specifically, this generates a layer of open squares at the edge which
are adjacent to foe pieces , which themselves are adjacent to an
opposing pieces in the inner row.

The idea is not to be perfectly accurate, but rather to rule out a
majority of ineligible edge positions via a relatively cheap "bulk"
computation, so that not every move to an edge position need do a full
shield wall check. As such, the resulting layer may contain false
positives, but may not contain false negatives, as the latter would
never be checked and thus go undiscovered.
*/
layer gen_shield_wall_triggers(
    const layer allies,
    const layer foes,
    const layer occ) {
  layer triggers = EMPTY_LAYER;
  layer open = LAYER_NOT(occ);

  // north
  {
    u64 edges = foes._[1] & (allies._[1] << 11);
    u64 left = (edges << 1);
    u64 right = (edges >> 1);
    triggers._[1] |= left | right;
  }

  // south
  {
    u64 edges = foes._[0] & (allies._[0] >> 11);
    u64 left = (edges << 1);
    u64 right = (edges >> 1);
    triggers._[0] |= left | right;
  }

  // east
  {
    layer edges = LAYER_AND(foes, LAYER_SHIFTR(allies, 1));
    layer up = LAYER_SHIFTL_SHORT(edges, 11);
    layer down = LAYER_SHIFTR(edges, 11);
    triggers = LAYER_OR(triggers, LAYER_OR(up, down));
  }

  // west
  {
    layer edges = LAYER_AND(foes, LAYER_SHIFTL_SHORT(allies, 1));
    layer up = LAYER_SHIFTL_SHORT(edges, 11);
    layer down = LAYER_SHIFTR(edges, 11);
    triggers = LAYER_OR(triggers, LAYER_OR(up, down));
  }

  // limit to open squares at the edge.
  triggers = LAYER_AND(triggers, open);
  triggers = LAYER_AND(triggers, EDGES);

  return triggers;
}

layer black_capture_destinations(const board *b) {
  layer occ = board_occ(*b);
  layer allies = LAYER_OR(b->black, corners);
  return LAYER_OR(
      simple_capture_destinations(allies, b->white, occ),
      gen_shield_wall_triggers(allies, b->white, occ));
}

/* This includes the throne so it can be used with the king; pawns must remove
 * it before calculating destinations */
layer white_capture_destinations(const board *b) {
  layer occ = LAYER_OR(corners, king_board_occ(*b));
  layer allies = LAYER_OR(LAYER_OR(b->white, b->king), corners);
  return LAYER_OR(
      simple_capture_destinations(allies, b->black, occ),
      gen_shield_wall_triggers(allies, b->black, occ));
}

layer black_capture_destinations_r(const board *b) {
  layer occ = board_occ_r(*b);
  layer allies = LAYER_OR(b->black_r, corners);
  return LAYER_OR(
      simple_capture_destinations(allies, b->white_r, occ),
      gen_shield_wall_triggers(allies, b->white_r, occ));
}

/* This includes the throne so it can be used with the king; pawns must remove
 * it before calculating destinations */
layer white_capture_destinations_r(const board *b) {
  layer occ = LAYER_OR(corners, king_board_occ_r(*b));
  layer allies = LAYER_OR(LAYER_OR(b->white_r, b->king_r), corners);
  return LAYER_OR(
      simple_capture_destinations(allies, b->black_r, occ),
      gen_shield_wall_triggers(allies, b->black_r, occ));
}

//******************************************************************************
// Capture functions

// #define foes_dir(is_rotated) (is_rotated ? foes_r : foes)
// #define allies_dir(is_rotated) (is_rotated ? allies_r : allies)
// #define rotate_table_dir(is_rotated) (is_rotated ? rotate_left :
// rotate_right)

//******************************************************************************
// Components

inline u16 right_shield_captures(
    const unsigned short flank,
    const unsigned short wall,
    const unsigned short foes,
    const unsigned char pos) {
  static const unsigned short lowers[10] = {
      0b00000000000,
      0b00000000001,
      0b00000000011,
      0b00000000111,
      0b00000001111,
      0b00000011111,
      0b00000111111,
      0b00001111111,
      0b00011111111,
      0b00111111111,
  };
  // TODO: check that this is maximally efficient
  const unsigned short rightward = lowers[pos];
  const unsigned short blockers = flank & rightward;
  const unsigned short blocked = 0xFFFF >> __lzcnt16(blockers);
  const unsigned short mask = (rightward - blocked);
  const unsigned short candidates = mask & foes & wall;
  return mask == candidates ? mask : 0;
}

inline u16 left_shield_captures(
    const unsigned short flank,
    const unsigned short wall,
    const unsigned short foes,
    const unsigned char pos) {
  static const unsigned short uppers[11] = {
      0b11111111110,
      0b11111111100,
      0b11111111000,
      0b11111110000,
      0b11111100000,
      0b11111000000,
      0b11110000000,
      0b11100000000,
      0b11000000000,
      0b10000000000,
      0b00000000000,
  };
  // TODO: check that this is maximally efficient
  const unsigned short leftward = uppers[pos];
  const unsigned short blockers = flank & leftward;
  const unsigned short until = (blockers & -blockers) - 1;
  const unsigned short mask = leftward & until;
  const unsigned short candidates = mask & foes & wall;
  return mask == candidates ? mask : 0;
}

inline void upper_left_shield_captures(
    const u64 allies,
    const u64 foes,
    const unsigned char pos,
    u64 *captures) {
  u16 row_captures =
      left_shield_captures((allies >> 46), (allies >> 35), (foes >> 46), pos);
  (*captures) |= ((u64)row_captures << 46);
}

inline void upper_right_shield_captures(
    const u64 allies,
    const u64 foes,
    const unsigned char pos,
    u64 *captures) {
  u16 row_captures =
      right_shield_captures((allies >> 46), (allies >> 35), (foes >> 46), pos);
  (*captures) |= ((u64)row_captures << 46);
}

inline void upper_middle_shield_captures(
    const u64 allies,
    const u64 foes,
    const unsigned char pos,
    u64 *captures) {
  u16 flank = allies >> 46;
  u16 wall = allies >> 35;
  u16 foes_row = foes >> 46;
  u16 row_captures = left_shield_captures(flank, wall, foes_row, pos) |
                     right_shield_captures(flank, wall, foes_row, pos);
  (*captures) |= ((u64)row_captures << 46);
}

inline void lower_left_shield_captures(
    const u64 allies,
    const u64 foes,
    const unsigned char pos,
    u64 *captures) {
  u16 row_captures = left_shield_captures(allies, (allies >> 11), foes, pos);
  (*captures) |= ((u64)row_captures);
}

inline void lower_right_shield_captures(
    const u64 allies,
    const u64 foes,
    const unsigned char pos,
    u64 *captures) {
  u16 row_captures = right_shield_captures(allies, (allies >> 11), foes, pos);
  (*captures) |= ((u64)row_captures);
}

inline void lower_middle_shield_captures(
    const u64 allies,
    const u64 foes,
    const unsigned char pos,
    u64 *captures) {
  u16 wall = allies >> 11;
  u16 row_captures = left_shield_captures(allies, wall, foes, pos) |
                     right_shield_captures(allies, wall, foes, pos);
  (*captures) |= ((u64)row_captures);
}

/*[[[cog
import cog
from itertools import product

cog.outl("")

def when(cond, a): return a if cond else ""

def choose_layer(is_black, is_rotated=False):
    return ("black" if is_black else "white") + ("_r" if is_rotated else "")

dir_rot = {"north": False, "south": False, "east": True, "west": True}
dir_upper = {"north": 1, "south": 0, "east": 0, "west": 1}

def build_f(color, direction, portion):
    is_black = color == "black"
    is_rotated = dir_rot[direction]
    is_upper = dir_upper[direction]
    allies = choose_layer(is_black, is_rotated)
    foes = choose_layer(not is_black, is_rotated)
    foes_r = choose_layer(not is_black, not is_rotated)
    sub = f"._[{is_upper}]"
    aligned = "_r" if is_rotated else ""
    rotate_dir = "left" if is_rotated else "right"
    offset = "64 + " if is_upper else ""
    half = "upper" if is_upper else "lower"

    return f"""
layer shield_wall_{color}_{direction}_{portion}(board *b, u64 *z, int pos) {{
  u64 sub_allies = b->{allies}{sub};
  u64 *sub_foes = &b->{foes}{sub};
  layer *foes_r = &b->{foes_r};
  {when(is_rotated, "pos = rotate_right[pos];")}
  {when(is_upper, "pos -= 110;")}
  {when(is_black, "(*sub_foes) |= b->king" + aligned + sub + ";")}
  u64 captures = 0;
  {half}_{portion}_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  {when(is_black, "(*sub_foes) &= ~(b->" + "king" + aligned +  sub + ");")}
  layer output = EMPTY_LAYER;
  while (captures) {{
    int index = {offset}_tzcnt_u64(captures);
    u8 r = rotate_{rotate_dir}[index];
    *z ^= {choose_layer(not is_black)}_hashes[{"r" if is_rotated else "index"}];
    SET_INDEX(output, {"r" if is_rotated else "index"});
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }}
  return output;
}}
    """.lstrip("\n")

colors = ['black', 'white']
dirs = ['north', 'east', 'south', 'west']
portion = ['left', 'middle', 'right']

for (c, d, p) in product(colors, dirs, portion):
    # cog.outl(f"{c}-{d}-{p}")
    cog.outl(build_f(c, d, p))

]]]*/

layer shield_wall_black_north_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black._[1];
  u64 *sub_foes = &b->white._[1];
  layer *foes_r = &b->white_r;

  pos -= 110;
  (*sub_foes) |= b->king._[1];
  u64 captures = 0;
  upper_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king._[1]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= white_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_north_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black._[1];
  u64 *sub_foes = &b->white._[1];
  layer *foes_r = &b->white_r;

  pos -= 110;
  (*sub_foes) |= b->king._[1];
  u64 captures = 0;
  upper_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king._[1]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= white_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_north_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black._[1];
  u64 *sub_foes = &b->white._[1];
  layer *foes_r = &b->white_r;

  pos -= 110;
  (*sub_foes) |= b->king._[1];
  u64 captures = 0;
  upper_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king._[1]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= white_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_east_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black_r._[0];
  u64 *sub_foes = &b->white_r._[0];
  layer *foes_r = &b->white;
  pos = rotate_right[pos];

  (*sub_foes) |= b->king_r._[0];
  u64 captures = 0;
  lower_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king_r._[0]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= white_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_east_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black_r._[0];
  u64 *sub_foes = &b->white_r._[0];
  layer *foes_r = &b->white;
  pos = rotate_right[pos];

  (*sub_foes) |= b->king_r._[0];
  u64 captures = 0;
  lower_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king_r._[0]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= white_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_east_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black_r._[0];
  u64 *sub_foes = &b->white_r._[0];
  layer *foes_r = &b->white;
  pos = rotate_right[pos];

  (*sub_foes) |= b->king_r._[0];
  u64 captures = 0;
  lower_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king_r._[0]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= white_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_south_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black._[0];
  u64 *sub_foes = &b->white._[0];
  layer *foes_r = &b->white_r;

  (*sub_foes) |= b->king._[0];
  u64 captures = 0;
  lower_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king._[0]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= white_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_south_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black._[0];
  u64 *sub_foes = &b->white._[0];
  layer *foes_r = &b->white_r;

  (*sub_foes) |= b->king._[0];
  u64 captures = 0;
  lower_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king._[0]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= white_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_south_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black._[0];
  u64 *sub_foes = &b->white._[0];
  layer *foes_r = &b->white_r;

  (*sub_foes) |= b->king._[0];
  u64 captures = 0;
  lower_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king._[0]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= white_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_west_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black_r._[1];
  u64 *sub_foes = &b->white_r._[1];
  layer *foes_r = &b->white;
  pos = rotate_right[pos];
  pos -= 110;
  (*sub_foes) |= b->king_r._[1];
  u64 captures = 0;
  upper_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king_r._[1]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= white_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_west_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black_r._[1];
  u64 *sub_foes = &b->white_r._[1];
  layer *foes_r = &b->white;
  pos = rotate_right[pos];
  pos -= 110;
  (*sub_foes) |= b->king_r._[1];
  u64 captures = 0;
  upper_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king_r._[1]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= white_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_black_west_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->black_r._[1];
  u64 *sub_foes = &b->white_r._[1];
  layer *foes_r = &b->white;
  pos = rotate_right[pos];
  pos -= 110;
  (*sub_foes) |= b->king_r._[1];
  u64 captures = 0;
  upper_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;
  (*sub_foes) &= ~(b->king_r._[1]);
  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= white_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_north_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white._[1];
  u64 *sub_foes = &b->black._[1];
  layer *foes_r = &b->black_r;

  pos -= 110;

  u64 captures = 0;
  upper_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= black_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_north_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white._[1];
  u64 *sub_foes = &b->black._[1];
  layer *foes_r = &b->black_r;

  pos -= 110;

  u64 captures = 0;
  upper_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= black_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_north_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white._[1];
  u64 *sub_foes = &b->black._[1];
  layer *foes_r = &b->black_r;

  pos -= 110;

  u64 captures = 0;
  upper_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= black_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_east_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white_r._[0];
  u64 *sub_foes = &b->black_r._[0];
  layer *foes_r = &b->black;
  pos = rotate_right[pos];

  u64 captures = 0;
  lower_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= black_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_east_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white_r._[0];
  u64 *sub_foes = &b->black_r._[0];
  layer *foes_r = &b->black;
  pos = rotate_right[pos];

  u64 captures = 0;
  lower_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= black_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_east_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white_r._[0];
  u64 *sub_foes = &b->black_r._[0];
  layer *foes_r = &b->black;
  pos = rotate_right[pos];

  u64 captures = 0;
  lower_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= black_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_south_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white._[0];
  u64 *sub_foes = &b->black._[0];
  layer *foes_r = &b->black_r;

  u64 captures = 0;
  lower_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= black_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_south_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white._[0];
  u64 *sub_foes = &b->black._[0];
  layer *foes_r = &b->black_r;

  u64 captures = 0;
  lower_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= black_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_south_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white._[0];
  u64 *sub_foes = &b->black._[0];
  layer *foes_r = &b->black_r;

  u64 captures = 0;
  lower_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = _tzcnt_u64(captures);
    u8 r = rotate_right[index];
    *z ^= black_hashes[index];
    SET_INDEX(output, index);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_west_left(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white_r._[1];
  u64 *sub_foes = &b->black_r._[1];
  layer *foes_r = &b->black;
  pos = rotate_right[pos];
  pos -= 110;

  u64 captures = 0;
  upper_left_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= black_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_west_middle(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white_r._[1];
  u64 *sub_foes = &b->black_r._[1];
  layer *foes_r = &b->black;
  pos = rotate_right[pos];
  pos -= 110;

  u64 captures = 0;
  upper_middle_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= black_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

layer shield_wall_white_west_right(board *b, u64 *z, int pos) {
  u64 sub_allies = b->white_r._[1];
  u64 *sub_foes = &b->black_r._[1];
  layer *foes_r = &b->black;
  pos = rotate_right[pos];
  pos -= 110;

  u64 captures = 0;
  upper_right_shield_captures(sub_allies, *sub_foes, pos, &captures);
  (*sub_foes) -= captures;

  layer output = EMPTY_LAYER;
  while (captures) {
    int index = 64 + _tzcnt_u64(captures);
    u8 r = rotate_left[index];
    *z ^= black_hashes[r];
    SET_INDEX(output, r);
    OP_LAYER_BIT_PTR(foes_r, r, ^=);
    captures = _blsr_u64(captures);
  }
  return output;
}

//[[[end]]]

// clang-format off
/*[[[cog
import cog


def build_map(color):
    return [
        ("shield_wall_" + color + "_south_left", [1,2]),
        ("shield_wall_" + color + "_south_middle", [3,4,5,6,7]),
        ("shield_wall_" + color + "_south_right", [8,9]),
        ("shield_wall_" + color + "_north_left", [111,112]),
        ("shield_wall_" + color + "_north_middle", [113,114,115,116,117]),
        ("shield_wall_" + color + "_north_right", [118,119]),
        ("shield_wall_" + color + "_east_right", [11,22]),
        ("shield_wall_" + color + "_east_middle", [33,44,55,66,77]),
        ("shield_wall_" + color + "_east_left", [88,99]),
        ("shield_wall_" + color + "_west_right", [21,32]),
        ("shield_wall_" + color + "_west_middle", [43,54,65,76,87]),
        ("shield_wall_" + color + "_west_left", [98,109]),
]


black_map = build_map("black")
cog.outl("layer shield_wall_black(board *b, u64 *z, u8 pos) {\n  switch (pos) {")
for f, indices in black_map:
    for i in indices:
        cog.outl("  case " + str(i) + ":")
        cog.outl("    return " + f + "(b, z, pos);")
cog.outl("  default:")
cog.outl("    return EMPTY_LAYER;")
cog.outl("  }\n}")

cog.outl("")

white_map = build_map("white")
cog.outl("layer shield_wall_white(board *b, u64 *z, u8 pos) {\n  switch (pos) {")
for f, indices in white_map:
    for i in indices:
        cog.outl("  case " + str(i) + ":")
        cog.outl("    return " + f + "(b, z, pos);")
cog.outl("  default:")
cog.outl("    return EMPTY_LAYER;")
cog.outl("  }\n}")

]]]*/
layer shield_wall_black(board *b, u64 *z, u8 pos) {
  switch (pos) {
  case 1:
    return shield_wall_black_south_left(b, z, pos);
  case 2:
    return shield_wall_black_south_left(b, z, pos);
  case 3:
    return shield_wall_black_south_middle(b, z, pos);
  case 4:
    return shield_wall_black_south_middle(b, z, pos);
  case 5:
    return shield_wall_black_south_middle(b, z, pos);
  case 6:
    return shield_wall_black_south_middle(b, z, pos);
  case 7:
    return shield_wall_black_south_middle(b, z, pos);
  case 8:
    return shield_wall_black_south_right(b, z, pos);
  case 9:
    return shield_wall_black_south_right(b, z, pos);
  case 111:
    return shield_wall_black_north_left(b, z, pos);
  case 112:
    return shield_wall_black_north_left(b, z, pos);
  case 113:
    return shield_wall_black_north_middle(b, z, pos);
  case 114:
    return shield_wall_black_north_middle(b, z, pos);
  case 115:
    return shield_wall_black_north_middle(b, z, pos);
  case 116:
    return shield_wall_black_north_middle(b, z, pos);
  case 117:
    return shield_wall_black_north_middle(b, z, pos);
  case 118:
    return shield_wall_black_north_right(b, z, pos);
  case 119:
    return shield_wall_black_north_right(b, z, pos);
  case 11:
    return shield_wall_black_east_right(b, z, pos);
  case 22:
    return shield_wall_black_east_right(b, z, pos);
  case 33:
    return shield_wall_black_east_middle(b, z, pos);
  case 44:
    return shield_wall_black_east_middle(b, z, pos);
  case 55:
    return shield_wall_black_east_middle(b, z, pos);
  case 66:
    return shield_wall_black_east_middle(b, z, pos);
  case 77:
    return shield_wall_black_east_middle(b, z, pos);
  case 88:
    return shield_wall_black_east_left(b, z, pos);
  case 99:
    return shield_wall_black_east_left(b, z, pos);
  case 21:
    return shield_wall_black_west_right(b, z, pos);
  case 32:
    return shield_wall_black_west_right(b, z, pos);
  case 43:
    return shield_wall_black_west_middle(b, z, pos);
  case 54:
    return shield_wall_black_west_middle(b, z, pos);
  case 65:
    return shield_wall_black_west_middle(b, z, pos);
  case 76:
    return shield_wall_black_west_middle(b, z, pos);
  case 87:
    return shield_wall_black_west_middle(b, z, pos);
  case 98:
    return shield_wall_black_west_left(b, z, pos);
  case 109:
    return shield_wall_black_west_left(b, z, pos);
  default:
    return EMPTY_LAYER;
  }
}

layer shield_wall_white(board *b, u64 *z, u8 pos) {
  switch (pos) {
  case 1:
    return shield_wall_white_south_left(b, z, pos);
  case 2:
    return shield_wall_white_south_left(b, z, pos);
  case 3:
    return shield_wall_white_south_middle(b, z, pos);
  case 4:
    return shield_wall_white_south_middle(b, z, pos);
  case 5:
    return shield_wall_white_south_middle(b, z, pos);
  case 6:
    return shield_wall_white_south_middle(b, z, pos);
  case 7:
    return shield_wall_white_south_middle(b, z, pos);
  case 8:
    return shield_wall_white_south_right(b, z, pos);
  case 9:
    return shield_wall_white_south_right(b, z, pos);
  case 111:
    return shield_wall_white_north_left(b, z, pos);
  case 112:
    return shield_wall_white_north_left(b, z, pos);
  case 113:
    return shield_wall_white_north_middle(b, z, pos);
  case 114:
    return shield_wall_white_north_middle(b, z, pos);
  case 115:
    return shield_wall_white_north_middle(b, z, pos);
  case 116:
    return shield_wall_white_north_middle(b, z, pos);
  case 117:
    return shield_wall_white_north_middle(b, z, pos);
  case 118:
    return shield_wall_white_north_right(b, z, pos);
  case 119:
    return shield_wall_white_north_right(b, z, pos);
  case 11:
    return shield_wall_white_east_right(b, z, pos);
  case 22:
    return shield_wall_white_east_right(b, z, pos);
  case 33:
    return shield_wall_white_east_middle(b, z, pos);
  case 44:
    return shield_wall_white_east_middle(b, z, pos);
  case 55:
    return shield_wall_white_east_middle(b, z, pos);
  case 66:
    return shield_wall_white_east_middle(b, z, pos);
  case 77:
    return shield_wall_white_east_middle(b, z, pos);
  case 88:
    return shield_wall_white_east_left(b, z, pos);
  case 99:
    return shield_wall_white_east_left(b, z, pos);
  case 21:
    return shield_wall_white_west_right(b, z, pos);
  case 32:
    return shield_wall_white_west_right(b, z, pos);
  case 43:
    return shield_wall_white_west_middle(b, z, pos);
  case 54:
    return shield_wall_white_west_middle(b, z, pos);
  case 65:
    return shield_wall_white_west_middle(b, z, pos);
  case 76:
    return shield_wall_white_west_middle(b, z, pos);
  case 87:
    return shield_wall_white_west_middle(b, z, pos);
  case 98:
    return shield_wall_white_west_left(b, z, pos);
  case 109:
    return shield_wall_white_west_left(b, z, pos);
  default:
    return EMPTY_LAYER;
  }
}
//[[[end]]]

typedef enum { LEFT, RIGHT, MIDDLE } Portion;

void shield_wall_gen(
    bool is_black,
    bool is_rotated,
    int sub_index,
    Portion portion,
    board *b,
    uint pos) {
  layer *allies = is_black ? &b->black : &b->white;
  layer *allies_r = is_black ? &b->black_r : &b->white_r;
  layer *foes = is_black ? &b->white : &b->black;
  layer *foes_r = is_black ? &b->white_r : &b->black_r;

  if (is_rotated) {
    pos = rotate_right[pos];
  }

  if (sub_index) {
    pos -= 110;
  }

  const u64 sub_allies =
      (is_rotated ? allies_r : allies)->_[sub_index] | corners._[sub_index];
  u64 sub_foes = (is_rotated ? foes_r : foes)->_[sub_index];

  // add king to foes if foes are white
  if (is_black) {
    sub_foes |= (is_rotated ? b->king_r : b->king)._[sub_index];
  }

  u64 captures = 0;

  if (sub_index) {
    switch (portion) {
    case LEFT:
      upper_left_shield_captures(sub_allies, sub_foes, pos, &captures);
      break;
    case RIGHT:
      upper_right_shield_captures(sub_allies, sub_foes, pos, &captures);
      break;
    case MIDDLE:
      upper_middle_shield_captures(sub_allies, sub_foes, pos, &captures);
      break;
    }
  } else {
    switch (portion) {
    case LEFT:
      lower_left_shield_captures(sub_allies, sub_foes, pos, &captures);
      break;
    case RIGHT:
      lower_right_shield_captures(sub_allies, sub_foes, pos, &captures);
      break;
    case MIDDLE:
      lower_middle_shield_captures(sub_allies, sub_foes, pos, &captures);
      break;
    }
  }

  // remove king from captures if it's there
  if (is_black) {
    captures &= ~(is_rotated ? b->king_r : b->king)._[sub_index];
  }

  (is_rotated ? foes_r : foes)->_[sub_index] -= captures;

  while (captures) {
    u8 r =
        (is_rotated
             ? rotate_left
             : rotate_right)[(sub_index ? 64 : 0) + _tzcnt_u64(captures)];
    OP_LAYER_BIT_PTR((is_rotated ? foes : foes_r), r, ^=);
    captures = _blsr_u64(captures);
  }
}

// clang-format off
/*[[[cog
import cog

def build_map(is_black):
  return [
    (f"shield_wall_gen({is_black}, false, 0, LEFT, b, pos)", [1,2]),
    (f"shield_wall_gen({is_black}, false, 0, MIDDLE, b, pos)", [3,4,5,6,7]),
    (f"shield_wall_gen({is_black}, false, 0, RIGHT, b, pos)", [8,9]),
    (f"shield_wall_gen({is_black}, false, 1, LEFT, b, pos)", [111,112]),
    (f"shield_wall_gen({is_black}, false, 1, MIDDLE, b, pos)", [113,114,115,116,117]),
    (f"shield_wall_gen({is_black}, false, 1, RIGHT, b, pos)", [118,119]),
    (f"shield_wall_gen({is_black}, true, 0, LEFT, b, pos)", [11,22]),
    (f"shield_wall_gen({is_black}, true, 0, MIDDLE, b, pos)", [33,44,55,66,77]),
    (f"shield_wall_gen({is_black}, true, 0, RIGHT, b, pos)", [88,99]),
    (f"shield_wall_gen({is_black}, true, 1, LEFT, b, pos)", [21,32]),
    (f"shield_wall_gen({is_black}, true, 1, MIDDLE, b, pos)", [43,54,65,76,87]),
    (f"shield_wall_gen({is_black}, true, 1, RIGHT, b, pos)", [98,109]),
    ]


black_map = build_map("true")
cog.outl("void shield_wall_black_gen(board *b, u8 pos) {\n  switch (pos) {")
for f, indices in black_map:
    for i in indices:
        cog.outl("  case " + str(i) + ":")
        cog.outl("    " + f + ";")
        cog.outl("    break;")
cog.outl("  }\n}")

cog.outl("")

white_map = build_map("false")
cog.outl("void shield_wall_white_gen(board *b, u8 pos) {\n  switch (pos) {")
for f, indices in white_map:
    for i in indices:
        cog.outl("  case " + str(i) + ":")
        cog.outl("    " + f + ";")
        cog.outl("    break;")
cog.outl("  }\n}")

]]]*/
void shield_wall_black_gen(board *b, u8 pos) {
  switch (pos) {
  case 1:
    shield_wall_gen(true, false, 0, LEFT, b, pos);
    break;
  case 2:
    shield_wall_gen(true, false, 0, LEFT, b, pos);
    break;
  case 3:
    shield_wall_gen(true, false, 0, MIDDLE, b, pos);
    break;
  case 4:
    shield_wall_gen(true, false, 0, MIDDLE, b, pos);
    break;
  case 5:
    shield_wall_gen(true, false, 0, MIDDLE, b, pos);
    break;
  case 6:
    shield_wall_gen(true, false, 0, MIDDLE, b, pos);
    break;
  case 7:
    shield_wall_gen(true, false, 0, MIDDLE, b, pos);
    break;
  case 8:
    shield_wall_gen(true, false, 0, RIGHT, b, pos);
    break;
  case 9:
    shield_wall_gen(true, false, 0, RIGHT, b, pos);
    break;
  case 111:
    shield_wall_gen(true, false, 1, LEFT, b, pos);
    break;
  case 112:
    shield_wall_gen(true, false, 1, LEFT, b, pos);
    break;
  case 113:
    shield_wall_gen(true, false, 1, MIDDLE, b, pos);
    break;
  case 114:
    shield_wall_gen(true, false, 1, MIDDLE, b, pos);
    break;
  case 115:
    shield_wall_gen(true, false, 1, MIDDLE, b, pos);
    break;
  case 116:
    shield_wall_gen(true, false, 1, MIDDLE, b, pos);
    break;
  case 117:
    shield_wall_gen(true, false, 1, MIDDLE, b, pos);
    break;
  case 118:
    shield_wall_gen(true, false, 1, RIGHT, b, pos);
    break;
  case 119:
    shield_wall_gen(true, false, 1, RIGHT, b, pos);
    break;
  case 11:
    shield_wall_gen(true, true, 0, LEFT, b, pos);
    break;
  case 22:
    shield_wall_gen(true, true, 0, LEFT, b, pos);
    break;
  case 33:
    shield_wall_gen(true, true, 0, MIDDLE, b, pos);
    break;
  case 44:
    shield_wall_gen(true, true, 0, MIDDLE, b, pos);
    break;
  case 55:
    shield_wall_gen(true, true, 0, MIDDLE, b, pos);
    break;
  case 66:
    shield_wall_gen(true, true, 0, MIDDLE, b, pos);
    break;
  case 77:
    shield_wall_gen(true, true, 0, MIDDLE, b, pos);
    break;
  case 88:
    shield_wall_gen(true, true, 0, RIGHT, b, pos);
    break;
  case 99:
    shield_wall_gen(true, true, 0, RIGHT, b, pos);
    break;
  case 21:
    shield_wall_gen(true, true, 1, LEFT, b, pos);
    break;
  case 32:
    shield_wall_gen(true, true, 1, LEFT, b, pos);
    break;
  case 43:
    shield_wall_gen(true, true, 1, MIDDLE, b, pos);
    break;
  case 54:
    shield_wall_gen(true, true, 1, MIDDLE, b, pos);
    break;
  case 65:
    shield_wall_gen(true, true, 1, MIDDLE, b, pos);
    break;
  case 76:
    shield_wall_gen(true, true, 1, MIDDLE, b, pos);
    break;
  case 87:
    shield_wall_gen(true, true, 1, MIDDLE, b, pos);
    break;
  case 98:
    shield_wall_gen(true, true, 1, RIGHT, b, pos);
    break;
  case 109:
    shield_wall_gen(true, true, 1, RIGHT, b, pos);
    break;
  }
}

void shield_wall_white_gen(board *b, u8 pos) {
  switch (pos) {
  case 1:
    shield_wall_gen(false, false, 0, LEFT, b, pos);
    break;
  case 2:
    shield_wall_gen(false, false, 0, LEFT, b, pos);
    break;
  case 3:
    shield_wall_gen(false, false, 0, MIDDLE, b, pos);
    break;
  case 4:
    shield_wall_gen(false, false, 0, MIDDLE, b, pos);
    break;
  case 5:
    shield_wall_gen(false, false, 0, MIDDLE, b, pos);
    break;
  case 6:
    shield_wall_gen(false, false, 0, MIDDLE, b, pos);
    break;
  case 7:
    shield_wall_gen(false, false, 0, MIDDLE, b, pos);
    break;
  case 8:
    shield_wall_gen(false, false, 0, RIGHT, b, pos);
    break;
  case 9:
    shield_wall_gen(false, false, 0, RIGHT, b, pos);
    break;
  case 111:
    shield_wall_gen(false, false, 1, LEFT, b, pos);
    break;
  case 112:
    shield_wall_gen(false, false, 1, LEFT, b, pos);
    break;
  case 113:
    shield_wall_gen(false, false, 1, MIDDLE, b, pos);
    break;
  case 114:
    shield_wall_gen(false, false, 1, MIDDLE, b, pos);
    break;
  case 115:
    shield_wall_gen(false, false, 1, MIDDLE, b, pos);
    break;
  case 116:
    shield_wall_gen(false, false, 1, MIDDLE, b, pos);
    break;
  case 117:
    shield_wall_gen(false, false, 1, MIDDLE, b, pos);
    break;
  case 118:
    shield_wall_gen(false, false, 1, RIGHT, b, pos);
    break;
  case 119:
    shield_wall_gen(false, false, 1, RIGHT, b, pos);
    break;
  case 11:
    shield_wall_gen(false, true, 0, LEFT, b, pos);
    break;
  case 22:
    shield_wall_gen(false, true, 0, LEFT, b, pos);
    break;
  case 33:
    shield_wall_gen(false, true, 0, MIDDLE, b, pos);
    break;
  case 44:
    shield_wall_gen(false, true, 0, MIDDLE, b, pos);
    break;
  case 55:
    shield_wall_gen(false, true, 0, MIDDLE, b, pos);
    break;
  case 66:
    shield_wall_gen(false, true, 0, MIDDLE, b, pos);
    break;
  case 77:
    shield_wall_gen(false, true, 0, MIDDLE, b, pos);
    break;
  case 88:
    shield_wall_gen(false, true, 0, RIGHT, b, pos);
    break;
  case 99:
    shield_wall_gen(false, true, 0, RIGHT, b, pos);
    break;
  case 21:
    shield_wall_gen(false, true, 1, LEFT, b, pos);
    break;
  case 32:
    shield_wall_gen(false, true, 1, LEFT, b, pos);
    break;
  case 43:
    shield_wall_gen(false, true, 1, MIDDLE, b, pos);
    break;
  case 54:
    shield_wall_gen(false, true, 1, MIDDLE, b, pos);
    break;
  case 65:
    shield_wall_gen(false, true, 1, MIDDLE, b, pos);
    break;
  case 76:
    shield_wall_gen(false, true, 1, MIDDLE, b, pos);
    break;
  case 87:
    shield_wall_gen(false, true, 1, MIDDLE, b, pos);
    break;
  case 98:
    shield_wall_gen(false, true, 1, RIGHT, b, pos);
    break;
  case 109:
    shield_wall_gen(false, true, 1, RIGHT, b, pos);
    break;
  }
}
//[[[end]]]
