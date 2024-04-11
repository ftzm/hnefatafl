/*
 * # Capture
 * 

There are two types of captures we need to account for: 1) normal
captures, where a piece is sandwiched vertically or horizontally
between two pieces or squares, and 2) shield wall captures, where a
row of pieces along the wall are surrounded by enemy pieces. There are
several implementations of each kind of check; the motivations for
these are described below

# normal captures

To find normal captures, we need to check if any squares around the
arrival square contain opposing pieces (foes), and if the squares
behind those contain allied pieces (allies). To accomplish this, we
pre-generate two lookup tables: one containing foe
position masks for each position, and the other containing corresponding
ally masks. We use pext (parallel bit extract) with the foe layer and
mask to move the relevant 4 bits to the bottom, and do the same with
the ally layer and mask. After a bitwise and on these 2 sets of 4
bits, any remaining 1s represent captures. We use pdep (parallel bit deposit) on
the result and the foe mask to move the captures back into
position, producing a result that can be used to update the board or
extract capture indices.

The above description elided the compicating factor that each layer is
made up of two sub-layer ints. If a mask is split over both sub-layers
then at best we have to duplicate the check on each half, and at worst
we additionally need to shuffle bits between halves so that
corresponding foes and allies are together. Thankfully most masks fall
entirely on one half of either the standard or rotated layers, and
only for captures in the middle of the board do we need to do more
complicated checks. We thus have several normal capture functions,
each of which does the most efficient possible check for a given
section of the board.

# shield wall captures


 * Many capture functions are defined in order to do the most
 * efficient check given the location on the board. The specific
 * considerations are as follows:
 * - captures close to the top or bottom of the board need only check
 *   bits in one sub-layer.
 * - captures close to the left or right of the board need only check
 *   bits in one sub-layer of the rotated copy of the board
 * - some captures are inevitably split between sub boards, but by
 *   strategically using the rotated board we can ensure that
 *   corresponding foes and allies lie in the same sub-boards, such that
 *   we don't need to move bits from one sub-layer to another in order to
 *   to perform the needed bitwise ands.
 * - shield wall capture checks only need to be performed at the edge
 *   of the board
 * - edge squares within 2 squares of a corner only need check for a shield
 *   wall capture in the direction of the opposite corner.
 * 
 * The following map shows which capture functions are used at which positions:
 *
 *                              S
 *
 *          0   1   2   3   4   5   6   7   8   9   10 
 *        _____________________________________________
 *     0  | _,  se, se, s,  s,  s,  s,  s,  sw, sw, _,
 *     1  | es, l,  l,  l,  l,  l,  l,  l,  l,  l,  ws,
 *     2  | es, l,  l,  l,  l,  l,  l,  l,  l,  l,  ws,
 *     3  | e,  l,  l,  l,  l,  l,  l,  l,  l,  r,  w,
 *     4  | e,  R,  R,  R,  b,  y,  y,  b,  r,  r,  w,
 *  E  5  | e,  R,  R,  R,  x,  x,  x,  62, r,  r,  w,  W
 *     6  | e,  R,  R,  R,  x,  x,  x,  x,  r,  r,  w,
 *     7  | e,  R,  R,  R,  B,  y,  y,  B,  r,  u,  w,
 *     8  | en, u,  u,  u,  u,  u,  u,  u,  u,  u,  wn,
 *     9  | en, u,  u,  u,  u,  u,  u,  u,  u,  u,  wn,
 *    10  | _,  ne, ne, n,  n,  n,  n,  n,  nw, nw, _,
 *
 *                              N
 *
 * Standard capture function codes:
 * 
 *   l: capture check on [0] only
 *   u: capture check on [1] only
 *   R: rotated l
 *   r: rotated u
 *   x: capture check on both [0] and [1]
 *   y: rotated x
 *   b: foe check on lower, ally check on above and overlay the
 *      maybe present bit from upper position in highest spot
 *   B: foe check on upper, ally check on lower and overlay the maybe
 *      present bit from lower position in lowest spot, shifting to make space
 *  62: here the north capture is entirely in [1], but the ally for the west
 *      capture also is, so we need to move that down.
 * 
 * Edge (shield wall) functions:
 *   n: u + north shield wall check both directions
 *  ne: u + north shield wall check from east flank
 *  nw: u + north shield wall check from west flank
 *   s: l + south shield wall check both directions
 *  se: l + south shield wall check from east flank
 *  sw: l + south shield wall check from west flank
 *   e: R + east shield wall check both directions
 *  en: R + east shield wall check from north flank
 *  es: R + east shield wall check from south flank
 *   w: r + west shield wall check both directions
 *  wn: r + west shield wall check from north flank
 *  ws: r + west shield wall check from south flank
 *
 ******************************************************************************/

#pragma once

#include "layer.h"
#include "board2.h"

inline layer foe_masks[120];
inline layer foe_masks_r[120];
inline layer ally_masks[120];
inline layer ally_masks_r[120];

void gen_foe_masks();
void gen_ally_masks();
void gen_foe_masks();
void gen_ally_masks();

/*
void capture_l (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_u (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_r (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_R (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_x (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_y (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_b (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_B (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_62 (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_s (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_se (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_sw (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_e (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_en (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_es (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_n (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_ne (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_nw (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_w (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_wn (const layer &, const layer &, layer &, layer &, const unsigned char);
void capture_ws (const layer &, const layer &, layer &, layer &, const unsigned char);

void apply_captures_niave(const layer friends, layer foes, layer output, int dest);
*/

//******************************************************************************

inline void gen_foe_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    // printf("-------------------------------------------------\n");
    // printf("i: %d\n", i);
    modDest = i % 11;
    // printf("modDest: %d\n", modDest);
    if (i < 99) {
      // printf("north\n");
      target = i + 11;
      // printf("target: %d\n", target);
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      // printf("target_r: %d\n", target_r);
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
      // foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r -
      // sub_layer_offset[target_r]));
    }
    if (i > 21) {
      target = i - 11;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
      // foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r -
      // sub_layer_offset[target_r]));
    }
    if (modDest < 9) {
      target = i + 1;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
      // foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r -
      // sub_layer_offset[target_r]));
    }
    if (modDest > 1) {
      target = i - 1;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
      // foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r -
      // sub_layer_offset[target_r]));
    }
    // print_layer(foe_masks[i]);
    // print_layer(foe_masks_r[i]);
  }
}

inline void gen_ally_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 22;
      ally_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 21) {
      target = i - 22;
      ally_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 2;
      ally_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 2;
      ally_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

//******************************************************************************
// Capture functions

#define foes_dir(is_rotated) (is_rotated ? foes_r : foes)
#define allies_dir(is_rotated) (is_rotated ? allies_r : allies)
#define foe_masks_dir(is_rotated) (is_rotated ? foe_masks_r : foe_masks)
#define ally_masks_dir(is_rotated) (is_rotated ? ally_masks_r : ally_masks)
#define rotate_table_dir(is_rotated) (is_rotated ? rotate_left : rotate_right)

/**
 * One might expect that it would be faster to repostion the ally bits
 * to the position of the foe bits and then do the bitwise and there,
 * but surprisingly it seems to be faster to extract both down to the
 * lowest 4 bits, do bitwise and, and then move the result back into
 * position.
 */
template <int sub_index, int sub_offset, bool is_rotated>
void capture_d(const layer &allies, const layer &allies_r, layer &foes,
               layer &foes_r, const unsigned char pos) {

  const layer foe_mask = foe_masks_dir(is_rotated)[pos];
  const layer ally_mask = ally_masks_dir(is_rotated)[pos];

  const uint64_t attackers =
      _pext_u64(allies_dir(is_rotated)[sub_index], ally_mask[sub_index]);
  const uint64_t attackees_ext =
      _pext_u64(foes_dir(is_rotated)[sub_index], foe_mask[sub_index]) &
      attackers;
  if (attackees_ext) {
    uint64_t attackees_dep = _pdep_u64(attackees_ext, foe_mask[sub_index]);
    foes_dir(is_rotated)[sub_index] -= attackees_dep;

    unsigned char r;
    while (attackees_dep) {
      r = rotate_table_dir(is_rotated)[_tzcnt_u64(attackees_dep) + sub_offset];
      foes_dir(!is_rotated)[sub_layer[r]] -=
          ((uint64_t)1 << sub_layer_offset_direct[r]);
      attackees_dep = _blsr_u64(attackees_dep);
    }
  }
}

#define capture_l (capture_d<0, 0, false>)
#define capture_u (capture_d<1, 64, false>)
#define capture_r (capture_d<1, 64, true>)
#define capture_R (capture_d<0, 0, true>)

inline void capture_x(const layer &allies, const layer &allies_r, layer &foes,
               layer &foes_r, const unsigned char pos) {
  const layer foe_mask = foe_masks[pos];
  const layer ally_mask = ally_masks[pos];

  const uint64_t attackers_0 = _pext_u64(allies[0], ally_mask[0]);
  const uint64_t attackees_ext_0 =
      _pext_u64(foes[0], foe_mask[0]) & attackers_0;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes[0] -= attackees_dep_0;

  unsigned char r;
  while (attackees_dep_0) {
    r = rotate_right[_tzcnt_u64(attackees_dep_0)];
    foes_r[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }

  const uint64_t attackers_1 = _pext_u64(allies[1], ally_mask[1]);
  const uint64_t attackees_ext_1 =
      _pext_u64(foes[1], foe_mask[1]) & attackers_1;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  foes[1] -= attackees_dep_1;

  while (attackees_dep_1) {
    // TODO: could have a rotate lookup that is already offset by 64. though
    // zobrist needs the real index so maybe a moot point.
    r = rotate_right[_tzcnt_u64(attackees_dep_1) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
}

inline void capture_y(const layer &allies, const layer &allies_r, layer &foes,
               layer &foes_r, const unsigned char pos) {

  // print_layer(ally_layers.allies);

  // printf("foe_mask\n");
  const layer foe_mask = foe_masks_r[pos];
  const layer ally_mask = ally_masks_r[pos];

  // print_layer(foe_mask);
  // print_layer(ally_mask);

  const uint64_t attackers_0 = _pext_u64(allies_r[0], ally_mask[0]);
  // print_row(attackers_0);
  const uint64_t attackees_ext_0 =
      _pext_u64(foes_r[0], foe_mask[0]) & attackers_0;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes_r[0] -= attackees_dep_0;

  const uint64_t attackers_1 = _pext_u64(allies_r[1], ally_mask[1]);
  const uint64_t attackees_ext_1 =
      _pext_u64(foes_r[1], foe_mask[1]) & attackers_1;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  foes_r[1] -= attackees_dep_1;

  /*
  layer attackers = {attackers_0, attackers_1};
  print_layer(attackers);
  layer attackees_ext = {attackees_ext_0, attackees_ext_1};
  print_layer(attackees_ext);
  layer attackees_dep = {attackees_dep_0, attackees_dep_1};
  print_layer(attackees_dep);
  */

  unsigned char r;
  while (attackees_dep_0) {
    r = rotate_left[_tzcnt_u64(attackees_dep_0)];
    foes[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }

  while (attackees_dep_1) {
    r = rotate_left[_tzcnt_u64(attackees_dep_1) + 64];
    foes[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
  /*
   */
}

inline void capture_b(const layer &allies, const layer &allies_r, layer &foes,
               layer &foes_r, const unsigned char pos) {
  const layer foe_mask = foe_masks[pos];
  const layer ally_mask = ally_masks[pos];

  const uint64_t attackers_0 = _pext_u64(allies[0], ally_mask[0]);
  const uint64_t attackers_1 = _pext_u64(allies[1], ally_mask[1]);
  const uint64_t attackers_all = attackers_0 | (attackers_1 << 3);
  const uint64_t attackees_ext_0 =
      _pext_u64(foes[0], foe_mask[0]) & attackers_all;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes[0] -= attackees_dep_0;

  unsigned char r;
  while (attackees_dep_0) {
    r = rotate_right[_tzcnt_u64(attackees_dep_0)];
    foes_r[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }
}

inline void capture_B(const layer &allies, const layer &allies_r, layer &foes,
               layer &foes_r, const unsigned char pos) {

  /*
  printf("allies\n");
  print_layer(ally_layers.allies);
  printf("foes\n");
  print_layer(foes);
  */

  layer foe_mask = foe_masks[pos];
  layer ally_mask = ally_masks[pos];

  /*
  printf("foe mask\n");
  print_layer(foe_mask);
  printf("ally mask\n");
  print_layer(ally_mask);
  */

  const uint64_t attackers_0 = _pext_u64(allies[0], ally_mask[0]);
  const uint64_t attackers_1 = _pext_u64(allies[1], ally_mask[1]);
  const uint64_t attackers_all = attackers_0 | (attackers_1 << 1);
  const uint64_t attackees_ext_1 =
      _pext_u64(foes[1], foe_mask[1]) & attackers_all;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  foes[1] -= attackees_dep_1;

  /*
  layer attackers = {attackers_0, attackers_1};
  print_layer(attackers);
  layer attackees_ext = {attackees_ext_0, attackees_ext_1};
  print_layer(attackees_ext);
  layer attackees_dep = {attackees_dep_0, attackees_dep_1};
  print_layer(attackees_dep);
  */

  unsigned char r;
  while (attackees_dep_1) {
    r = rotate_right[_tzcnt_u64(attackees_dep_1) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
}

inline void capture_62(const layer &allies, const layer &allies_r, layer &foes,
                layer &foes_r, const unsigned char pos) {

  /*
  printf("allies\n");
  print_layer(ally_layers.allies);
  printf("foes\n");
  print_layer(foes);
  */

  layer foe_mask = foe_masks[pos];
  layer ally_mask = ally_masks[pos];

  /*
  printf("foe mask\n");
  print_layer(foe_mask);
  printf("ally mask\n");
  print_layer(ally_mask);
  */

  const uint64_t attackers_0 = _pext_u64(allies[0], ally_mask[0]);
  const uint64_t attackers_1 = _pext_u64(allies[1], ally_mask[1]);

  const uint64_t borrowed = (attackers_1 & 1) << 2;
  const uint64_t attackees_ext_0 =
      _pext_u64(foes[0], foe_mask[0]) & (attackers_0 | borrowed);
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes[0] -= attackees_dep_0;

  const uint64_t attackees_ext_1 =
      _pext_u64(foes[1], foe_mask[1]) & (attackers_1 >> 1);
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  foes[1] -= attackees_dep_1;

  /*
  layer attackers = {attackers_0, attackers_1};
  print_layer(attackers);
  layer attackees_ext = {attackees_ext_0, attackees_ext_1};
  print_layer(attackees_ext);
  layer attackees_dep = {attackees_dep_0, attackees_dep_1};
  print_layer(attackees_dep);
  */

  unsigned char r;
  while (attackees_dep_0) {
    r = rotate_right[_tzcnt_u64(attackees_dep_0)];
    foes_r[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }

  while (attackees_dep_1) {
    // TODO: could have a rotate lookup that is already offset by 64. though
    // zobrist needs the real index so maybe a moot point.
    r = rotate_right[_tzcnt_u64(attackees_dep_1) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t)1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
  /*
   */
}

//******************************************************************************
// Components

template<int half>
inline uint64_t half_captures(const layer &allies, const layer &foes,
                              const unsigned char pos) {
  const layer foe_mask = foe_masks[pos];
  const layer ally_mask = ally_masks[pos];
  const uint64_t attackers = _pext_u64(allies[half], ally_mask[half]);
  const uint64_t attackees_ext =
      _pext_u64(foes[half], foe_mask[half]) & attackers;
  return _pdep_u64(attackees_ext, foe_mask[half]);
}

template<bool is_rotated, int sub_offset>
inline void distribute_rotated(uint64_t captures, layer &foes) {
  unsigned char r;
  while (captures) {
    r = rotate_table_dir(is_rotated)[_tzcnt_u64(captures) + sub_offset];
    foes[sub_layer[r]] ^= ((uint64_t)1 << sub_layer_offset_direct[r]);
    captures = _blsr_u64(captures);
  }
}

inline void lower_right_shield_captures(const unsigned short flank,
                                        const unsigned short wall,
                                        const unsigned short foes,
                                        const unsigned char pos,
                                        uint64_t *captures) {
  static const unsigned short lowers[10] = {
      0b00000000000, 0b00000000001, 0b00000000011, 0b00000000111, 0b00000001111,
      0b00000011111, 0b00000111111, 0b00001111111, 0b00011111111, 0b00111111111,
  };
  const unsigned short rightward = lowers[pos];
  const unsigned short blockers = flank & rightward;
  const unsigned short blocked = 0xFFFF >> __lzcnt16(blockers);
  const unsigned short mask = (rightward - blocked);
  const unsigned short candidates = mask & foes & wall;
  if (mask == candidates) {
    (*captures) |= mask;
  }
}

inline void lower_left_shield_captures(const unsigned short flank,
                                       const unsigned short wall,
                                       const unsigned short foes,
                                       const unsigned char pos,
                                       uint64_t *captures) {
  static const unsigned short uppers[11] = {
      0b11111111110, 0b11111111100, 0b11111111000, 0b11111110000,
      0b11111100000, 0b11111000000, 0b11110000000, 0b11100000000,
      0b11000000000, 0b10000000000, 0b00000000000,
  };
  const unsigned short leftward = uppers[pos];
  const unsigned short blockers = flank & leftward;
  const unsigned short until = (blockers & -blockers) - 1;
  const unsigned short mask = leftward & until;
  const unsigned short candidates = mask & foes & wall;
  if (mask == candidates) {
    (*captures) |= mask;
  }
}

inline void upper_right_shield_captures(const uint64_t allies,
                                        const uint64_t foes,
                                        const unsigned char pos,
                                        uint64_t *captures) {
  static const unsigned short lowers[10] = {
      0b00000000000, 0b00000000001, 0b00000000011, 0b00000000111, 0b00000001111,
      0b00000011111, 0b00000111111, 0b00001111111, 0b00011111111, 0b00111111111,
  };
  const unsigned short rightward = lowers[pos - 110];
  const unsigned short blockers = (allies >> 46) & rightward;
  const unsigned short blocked = 0xFFFF >> __lzcnt16(blockers);
  const unsigned short mask = (rightward - blocked);
  const unsigned short candidates = mask & (foes >> 46) & (allies >> 35);
  if (mask == candidates) {
    (*captures) |= ((uint64_t)mask << 46);
  }
}

inline void upper_left_shield_captures(const uint64_t allies,
                                       const uint64_t foes,
                                       const unsigned char pos,
                                       uint64_t *captures) {
  static const unsigned short uppers[11] = {
      0b11111111110, 0b11111111100, 0b11111111000, 0b11111110000,
      0b11111100000, 0b11111000000, 0b11110000000, 0b11100000000,
      0b11000000000, 0b10000000000, 0b00000000000,
  };
  const unsigned short leftward = uppers[pos - 110];
  const unsigned short blockers = (allies >> 46) & leftward;
  const unsigned short until = (blockers & -blockers) - 1;
  const unsigned short mask = leftward & until;
  const unsigned short candidates = mask & (foes >> 46) & (allies >> 35);
  if (mask == candidates) {
    (*captures) |= ((uint64_t)mask << 46);
  }
}

//******************************************************************************
// side captures

//**************************************
// south

#define noop ((void)0)

#define lower_left_wall(cond)                                                  \
  (cond ? lower_left_shield_captures(sub_allies, sub_allies >> 11, sub_foes,   \
                                     pos, &captures)                           \
        : noop)

#define lower_right_wall(cond)                                                 \
  (cond ? lower_right_shield_captures(sub_allies, sub_allies >> 11, sub_foes,  \
                                      pos, &captures)                          \
        : noop)

#define upper_left_wall(cond)                                                  \
  (cond ? upper_left_shield_captures(sub_allies, sub_foes, pos, &captures)     \
        : noop)

#define upper_right_wall(cond)                                                 \
  (cond ? upper_right_shield_captures(sub_allies, sub_foes, pos, &captures)    \
        : noop)

#define rotate_pos(cond) (cond ? rotate_right[pos] : pos)

template <bool left, bool right, int sub_index, int sub_offset, bool is_rotated>
void capture_edge(const layer &allies, const layer &allies_r, layer &foes,
                  layer &foes_r, unsigned char pos) {
  pos = rotate_pos(is_rotated);
  uint64_t captures = half_captures<sub_index>(allies_dir(is_rotated),
                                               foes_dir(is_rotated), pos);
  const uint64_t sub_allies = allies_dir(is_rotated)[sub_index];
  const uint64_t sub_foes = foes_dir(is_rotated)[sub_index];

  lower_left_wall(left && !sub_index);
  lower_right_wall(right && !sub_index);
  upper_left_wall(left && sub_index);
  upper_right_wall(right && sub_index);

  foes_dir(is_rotated)[sub_index] -= captures;
  distribute_rotated<is_rotated, sub_offset>(captures, foes_dir(!is_rotated));
}

#define capture_s (capture_edge<true, true, 0, 0, false>)
#define capture_se (capture_edge<true, false, 0, 0, false>)
#define capture_sw (capture_edge<false, true, 0, 0, false>)
#define capture_e (capture_edge<true, true, 0, 0, true>)
#define capture_en (capture_edge<false, true, 0, 0, true>)
#define capture_es (capture_edge<true, false, 0, 0, true>)
#define capture_n (capture_edge<true, true, 1, 64, false>)
#define capture_ne (capture_edge<true, true, 1, 64, false>)
#define capture_nw (capture_edge<true, true, 1, 64, false>)
#define capture_w (capture_edge<true, true, 1, 64, true>)
#define capture_wn (capture_edge<false, true, 1, 64, true>)
#define capture_ws (capture_edge<true, false, 1, 64, true>)

//******************************************************************************

inline __attribute__((always_inline)) void
apply_captures_niave(const layer friends, layer foes, layer foes_r, int dest) {
  int modDest = dest % 11;
  int target;
  int behind;

  //northCapture
  target = dest + 11;
  behind = dest + 22;
  if (dest < 99 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }

  //southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 23 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }

  //westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 8 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }
   
  //eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 2 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }
}

inline void (*capture_functions[121])(const layer &, const layer &, layer &, layer &, const unsigned char) = {
    capture_se,
    capture_se,
    capture_se,
    capture_s,
    capture_s,
    capture_s,
    capture_s,
    capture_s,
    capture_sw,
    capture_sw,
    capture_sw,
    // 11
    capture_es,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_ws,
    // 22
    capture_es,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_ws,
    // 33
    capture_e,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_l,
    capture_r,
    capture_w,
    // 44
    capture_e,
    capture_R,
    capture_R,
    capture_R,
    capture_b,
    capture_y,
    capture_y,
    capture_b,
    capture_r,
    capture_r,
    capture_w,
    // 55
    capture_e,
    capture_R,
    capture_R,
    capture_R,
    capture_x,
    capture_x,
    capture_x,
    capture_62,
    capture_r,
    capture_r,
    capture_w,
    // 66
    capture_e,
    capture_R,
    capture_R,
    capture_R,
    capture_x,
    capture_x,
    capture_x,
    capture_x,
    capture_r,
    capture_r,
    capture_w,
    // 77
    capture_e,
    capture_R,
    capture_R,
    capture_R,
    capture_B,
    capture_y,
    capture_y,
    capture_B,
    capture_r,
    capture_u,
    capture_w,
    // 88
    capture_en,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_wn,
    // 99
    capture_en,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_u,
    capture_wn,
    // 110
    capture_ne,
    capture_ne,
    capture_ne,
    capture_n,
    capture_n,
    capture_n,
    capture_n,
    capture_n,
    capture_nw,
    capture_nw,
    capture_nw,
};

