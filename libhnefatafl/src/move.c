#include "move.h"
#include "board.h"
#include "capture.h"
#include "constants.h"
#include "io.h"
#include "layer.h"
#include "limits.h"
#include "macro_util.h"
#include "move_legacy.h"
#include "stdbool.h"
#include "stdio.h"
#include "string.h"
#include "x86intrin.h" // IWYU pragma: export

const move start_black_moves[116] = {
    {7, 8},     {7, 9},     {16, 17},   {16, 18},   {16, 19},   {16, 20},
    {16, 21},   {33, 34},   {33, 35},   {33, 36},   {33, 37},   {44, 45},
    {44, 46},   {44, 47},   {66, 67},   {66, 68},   {66, 69},   {77, 78},
    {77, 79},   {77, 80},   {77, 81},   {104, 105}, {104, 106}, {104, 107},
    {104, 108}, {104, 109}, {117, 118}, {117, 119}, {56, 57},   {33, 22},
    {33, 11},   {56, 45},   {56, 34},   {56, 23},   {56, 12},   {56, 1},
    {113, 102}, {113, 91},  {113, 80},  {113, 69},  {114, 103}, {114, 92},
    {114, 81},  {116, 105}, {116, 94},  {116, 83},  {117, 106}, {117, 95},
    {117, 84},  {117, 73},  {64, 53},   {64, 42},   {64, 31},   {64, 20},
    {64, 9},    {43, 32},   {43, 21},   {104, 93},  {3, 1},     {3, 2},
    {16, 11},   {16, 12},   {16, 13},   {16, 14},   {16, 15},   {43, 39},
    {43, 40},   {43, 41},   {43, 42},   {54, 51},   {54, 52},   {54, 53},
    {76, 73},   {76, 74},   {76, 75},   {87, 83},   {87, 84},   {87, 85},
    {87, 86},   {104, 99},  {104, 100}, {104, 101}, {104, 102}, {104, 103},
    {113, 111}, {113, 112}, {64, 63},   {77, 99},   {77, 88},   {56, 111},
    {56, 100},  {56, 89},   {56, 78},   {56, 67},   {3, 47},    {3, 36},
    {3, 25},    {3, 14},    {4, 37},    {4, 26},    {4, 15},    {6, 39},
    {6, 28},    {6, 17},    {7, 51},    {7, 40},    {7, 29},    {7, 18},
    {64, 119},  {64, 108},  {64, 97},   {64, 86},   {64, 75},   {87, 109},
    {87, 98},   {16, 27}};

#define ADJUST_AXIS_VAL_file(_i) _i
#define ADJUST_AXIS_VAL_rank(_i) (10 - _i)
#define ADJUST_AXIS_VAL(_axis) JOIN(ADJUST_AXIS_VAL, _axis)(_axis)

int cmp_moves(const move *a, const move *b) {
  int orig_cmp = a->orig - b->orig;
  if (orig_cmp == 0) {
    return a->dest - b->dest;
  } else {
    return orig_cmp;
  }
}

// -----------------------------------------------------------------------------
// new move to destination

/* Offset if the index is 1, otherwise do nothing */
#define OFFSET_0(_)
#define OFFSET_1(_n) (_n += 64)
#define OFFSET(_n, _i) OFFSET_##_i(_n)

#define ROTATE rotate_right
#define ROTATE_r rotate_left
#define ROTATE_DIR(_r) ROTATE##_r

#define ROTATED_HALF rotated_half_right
#define ROTATED_HALF_r rotated_half_left
#define ROTATED_HALF_DIR(_r) ROTATED_HALF##_r

#define ROTATED_OFFSET rotated_offset_right
#define ROTATED_OFFSET_r rotated_offset_left
#define ROTATED_OFFSET_DIR(_r) ROTATED_OFFSET##_r

/*
leftward
*/

#define EXTRACT_CENTER_LEFTWARD(_l)                                            \
  u16 dest_bit = dests & -dests;                                               \
  u8 dest = _tzcnt_u16(dest_bit);                                              \
  dest += 55;                                                                  \
  u8 orig = 15 - __lzcnt16((dest_bit - 1) & _l);                               \
  orig += 55;

#define DROP_1_EAST_0 18410697675910412286ULL
#define DROP_1_EAST_1 144044784955154427ULL

// TODO: I don't know what the DROP_1_EAST is about; I'm not doing it
// in move counts but in move counts I am doing & LOWER_HALF_MASK. I
// should unify them so the logic is the same.
//
// UPDATE: In move count I'm doing & ~occ to remove everything that isn't a
// ray; because targets here are a subset of ~occ then it performs the
// same function.
//

/* HOW THIS WORKS:
 *   Generation of move destinations:
 *   - the general technique is subtraction difference: subtracting
 *     one u64 from another in order to create a set of leftward rays.
 *   - prepare the occ u64:
 *   - subtraction difference: given a half-layer of movers, shift it
 *     to the left, and then subtract it from a half-layer of occupied
 *     this position. We shift it to the left first because otherwise
 *     we'd simply remove the movers from the occupied u64.
 *   - the result is a u64 in which contiguous 0s left of each 1 in
 *     the occ u64 is changed to 1, and the closest leftward "blocker"
 *     or set bit to the left of each mover bit is changed to 0.
 *   - isolate move bits: we do bitwise and on the mask that excludes
 *     the 0 file, to remove that file of blockers which prevented
 *     rank-wrap.
 *   - extract legal target moves: our resulting u64 contains both
 *     leftward move rays and remaining un-zeroed occupied
 *     positions. By doing bitwise and with the u64 of target
 *     destinations (which are assumed to be legal, unoccupied
 *     positions) we limit results to legal destinations.
 */
/* From extract:
 * - isolate the lowest set bit
 * - get the index of the lowest set bit
 * - potentially offset the index if processing upper half
 * - get the index of the origin by:
 *   - masking below the dest bit
 *   - doing bitwise and with the occ layer
 *   - doing leading zero count of the result
 *   - subtracting the leading zero count from 64
 *
 * NOTE: We recalculate the origin for each destination. The alternative
 *     (iterating movers, partitioning destinations per mover via a nested
 *     loop) has been benchmarked multiple times and is counterintuitively
 *     slower—the overhead of iterating all movers (including those without
 *     destinations) and computing partition masks outweighs the cost of the
 *     per-destination lzcnt chain.
 */
#define LEFTWARD(_i, _r)                                                       \
  {                                                                            \
    u64 dests = targets##_r._[_i]                                              \
                & (leftward_occ##_r._[_i] - (movers##_r._[_i] << 1))           \
                & DROP_1_EAST_##_i;                                            \
    while (dests) {                                                            \
      u64 dest_bit = _blsi_u64(dests);                                         \
      u8 dest = _tzcnt_u64(dest_bit);                                          \
      OFFSET(dest, _i);                                                        \
      u8 orig =                                                                \
          63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & leftward_occ##_r._[_i]);     \
      __attribute__((unused)) u64 orig_bit = (u64)1 << orig;                   \
      OFFSET(orig, _i);                                                        \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      u8 dest_r = ROTATE_DIR(_r)[dest];                                        \
      BOOKKEEP##_r(_i);                                                        \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }

#define RIGHTWARD2(_i, _r)                                                     \
  {                                                                            \
    u64 blockers = occ##_r._[_i] | file_mask_10._[_i];                         \
    u64 movers_ext = _pext_u64(movers##_r._[_i], blockers) >> 1;               \
    u64 movers_dep = _pdep_u64(movers_ext, (blockers << 1));                   \
    u64 dests = (movers##_r._[_i] - movers_dep) & targets##_r._[_i];           \
    while (dests) {                                                            \
      u64 dest_bit = _blsi_u64(dests);                                         \
      u8 dest = _tzcnt_u64(dest_bit);                                          \
      OFFSET(dest, _i);                                                        \
                                                                               \
      u64 orig_bit = _blsi_u64(movers##_r._[_i] & -dest_bit);                  \
      u8 orig = _tzcnt_u64(orig_bit);                                          \
      OFFSET(orig, _i);                                                        \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      u8 dest_r = ROTATE_DIR(_r)[dest];                                        \
      BOOKKEEP##_r(_i);                                                        \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }

#define LEFTWARD_CENTER(_r)                                                    \
  {                                                                            \
    u16 dests = GET_CENTER_ROW(targets##_r)                                    \
                & (center_occ##_r - (center_movers##_r << 1));                 \
    while (dests) {                                                            \
      u16 dest_bit = dests & -dests;                                           \
      u8 dest = _tzcnt_u16(dest_bit);                                          \
      dest += 55;                                                              \
      u8 orig = 15 - __lzcnt16((dest_bit - 1) & center_occ##_r);               \
      orig += 55;                                                              \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      u8 dest_r = ROTATE_DIR(_r)[dest];                                        \
      BOOKKEEP_CENTER##_r();                                                   \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }
/*
leftward
*/

#define RIGHTWARD_DESTS(_i, _r)                                                \
  u64 below = orig_bit - 1;                                                    \
  u64 above_highest_occ_mask =                                                 \
      ~((u64) - 1 >> _lzcnt_u64(rightward_occ##_r._[_i] & below));             \
  u64 dests = targets##_r._[_i] & below & above_highest_occ_mask;

#define RIGHTWARD_DESTS_CENTER(_r)                                             \
  u16 below = orig_bit - 1;                                                    \
  u16 above_highest_occ_mask =                                                 \
      (center_occ##_r & below)                                                 \
          ? ((u16) - 1 << (16 - __lzcnt16(center_occ##_r & below)))            \
          : (u16) - 1;                                                         \
  u16 dests = GET_CENTER_ROW(targets##_r) & below & above_highest_occ_mask;

#define RIGHTWARD(_i, _r)                                                      \
  {                                                                            \
    u64 origs =                                                                \
        movers##_r._[_i] & ~(rightward_occ##_r._[_i] - targets##_r._[_i]);     \
    while (origs) {                                                            \
                                                                               \
      u64 orig_bit = _blsi_u64(origs);                                         \
      u8 orig = _tzcnt_u64(orig_bit);                                          \
      OFFSET(orig, _i);                                                        \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      origs -= orig_bit;                                                       \
                                                                               \
      RIGHTWARD_DESTS(_i, _r);                                                 \
                                                                               \
      while (dests) {                                                          \
        u8 dest = _tzcnt_u64(dests);                                           \
        u64 dest_bit = (u64)1 << dest;                                         \
        OFFSET(dest, _i);                                                      \
        u8 dest_r = ROTATE_DIR(_r)[dest];                                      \
                                                                               \
        BOOKKEEP##_r(_i);                                                      \
                                                                               \
        dests -= dest_bit;                                                     \
      }                                                                        \
    }                                                                          \
  }

#define HALF_MASK_0 LOWER_HALF_MASK
#define HALF_MASK_1 UPPER_HALF_MASK

/* Generating move mask:
 * - prepare the blockers u64 by doing bitwise or with file mask 10.
 *   Filling file 10 ensures rightward rays don't wrap around ranks.
 * - produce a mask wherein movers are shifted into the positions of
 *     the closest blocker to the right.
 *   - PEXT mover bits based on the pattern of blockers, then shift
 *     them to the right.
 *   - PDEP extracted bits back into the blockers pattern, shifted 1
 *     bit left so that subtraction will produce leftward rays.
 * Generating origs:
 * Extraction:
 */
#define RIGHTWARD1(_i, _r)                                                     \
  {                                                                            \
    u64 blockers = occ##_r._[_i] | file_mask_10._[_i];                         \
    u64 movers_ext = _pext_u64(movers##_r._[_i], blockers) >> 1;               \
    u64 movers_dep = _pdep_u64(movers_ext, (blockers << 1));                   \
    u64 move_mask = (movers##_r._[_i] - movers_dep) & targets##_r._[_i];       \
    u64 origs = movers##_r._[_i] & ~(movers##_r._[_i] - targets##_r._[_i]);    \
    while (origs) {                                                            \
                                                                               \
      u64 orig_bit = _blsi_u64(origs);                                         \
      u8 orig = _tzcnt_u64(orig_bit);                                          \
      OFFSET(orig, _i);                                                        \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      origs -= orig_bit;                                                       \
                                                                               \
      u64 dests = move_mask & (orig_bit - 1);                                  \
                                                                               \
      while (dests) {                                                          \
        u8 dest = _tzcnt_u64(dests);                                           \
        u64 dest_bit = (u64)1 << dest;                                         \
        OFFSET(dest, _i);                                                      \
        u8 dest_r = ROTATE_DIR(_r)[dest];                                      \
                                                                               \
        BOOKKEEP##_r(_i);                                                      \
                                                                               \
        dests -= dest_bit;                                                     \
      }                                                                        \
                                                                               \
      move_mask &= 0 - orig_bit;                                               \
    }                                                                          \
  }

// The only difference with the one above is `1 |` on the blockers.
#define RIGHTWARD1_KING(_i, _r)                                                \
  {                                                                            \
    u64 blockers = occ##_r._[_i] | file_mask_10._[_i];                         \
    u64 movers_ext = _pext_u64(movers##_r._[_i], blockers);                    \
    u64 movers_dep = _pdep_u64(movers_ext, 1 | (blockers << 1));               \
    u64 move_mask =                                                            \
        (movers##_r._[_i] - movers_dep) & HALF_MASK_##_i & targets##_r._[_i];  \
    u64 origs = movers##_r._[_i] & ~(movers##_r._[_i] - targets##_r._[_i]);    \
    while (origs) {                                                            \
                                                                               \
      u64 orig_bit = _blsi_u64(origs);                                         \
      u8 orig = _tzcnt_u64(orig_bit);                                          \
      OFFSET(orig, _i);                                                        \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      origs -= orig_bit;                                                       \
                                                                               \
      u64 dests = move_mask & (orig_bit - 1);                                  \
                                                                               \
      while (dests) {                                                          \
        u8 dest = _tzcnt_u64(dests);                                           \
        u64 dest_bit = (u64)1 << dest;                                         \
        OFFSET(dest, _i);                                                      \
        u8 dest_r = ROTATE_DIR(_r)[dest];                                      \
                                                                               \
        BOOKKEEP##_r(_i);                                                      \
                                                                               \
        dests -= dest_bit;                                                     \
      }                                                                        \
                                                                               \
      move_mask &= 0 - orig_bit;                                               \
    }                                                                          \
  }

#define RIGHTWARD_CENTER(_r)                                                   \
  {                                                                            \
    u16 origs =                                                                \
        center_movers##_r & ~(center_occ##_r - (GET_CENTER_ROW(targets##_r))); \
    while (origs) {                                                            \
                                                                               \
      u16 orig_bit = origs & -origs;                                           \
      u8 orig = _tzcnt_u16(orig_bit);                                          \
      origs -= orig_bit;                                                       \
      orig += 55;                                                              \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
                                                                               \
      RIGHTWARD_DESTS_CENTER(_r);                                              \
                                                                               \
      while (dests) {                                                          \
        u8 dest = _tzcnt_u16(dests);                                           \
        u16 dest_bit = (u16)1 << dest;                                         \
        dest += 55;                                                            \
                                                                               \
        u8 dest_r = ROTATE_DIR(_r)[dest];                                      \
        BOOKKEEP_CENTER##_r();                                                 \
                                                                               \
        dests -= dest_bit;                                                     \
      }                                                                        \
    }                                                                          \
  }

/*
Save the move and layers, increment the loop.

because we're generating diff layers that are applied with xor we
don't actually need to distinguish between the orig and dest in the
layers, only in the move.
*/

#define _WRITE_LAYER_PAIR(_layer, _a, _b)                                      \
  {                                                                            \
    u8 _ah = SUB_LAYER(_a);                                                    \
    u64 _ab = (u64)1 << sub_layer_offset_direct[_a];                           \
    u8 _bh = SUB_LAYER(_b);                                                    \
    u64 _bb = (u64)1 << sub_layer_offset_direct[_b];                           \
    _layer._[_ah] = _ab;                                                       \
    _layer._[_bh] |= _bb;                                                      \
  }

#define BOOKKEEP(_i)                                                           \
  ms[(*total)] = (move){orig, dest};                                           \
  ls[(*total)]._[_i] = orig_bit | dest_bit;                                    \
  _WRITE_LAYER_PAIR(ls_r[(*total)], orig_r, dest_r)                            \
  (*total)++;

#define BOOKKEEP_R(_i)                                                         \
  ms[(*total)] = (move){orig_r, dest_r};                                       \
  ls_r[(*total)]._[_i] = orig_bit | dest_bit;                                  \
  _WRITE_LAYER_PAIR(ls[(*total)], orig_r, dest_r)                              \
  (*total)++;
#define BOOKKEEP_r BOOKKEEP_R
#define BOOKKEEP_CENTER_R()                                                    \
  ms[(*total)] = (move){orig_r, dest_r};                                       \
  _WRITE_LAYER_PAIR(ls_r[(*total)], orig, dest)                                \
  _WRITE_LAYER_PAIR(ls[(*total)], orig_r, dest_r)                              \
  (*total)++;
#define BOOKKEEP_CENTER_r() BOOKKEEP_CENTER_R()

#define BOOKKEEP_CENTER()                                                      \
  ms[(*total)] = (move){orig, dest};                                           \
  _WRITE_LAYER_PAIR(ls[(*total)], orig, dest)                                  \
  _WRITE_LAYER_PAIR(ls_r[(*total)], orig_r, dest_r)                            \
  (*total)++;

#define BIT_AT(_i) ((u64)1 << _i)

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
    int *total) {

  u16 center_occ = GET_CENTER_ROW(occ);
  u16 center_occ_r = GET_CENTER_ROW(occ_r);
  u16 center_movers = GET_CENTER_ROW(movers);
  u16 center_movers_r = GET_CENTER_ROW(movers_r);
  layer leftward_occ = LAYER_OR(occ, file_mask_0);
  layer leftward_occ_r = LAYER_OR(occ_r, file_mask_0);
  // I thought these might be necessary but maybe not...
  // rightward_occ._[1] |= 2;
  // rightward_occ_r._[1] |= 2;
  movers._[0] &= LOWER_HALF_MASK;
  movers._[1] &= UPPER_HALF_MASK;
  movers_r._[0] &= LOWER_HALF_MASK;
  movers_r._[1] &= UPPER_HALF_MASK;

  // printf("start total: %d\n", *total);
  LEFTWARD(0, ); // lower westward
  // printf("leftward total: %d\n", *total);
  LEFTWARD(1, ); // upper westward
  // printf("leftward total: %d\n", *total);
  LEFTWARD_CENTER(); // center westward
  // printf("leftward total: %d\n", *total);

  LEFTWARD(0, _r); // lower southward
  // printf("leftward total: %d\n", *total);
  LEFTWARD(1, _r); // upper southward
  // printf("leftward total: %d\n", *total);
  LEFTWARD_CENTER(_r); // center southward
  // printf("leftward total: %d\n", *total);

  RIGHTWARD1(0, ); // lower eastward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD1(1, ); // upper eastward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD_CENTER(); // center eastward
  // printf("rightward total: %d\n", *total);

  RIGHTWARD1(0, _r); // lower northward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD1(1, _r); // upper northward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD_CENTER(_r); // center northward
  // printf("rightward total: %d\n", *total);
}

void moves_to2(
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r,
    move *ms,
    layer *ls,
    layer *ls_r,
    int *total) {

  u16 center_occ = GET_CENTER_ROW(occ);
  u16 center_occ_r = GET_CENTER_ROW(occ_r);
  u16 center_movers = GET_CENTER_ROW(movers);
  u16 center_movers_r = GET_CENTER_ROW(movers_r);
  layer leftward_occ = LAYER_OR(occ, file_mask_0);
  layer leftward_occ_r = LAYER_OR(occ_r, file_mask_0);
  // I thought these might be necessary but maybe not...
  // rightward_occ._[1] |= 2;
  // rightward_occ_r._[1] |= 2;
  movers._[0] &= LOWER_HALF_MASK;
  movers._[1] &= UPPER_HALF_MASK;
  movers_r._[0] &= LOWER_HALF_MASK;
  movers_r._[1] &= UPPER_HALF_MASK;

  // printf("start total: %d\n", *total);
  LEFTWARD(0, ); // lower westward
  // printf("leftward total: %d\n", *total);
  LEFTWARD(1, ); // upper westward
  // printf("leftward total: %d\n", *total);
  LEFTWARD_CENTER(); // center westward
  // printf("leftward total: %d\n", *total);

  LEFTWARD(0, _r); // lower southward
  // printf("leftward total: %d\n", *total);
  LEFTWARD(1, _r); // upper southward
  // printf("leftward total: %d\n", *total);
  LEFTWARD_CENTER(_r); // center southward
  // printf("leftward total: %d\n", *total);

  RIGHTWARD2(0, ); // lower eastward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD2(1, ); // upper eastward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD_CENTER(); // center eastward
  // printf("rightward total: %d\n", *total);

  RIGHTWARD2(0, _r); // lower northward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD2(1, _r); // upper northward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD_CENTER(_r); // center northward
  // printf("rightward total: %d\n", *total);
}

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
    int *total) {

  u16 center_occ = GET_CENTER_ROW(occ);
  u16 center_occ_r = GET_CENTER_ROW(occ_r);
  u16 center_movers = GET_CENTER_ROW(movers);
  u16 center_movers_r = GET_CENTER_ROW(movers_r);
  layer leftward_occ = LAYER_OR(occ, file_mask_0);
  layer leftward_occ_r = LAYER_OR(occ_r, file_mask_0);
  // I thought these might be necessary but maybe not...
  // rightward_occ._[1] |= 2;
  // rightward_occ_r._[1] |= 2;
  movers._[0] &= LOWER_HALF_MASK;
  movers._[1] &= UPPER_HALF_MASK;
  movers_r._[0] &= LOWER_HALF_MASK;
  movers_r._[1] &= UPPER_HALF_MASK;

  // printf("start total: %d\n", *total);
  LEFTWARD(0, ); // lower westward
  // printf("leftward total: %d\n", *total);
  LEFTWARD(1, ); // upper westward
  // printf("leftward total: %d\n", *total);
  LEFTWARD_CENTER(); // center westward
  // printf("leftward total: %d\n", *total);

  LEFTWARD(0, _r); // lower southward
  // printf("leftward total: %d\n", *total);
  LEFTWARD(1, _r); // upper southward
  // printf("leftward total: %d\n", *total);
  LEFTWARD_CENTER(_r); // center southward
  // printf("leftward total: %d\n", *total);

  RIGHTWARD1_KING(0, ); // lower eastward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD1(1, ); // upper eastward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD_CENTER(); // center eastward
  // printf("rightward total: %d\n", *total);

  RIGHTWARD1_KING(0, _r); // lower northward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD1(1, _r); // upper northward
  // printf("rightward total: %d\n", *total);
  RIGHTWARD_CENTER(_r); // center northward
  // printf("rightward total: %d\n", *total);
}

/*
 * leftward_moves_layer - Generate all leftward move destinations for all movers
 * simultaneously.
 *
 * occ must include corners so that they act as blockers, preventing
 * move generation into corner squares.
 *
 * "Leftward" = towards higher bit index = towards the MSB within each row.
 * In diagrams, "/" marks squares belonging to the other half.
 *
 *
 * ====== LOWER HALF: _[0] (rows 0-5)
 *
 * _[0] bit layout (bit = index):
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5 bits:  /  / 63 62 61 60 59 58 57 56 55
 *   row 4 bits: 54 53 52 51 50 49 48 47 46 45 44
 *   row 3 bits: 43 42 41 40 39 38 37 36 35 34 33
 *   row 2 bits: 32 31 30 29 28 27 26 25 24 23 22
 *   row 1 bits: 21 20 19 18 17 16 15 14 13 12 11
 *   row 0 bits: 10  9  8  7  6  5  4  3  2  1  0
 *
 * Example — movers (M), other pieces (X), corners (C):
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  .  M  .  .
 *   row 4:       .  .  .  .  .  .  .  .  .  .  .
 *   row 3:       .  .  .  .  .  .  .  .  .  .  .
 *   row 2:       .  .  .  .  X  .  .  M  .  .  .
 *   row 1:       .  .  .  .  .  X  M  .  .  .  .
 *   row 0:       C  .  .  .  .  .  .  M  .  .  C
 *
 * blockers = occ._[0] | file_mask_0._[0]
 *
 *   Column-0 sentinels prevent borrow propagation across row boundaries.
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  .  1  .  1
 *   row 4:       .  .  .  .  .  .  .  .  .  .  1
 *   row 3:       .  .  .  .  .  .  .  .  .  .  1
 *   row 2:       .  .  .  .  1  .  .  1  .  .  1
 *   row 1:       .  .  .  .  .  1  1  .  .  .  1
 *   row 0:       1  .  .  .  .  .  .  1  .  .  1
 *                ^                             ^
 *             corner                       sentinels
 *
 * shifted_movers = movers._[0] << 1
 *
 *   Since movers are a subset of occ (and therefore blockers),
 *   subtracting unshifted movers would just cancel out (1-1=0, no
 *   borrow). We shift all movers to 1 bit to the left, so that that
 *   bit is 0 we are able to trigger a borrow propagation ray.
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  1  .  .  .
 *   row 4:       .  .  .  .  .  .  .  .  .  .  .
 *   row 3:       .  .  .  .  .  .  .  .  .  .  .
 *   row 2:       .  .  .  .  .  .  1  .  .  .  .
 *   row 1:       .  .  .  .  .  1  .  .  .  .  .
 *   row 0:       .  .  .  .  .  .  1  .  .  .  .
 *
 * subtraction = blockers - shifted_movers
 *
 *   Binary subtraction propagates borrows from each shifted mover
 *   towards the MSB. Each 0-bit along the way is flipped to 1; the
 *   first 1-bit (blocker) hit is flipped to 0, and the borrow stops.
 *   The sentinel at col 0 prevents borrow from wrapping into the next
 *   row. In row 1, the shifted mover lands directly on the blocker
 *   (1-1=0), producing no ray. In row 5, the borrow overflows past
 *   the MSB — the carryover logic (below) handles continuation into
 *   _[1]. So we end up with a row that contains leftward rays and the
 *   original blockers, modulo those that have been flipped to 0 when
 *   a subtraction ray hits it.
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  1  1  1  1  1  1  1  .  1
 *   row 4:       .  .  .  .  .  .  .  .  .  .  1
 *   row 3:       .  .  .  .  .  .  .  .  .  .  1
 *   row 2:       .  .  .  .  .  1  1  1  .  .  1
 *   row 1:       .  .  .  .  .  .  1  .  .  .  .
 *   row 0:       .  1  1  1  1  1  1  1  .  .  1
 *
 * dests = subtraction & ~(blockers | throne._[0])
 *
 *   Mask away the original blockers, sentinels, and throne square,
 *   leaving only the ray destination squares.
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  1  1  1  .  1  1  .  .  .  (throne at col 5 masked)
 *   row 4:       .  .  .  .  .  .  .  .  .  .  .
 *   row 3:       .  .  .  .  .  .  .  .  .  .  .
 *   row 2:       .  .  .  .  .  1  1  .  .  .  .
 *   row 1:       .  .  .  .  .  .  .  .  .  .  .
 *   row 0:       .  1  1  1  1  1  1  .  .  .  .
 *
 * boundary_bit = dests | (movers._[0] & ((u64)1 << 63))
 * carryover    = boundary_bit >> 63
 *
 *   Row 5 is split: cols 0-8 in _[0] (bits 55-63), cols 9-10 in _[1]
 *   (bits 0-1). A leftward ray that reaches bit 63 — or a mover
 *   sitting at bit 63 whose shift overflowed — needs to continue into
 *   _[1]. boundary_bit captures bit 63 if it holds a dest or a mover,
 *   and shifting right by 63 moves isolates it at index 0, which is
 *   where the corresponding ray should start in the upper half.
 *
 *   In our example, bit 63 (col 8) is set in dests, so carryover = 1.
 *
 *
 * ====== UPPER HALF: _[1] (rows 5-10)
 *
 * _[1] bit layout (bit = index - 64):
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10 bits: 56 55 54 53 52 51 50 49 48 47 46
 *   row  9 bits: 45 44 43 42 41 40 39 38 37 36 35
 *   row  8 bits: 34 33 32 31 30 29 28 27 26 25 24
 *   row  7 bits: 23 22 21 20 19 18 17 16 15 14 13
 *   row  6 bits: 12 11 10  9  8  7  6  5  4  3  2
 *   row  5 bits:  1  0  /  /  /  /  /  /  /  /  /
 *
 * Continuing the example with carryover = 1 (M=mover, X=other piece, C=corner):
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       C  .  .  .  .  .  .  .  .  .  C
 *   row  9:       .  .  .  .  .  .  .  .  .  .  .
 *   row  8:       .  .  .  .  .  .  .  .  .  .  .
 *   row  7:       .  .  .  .  .  .  .  .  .  .  .
 *   row  6:       .  .  .  X  .  .  M  .  .  .  .
 *   row  5:       .  .  /  /  /  /  /  /  /  /  /
 *
 * blockers = occ._[1] | file_mask_0._[1]
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       1  .  .  .  .  .  .  .  .  .  1
 *   row  9:       .  .  .  .  .  .  .  .  .  .  1
 *   row  8:       .  .  .  .  .  .  .  .  .  .  1
 *   row  7:       .  .  .  .  .  .  .  .  .  .  1
 *   row  6:       .  .  .  1  .  .  1  .  .  .  1
 *   row  5:       .  .  /  /  /  /  /  /  /  /  /
 *                 ^                              ^
 *              corner                        sentinels
 *
 * shifted_movers = (movers._[1] << 1) | carryover
 *
 *   The carryover injects a 1 at bit 0 (col 9 of row 5).
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       .  .  .  .  .  .  .  .  .  .  .
 *   row  9:       .  .  .  .  .  .  .  .  .  .  .
 *   row  8:       .  .  .  .  .  .  .  .  .  .  .
 *   row  7:       .  .  .  .  .  .  .  .  .  .  .
 *   row  6:       .  .  .  .  .  1  .  .  .  .  .
 *   row  5:       .  1  /  /  /  /  /  /  /  /  /
 *
 * subtraction = blockers - shifted_movers
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       1  .  .  .  .  .  .  .  .  .  1
 *   row  9:       .  .  .  .  .  .  .  .  .  .  1
 *   row  8:       .  .  .  .  .  .  .  .  .  .  1
 *   row  7:       .  .  .  .  .  .  .  .  .  .  1
 *   row  6:       .  .  .  .  1  1  1  .  .  .  .
 *   row  5:       1  1  /  /  /  /  /  /  /  /  /
 *
 * dests = subtraction & ~blockers
 *
 *   No throne mask needed (_[1] doesn't contain the throne).
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       .  .  .  .  .  .  .  .  .  .  .
 *   row  9:       .  .  .  .  .  .  .  .  .  .  .
 *   row  8:       .  .  .  .  .  .  .  .  .  .  .
 *   row  7:       .  .  .  .  .  .  .  .  .  .  .
 *   row  6:       .  .  .  .  1  1  .  .  .  .  .
 *   row  5:       1  1  /  /  /  /  /  /  /  /  /
 */
static inline layer leftward_moves_layer(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  u64 carryover;
  // lower
  {
    u64 blockers = occ._[0] | file_mask_0._[0];
    u64 shifted_movers = movers._[0] << 1;
    u64 subtraction = blockers - shifted_movers;
    u64 dests = subtraction & ~(blockers | throne._[0]);
    u64 boundary_bit = dests | (movers._[0] & ((u64)1 << 63));
    carryover = (boundary_bit >> 63);
    output._[0] = dests;
  }

  // upper
  {
    u64 blockers = occ._[1] | file_mask_0._[1];
    u64 shifted_movers = (movers._[1] << 1) | carryover;
    u64 subtraction = blockers - shifted_movers;
    u64 dests = subtraction & ~blockers;
    output._[1] = dests;
  }

  return output;
}

/*
 * rightward_moves_layer - Generate all rightward move destinations for all
 * movers simultaneously.
 *
 * occ must include corners so that they act as blockers, preventing
 * move generation into corner squares.
 *
 * "Rightward" = towards column 0 = towards lower bit index within each row.
 * In diagrams, "/" marks squares belonging to the other half.
 *
 * Binary subtraction propagates borrows towards the MSB (leftward), so we
 * cannot use the simple shift-and-subtract approach from leftward_moves_layer.
 * Instead, pext/pdep remaps each mover to a position just left of its nearest
 * rightward blocker; subtracting this from the original movers generates the
 * rightward ray via borrow propagation through the gap between them, turning
 * all intermediate 0-bits into 1-bits (destinations).
 *
 * The upper half is processed first (rightward carryover flows from _[1]
 * into _[0], the opposite direction of leftward).
 *
 *
 * ====== UPPER HALF: _[1] (rows 5-10)
 *
 * _[1] bit layout (bit = index - 64):
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10 bits: 56 55 54 53 52 51 50 49 48 47 46
 *   row  9 bits: 45 44 43 42 41 40 39 38 37 36 35
 *   row  8 bits: 34 33 32 31 30 29 28 27 26 25 24
 *   row  7 bits: 23 22 21 20 19 18 17 16 15 14 13
 *   row  6 bits: 12 11 10  9  8  7  6  5  4  3  2
 *   row  5 bits:  1  0  /  /  /  /  /  /  /  /  /
 *
 * Example — movers (M), other pieces (X), corners (C):
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       C  .  .  .  .  .  .  .  .  .  C
 *   row  9:       .  .  .  .  .  .  .  .  .  .  .
 *   row  8:       .  .  .  .  .  M  .  .  X  .  .
 *   row  7:       .  .  .  .  .  .  .  M  .  .  .
 *   row  6:       .  .  .  .  .  .  .  .  .  .  .
 *   row  5:       .  M  /  /  /  /  /  /  /  /  /
 *
 * blockers = occ._[1] | file_mask_10._[1]
 *
 *   Column-10 sentinels prevent the pext/pdep mapping from crossing
 *   row boundaries. Each sentinel also serves as the rightward
 *   boundary for the row above it: the sentinel at col 10 of row N
 *   is one bit below row (N+1)'s col 0, so a rightward ray in row
 *   N+1 naturally stops there.
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       1  .  .  .  .  .  .  .  .  .  1
 *   row  9:       1  .  .  .  .  .  .  .  .  .  .
 *   row  8:       1  .  .  .  .  1  .  .  1  .  .
 *   row  7:       1  .  .  .  .  .  .  1  .  .  .
 *   row  6:       1  .  .  .  .  .  .  .  .  .  .
 *   row  5:       1  1  /  /  /  /  /  /  /  /  /
 *                 ^                             ^
 *             sentinels                      corner
 *
 * extracted_movers = _pext_u64(movers._[1], blockers)
 *
 *   Since movers are a subset of occ (and therefore blockers), pext
 *   compresses the movers into blocker-index space: bit k of the
 *   result is 1 if the k-th blocker (sorted by position) has a mover.
 *
 *     index: 10  9   8   7   6   5   4   3   2   1   0
 *            .   .   .   .   1   .   .   1   .   .   1
 *
 * deposit_mask = 1 | (blockers << 1)
 *
 *   Shifting blockers left by 1 creates a position one to the LEFT of
 *   each blocker. The `1 |` at bit 0 serves two independent purposes
 *   simultaneously:
 *
 *   (a) Off-balancing: without it, the deposit mask would have the
 *       same number of set bits as the extraction mask (blockers), so
 *       pdep would map each mover back to its own blocker's shifted
 *       position — useless. The extra bit off-balances the deposit
 *       mask: every deposit target is shifted down by one slot in the
 *       ordering. A mover at blocker index k deposits at
 *       (blocker k-1) + 1 instead of (blocker k) + 1. This is
 *       exactly "one left of the nearest rightward blocker."
 *
 *   (b) Row 5 boundary: row 5 is the bottommost row in _[1]. Unlike
 *       other rows, it has no sentinel from a row below (row 4 is in
 *       _[0]), so without the `1` a mover in row 5 would have no
 *       valid deposit target.
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       .  .  .  .  .  .  .  .  .  1  1
 *   row  9:       .  .  .  .  .  .  .  .  .  .  1
 *   row  8:       .  .  .  .  1  .  .  1  .  .  1
 *   row  7:       .  .  .  .  .  .  1  .  .  .  1
 *   row  6:       .  .  .  .  .  .  .  .  .  .  1
 *   row  5:       1  1  /  /  /  /  /  /  /  /  /
 *
 * deposited_movers = _pdep_u64(extracted_movers, deposit_mask)
 *
 *   pdep deposits each mover (in blocker-index space) back into board
 *   space using the deposit_mask. The off-balanced mapping places each
 *   mover one left of its nearest rightward blocker.
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       .  .  .  .  .  .  .  .  .  .  .
 *   row  9:       .  .  .  .  .  .  .  .  .  .  .
 *   row  8:       .  .  .  .  .  .  .  1  .  .  .
 *   row  7:       .  .  .  .  .  .  .  .  .  .  1
 *   row  6:       .  .  .  .  .  .  .  .  .  .  .
 *   row  5:       .  1  /  /  /  /  /  /  /  /  /
 *
 * dests = movers._[1] - deposited_movers
 *
 *   The subtraction propagates borrows from each deposited_mover
 *   towards the MSB. Each 0-bit along the way is flipped to 1; the
 *   first 1-bit (the mover itself) absorbs the borrow (1-0-1=0).
 *   The result is the rightward ray between each mover and its
 *   nearest rightward blocker, exclusive of both the mover and
 *   the blocker. In row 5,
 *   deposited_movers equals movers (1-1=0), so no dests are
 *   generated — its rightward ray continues via the carryover.
 *
 *           col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 10:       .  .  .  .  .  .  .  .  .  .  .
 *   row  9:       .  .  .  .  .  .  .  .  .  .  .
 *   row  8:       .  .  .  .  .  .  1  1  .  .  .
 *   row  7:       .  .  .  .  .  .  .  .  1  1  1
 *   row  6:       .  .  .  .  .  .  .  .  .  .  .
 *   row  5:       .  .  /  /  /  /  /  /  /  /  /
 *
 * carryover = ((dests | movers._[1]) << 63) & ~blockers
 *
 *   Row 5 is split: cols 9-10 in _[1] (bits 0-1), cols 0-8 in
 *   _[0] (bits 55-63). A mover at bit 0 — or a dest that reaches
 *   bit 0 — needs to continue into _[0]. Shifting left by 63
 *   moves bit 0 to the MSB of _[0] (bit 63, row 5 col 8).
 *
 *   The `& ~blockers` check is necessary here (unlike leftward).  In
 *   the leftward case, if the boundary square is occupied, the
 *   subtraction naturally produces 1-1=0 — the blocker and the
 *   shifted carryover meet at the same bit, so no borrow propagates
 *   and no ray is generated. In the rightward case, pext/pdep remaps
 *   the carryover to a deposit position to the right of bit 63, which
 *   means that bit 63 being occupied will not prevent a borrow
 *   propagation, and a spurious ray will be generated. The explicit
 *   occupancy check prevents this.
 *
 * ====== LOWER HALF: _[0] (rows 0-5)
 *
 * _[0] bit layout (bit = index):
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5 bits:  /  / 63 62 61 60 59 58 57 56 55
 *   row 4 bits: 54 53 52 51 50 49 48 47 46 45 44
 *   row 3 bits: 43 42 41 40 39 38 37 36 35 34 33
 *   row 2 bits: 32 31 30 29 28 27 26 25 24 23 22
 *   row 1 bits: 21 20 19 18 17 16 15 14 13 12 11
 *   row 0 bits: 10  9  8  7  6  5  4  3  2  1  0
 *
 * Continuing the example with carryover at bit 63 (M=mover, X=other piece,
 * C=corner):
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  .  .  .  .
 *   row 4:       .  .  .  .  .  .  .  .  .  .  .
 *   row 3:       .  .  .  .  .  .  .  .  .  .  .
 *   row 2:       .  .  .  M  .  .  X  .  .  .  .
 *   row 1:       .  .  .  .  .  .  .  .  .  .  .
 *   row 0:       C  .  .  .  .  .  .  .  .  .  C
 *
 * blockers = occ._[0] | file_mask_10._[0]
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  .  .  .  .
 *   row 4:       1  .  .  .  .  .  .  .  .  .  .
 *   row 3:       1  .  .  .  .  .  .  .  .  .  .
 *   row 2:       1  .  .  1  .  .  1  .  .  .  .
 *   row 1:       1  .  .  .  .  .  .  .  .  .  .
 *   row 0:       1  .  .  .  .  .  .  .  .  .  1
 *                ^                             ^
 *            sentinels                      corner
 *
 * combined_movers   = carryover | movers._[0]
 * combined_blockers = carryover | blockers
 *
 *   The carryover injects a virtual mover at bit 63 (row 5, col 8)
 *   into both movers and blockers for the pext step.
 *
 * extracted_movers = _pext_u64(combined_movers, combined_blockers) >> 1
 *
 *   Same compression as the upper half. The >> 1 achieves the same
 *   off-balancing as the upper half's `1 |`, but from the other end:
 *   `1 |` adds an extra position at the bottom of the deposit mask;
 *   `>> 1` removes a position from the bottom of the extraction.
 *   Both shift the mover-to-deposit-slot mapping down by one.
 *
 *   This exploits a property of the board: the lower right corner
 *   (bit 0, row 0, col 0) is always in occ but can never be a mover.
 *   It serves two roles simultaneously:
 *   1. Boundary: shifted left via `blockers << 1`, the corner
 *      creates a deposit target at bit 1 — the rightward boundary
 *      for row 0, just as `1 |` provides for row 5 in the upper
 *      half.
 *   2. Off-balancing: the >> 1 discards the lowest extraction bit,
 *      which always corresponds to the corner. Since no mover can
 *      be at a corner, this bit is guaranteed to be 0, and is thus safe to
 *      discard.
 *
 *   extracted (before >> 1):
 *     index:  8   7   6   5   4   3   2   1   0
 *             1   .   .   .   1   .   .   .   .
 *
 *   extracted >> 1 (index 0 discarded):
 *     index:  8   7   6   5   4   3   2   1   0
 *             .   1   .   .   .   1   .   .   .
 *
 * deposited_movers = _pdep_u64(extracted_movers, blockers << 1)
 *
 *   blockers << 1 (deposit mask):
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  .  .  .  1
 *   row 4:       .  .  .  .  .  .  .  .  .  .  1
 *   row 3:       .  .  .  .  .  .  .  .  .  .  1
 *   row 2:       .  .  1  .  .  1  .  .  .  .  1
 *   row 1:       .  .  .  .  .  .  .  .  .  .  1
 *   row 0:       .  .  .  .  .  .  .  .  .  1  .
 *
 *   deposited_movers:
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  .  .  .  .  .  .  .  .  1
 *   row 4:       .  .  .  .  .  .  .  .  .  .  .
 *   row 3:       .  .  .  .  .  .  .  .  .  .  .
 *   row 2:       .  .  .  .  .  1  .  .  .  .  .
 *   row 1:       .  .  .  .  .  .  .  .  .  .  .
 *   row 0:       .  .  .  .  .  .  .  .  .  .  .
 *
 * dests = (movers._[0] - deposited_movers) & ~throne._[0]
 *
 *   The subtraction uses movers._[0], not combined_movers: the
 *   carryover position (bit 63, row 5 col 8) is a valid destination
 *   square, not a real mover. Including it in the minuend would
 *   absorb the borrow there, wrongly excluding it from dests. By
 *   omitting it, the borrow from the carryover's deposited position
 *   (bit 55) overflows the u64, and bit 63 correctly appears as a
 *   dest — same harmless overflow as the leftward row 5 case.
 *
 *          col: 10  9  8  7  6  5  4  3  2  1  0
 *   row 5:       /  /  1  1  1  .  1  1  1  1  1  (throne at col 5 masked)
 *   row 4:       .  .  .  .  .  .  .  .  .  .  .
 *   row 3:       .  .  .  .  .  .  .  .  .  .  .
 *   row 2:       .  .  .  .  .  1  1  .  .  .  .
 *   row 1:       .  .  .  .  .  .  .  .  .  .  .
 *   row 0:       .  .  .  .  .  .  .  .  .  .  .
 */
static inline layer rightward_moves_layer(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  u64 carryover;
  // upper
  {
    u64 blockers = occ._[1] | file_mask_10._[1];
    u64 extracted_movers = _pext_u64(movers._[1], blockers);
    u64 deposit_mask = 1 | (blockers << 1);
    u64 deposited_movers = _pdep_u64(extracted_movers, deposit_mask);
    u64 dests = movers._[1] - deposited_movers;
    carryover = (dests | movers._[1]) << 63;
    output._[1] = dests;
  }

  // lower
  {
    u64 blockers = occ._[0] | file_mask_10._[0];
    carryover &= ~blockers;
    // here I depend on the lower right corner being occupied to ensure that I
    // generate a ray towards it
    u64 combined_movers = carryover | movers._[0];
    u64 combined_blockers = carryover | blockers;
    u64 extracted_movers = _pext_u64(combined_movers, combined_blockers) >> 1;
    u64 deposited_movers = _pdep_u64(extracted_movers, blockers << 1);
    u64 dests = (movers._[0] - deposited_movers) & ~throne._[0];
    output._[0] = dests;
  }

  return output;
}

layer rightward_moves_layer_king(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  // lower
  {

    u64 blockers = occ._[0] | file_mask_10._[0];
    u64 movers_ext = _pext_u64(movers._[0], blockers);
    u64 movers_dep = _pdep_u64(movers_ext, 1 | (blockers << 1));
    u64 move_mask = movers._[0] - movers_dep;
    // I might be able to get away with not using LOWER_HALF_MASK here
    output._[0] = move_mask;
  }

  // upper
  {
    u64 blockers = (occ._[1] | file_mask_10._[1]);
    u64 movers_ext = _pext_u64(movers._[1], blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, blockers << 1);
    u64 subtracted = movers._[1] - movers_dep;
    output._[1] = subtracted & UPPER_HALF_MASK;
  }

  // center
  {
    u16 blockers = GET_CENTER_ROW(occ);
    u16 movers_row = GET_CENTER_ROW(movers);
    u16 movers_ext = _pext_u32(movers_row, 1 | blockers) >> 1;
    u16 movers_dep = _pdep_u32(movers_ext, 1 | (blockers << 1));
    u16 move_mask = movers_row - movers_dep;
    SET_CENTER_ROW(output, move_mask);
  }

  return output;
}

/* The idea for this will be to generate the whole struct up front and provide
 * it to search functions. These functions can then mask the portions they need,
 * such as king captures or other tactical moves, and then extract moves from
 * this sub-selection. We'll also calculate the remainder, so that follow-up
 * functions don't revisit explored moves. This paradigm also supports a
 * generator-style approach to extraction that allows stopping before all moves
 * have been extracted, albiet requiring more storage than the existing
 * implementation. */

bool move_state_from_cursor(
    const move_layers *layers,
    layer movers,
    layer movers_r,
    move_cursor cursor,
    move_state *result) {
  layer movers_array[2] = {movers, movers_r};
  static const u8 movers_rotation_map[12] =
      {0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1};

  // Handle cursors 0-7 (LOWER/UPPER cases) with direct array access
  for (move_cursor current_cursor = cursor; current_cursor < CENTER_LEFTWARD;
       current_cursor++) {
    u64 part = layers->u64s[current_cursor]
               & (current_cursor & 1 ? UPPER_HALF_MASK : LOWER_HALF_MASK);
    if (part != 0) {
      result->current = part;
      result->cursor = current_cursor;
      result->movers = movers_array[movers_rotation_map[current_cursor]]
                           ._[current_cursor & 1];
      return true;
    }
  }

  // Handle CENTER cases (8-11) with GET_CENTER_ROW
  for (move_cursor current_cursor =
           (cursor < CENTER_LEFTWARD ? CENTER_LEFTWARD : cursor);
       current_cursor <= CENTER_RIGHTWARD_R;
       current_cursor++) {
    int center_layer_index = current_cursor - CENTER_LEFTWARD;
    u64 part = GET_CENTER_ROW(layers->layers[center_layer_index]);
    if (part != 0) {
      result->current = part;
      result->cursor = current_cursor;
      result->movers =
          GET_CENTER_ROW(movers_array[movers_rotation_map[current_cursor]]);
      return true;
    }
  }

  return false;
}

move_state init_move_state(const move_layers *layers, layer movers) {
  move_state result = {
      .cursor = LOWER_LEFTWARD,
      .movers = movers._[0] & LOWER_HALF_MASK,
      .current = layers->leftward._[0] & LOWER_HALF_MASK};
  return result;
}

// -----------------------------------------------------------------------------
// Move extraction macros and functions

#define LEFTWARD_EXTRACT(_i, _r)                                               \
  do {                                                                         \
    u64 dest_bit = _blsi_u64(*dests);                                          \
    u8 dest = _tzcnt_u64(dest_bit);                                            \
    OFFSET(dest, _i);                                                          \
    u8 orig = 63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & movers);                 \
    __attribute__((unused)) u64 orig_bit = (u64)1 << orig;                     \
    OFFSET(orig, _i);                                                          \
    u8 orig_r = ROTATE_DIR(_r)[orig];                                          \
    u8 dest_r = ROTATE_DIR(_r)[dest];                                          \
    ASSIGN##_r(_i, orig, dest, orig_r, dest_r, orig_bit, dest_bit, *result);   \
    *dests -= dest_bit;                                                        \
  } while (0)

#define ASSIGN(_i, orig, dest, orig_r, dest_r, orig_bit, dest_bit, result)     \
  (result).m = (move){orig, dest};                                             \
  (result).l._[_i] = orig_bit | dest_bit;                                      \
  SET_INDEX((result).l_r, orig_r);                                             \
  SET_INDEX((result).l_r, dest_r);

#define ASSIGN_r(_i, orig, dest, orig_r, dest_r, orig_bit, dest_bit, result)   \
  (result).m = (move){orig_r, dest_r};                                         \
  SET_INDEX((result).l_r, orig);                                               \
  SET_INDEX((result).l_r, dest);                                               \
  SET_INDEX((result).l, orig_r);                                               \
  SET_INDEX((result).l, dest_r);

#define RIGHTWARD_EXTRACT(_i, _r)                                              \
  do {                                                                         \
    u64 dest_bit = _blsi_u64(*dests);                                          \
    u8 dest = _tzcnt_u64(dest_bit);                                            \
    OFFSET(dest, _i);                                                          \
    u64 orig_bit = _blsi_u64(movers & -dest_bit);                              \
    u8 orig = _tzcnt_u64(orig_bit);                                            \
    OFFSET(orig, _i);                                                          \
    u8 orig_r = ROTATE_DIR(_r)[orig];                                          \
    u8 dest_r = ROTATE_DIR(_r)[dest];                                          \
    ASSIGN##_r(_i, orig, dest, orig_r, dest_r, orig_bit, dest_bit, *result);   \
    *dests -= dest_bit;                                                        \
  } while (0)

#define LEFTWARD_CENTER_EXTRACT(_r)                                            \
  do {                                                                         \
    u16 dest_bit = *dests & -*dests;                                           \
    u8 dest = _tzcnt_u16(dest_bit);                                            \
    dest += 55;                                                                \
    u8 orig = 15 - __lzcnt16((dest_bit - 1) & movers);                         \
    orig += 55;                                                                \
    u8 orig_r = ROTATE_DIR(_r)[orig];                                          \
    u8 dest_r = ROTATE_DIR(_r)[dest];                                          \
    ASSIGN_CENTER##_r(orig, dest, orig_r, dest_r, *result);                    \
    *dests -= dest_bit;                                                        \
  } while (0)

#define ASSIGN_CENTER(orig, dest, orig_r, dest_r, result)                      \
  (result).m = (move){orig, dest};                                             \
  SET_INDEX((result).l, orig);                                                 \
  SET_INDEX((result).l, dest);                                                 \
  SET_INDEX((result).l_r, orig_r);                                             \
  SET_INDEX((result).l_r, dest_r);

#define ASSIGN_CENTER_r(orig, dest, orig_r, dest_r, result)                    \
  (result).m = (move){orig_r, dest_r};                                         \
  SET_INDEX((result).l_r, orig);                                               \
  SET_INDEX((result).l_r, dest);                                               \
  SET_INDEX((result).l, orig_r);                                               \
  SET_INDEX((result).l, dest_r);

#define RIGHTWARD_CENTER_EXTRACT(_r)                                           \
  do {                                                                         \
    u16 dest_bit = *dests & -*dests;                                           \
    u8 dest = _tzcnt_u16(dest_bit);                                            \
    dest += 55;                                                                \
    u8 orig = _tzcnt_u16(movers & -dest_bit) + 55;                             \
    u8 orig_r = ROTATE_DIR(_r)[orig];                                          \
    u8 dest_r = ROTATE_DIR(_r)[dest];                                          \
    ASSIGN_CENTER##_r(orig, dest, orig_r, dest_r, *result);                    \
    *dests -= dest_bit;                                                        \
  } while (0)

void extract_lower_leftward(u64 *dests, u64 movers, move_data *result) {
  LEFTWARD_EXTRACT(0, );
}

void extract_upper_leftward(u64 *dests, u64 movers, move_data *result) {
  LEFTWARD_EXTRACT(1, );
}

void extract_lower_leftward_r(u64 *dests, u64 movers, move_data *result) {
  LEFTWARD_EXTRACT(0, _r);
}

void extract_upper_leftward_r(u64 *dests, u64 movers, move_data *result) {
  LEFTWARD_EXTRACT(1, _r);
}

void extract_lower_rightward(u64 *dests, u64 movers, move_data *result) {
  RIGHTWARD_EXTRACT(0, );
}

void extract_upper_rightward(u64 *dests, u64 movers, move_data *result) {
  RIGHTWARD_EXTRACT(1, );
}

void extract_lower_rightward_r(u64 *dests, u64 movers, move_data *result) {
  RIGHTWARD_EXTRACT(0, _r);
}

void extract_upper_rightward_r(u64 *dests, u64 movers, move_data *result) {
  RIGHTWARD_EXTRACT(1, _r);
}

void extract_center_leftward(u64 *dests, u64 movers, move_data *result) {
  LEFTWARD_CENTER_EXTRACT();
}

void extract_center_leftward_r(u64 *dests, u64 movers, move_data *result) {
  LEFTWARD_CENTER_EXTRACT(_r);
}

void extract_center_rightward(u64 *dests, u64 movers, move_data *result) {
  RIGHTWARD_CENTER_EXTRACT();
}

void extract_center_rightward_r(u64 *dests, u64 movers, move_data *result) {
  RIGHTWARD_CENTER_EXTRACT(_r);
}

void extract_move(
    move_cursor cursor,
    u64 *dests,
    u64 movers,
    move_data *result) {
  static void *dispatch_table[12] = {
      [LOWER_LEFTWARD] = &&lower_leftward,
      [UPPER_LEFTWARD] = &&upper_leftward,
      [LOWER_LEFTWARD_R] = &&lower_leftward_r,
      [UPPER_LEFTWARD_R] = &&upper_leftward_r,
      [LOWER_RIGHTWARD] = &&lower_rightward,
      [UPPER_RIGHTWARD] = &&upper_rightward,
      [LOWER_RIGHTWARD_R] = &&lower_rightward_r,
      [UPPER_RIGHTWARD_R] = &&upper_rightward_r,
      [CENTER_LEFTWARD] = &&center_leftward,
      [CENTER_LEFTWARD_R] = &&center_leftward_r,
      [CENTER_RIGHTWARD] = &&center_rightward,
      [CENTER_RIGHTWARD_R] = &&center_rightward_r};

  goto *dispatch_table[cursor];

lower_leftward:
  LEFTWARD_EXTRACT(0, );
  return;
upper_leftward:
  LEFTWARD_EXTRACT(1, );
  return;
lower_leftward_r:
  LEFTWARD_EXTRACT(0, _r);
  return;
upper_leftward_r:
  LEFTWARD_EXTRACT(1, _r);
  return;
lower_rightward:
  RIGHTWARD_EXTRACT(0, );
  return;
upper_rightward:
  RIGHTWARD_EXTRACT(1, );
  return;
lower_rightward_r:
  RIGHTWARD_EXTRACT(0, _r);
  return;
upper_rightward_r:
  RIGHTWARD_EXTRACT(1, _r);
  return;
center_leftward:
  LEFTWARD_CENTER_EXTRACT();
  return;
center_leftward_r:
  LEFTWARD_CENTER_EXTRACT(_r);
  return;
center_rightward:
  RIGHTWARD_CENTER_EXTRACT();
  return;
center_rightward_r:
  RIGHTWARD_CENTER_EXTRACT(_r);
  return;
}

bool next_move_from_layers(
    const move_layers *layers,
    layer movers,
    layer movers_r,
    move_state *state,
    move_data *result) {
  // If current is empty, try to advance to next non-empty state
  if (state->current == 0) {
    if (!move_state_from_cursor(
            layers,
            movers,
            movers_r,
            state->cursor + 1,
            state)) {
      return false;
    }
  }

  // Extract move using current state (this will update state->current)
  extract_move(state->cursor, &state->current, state->movers, result);

  return true;
}

// -----------------------------------------------------------------------------
// Move count

static inline int leftward_moves_count_king(layer movers, layer occ) {
  int output = 0;

  // lower
  if (movers._[0] & LOWER_HALF_MASK) {
    u64 blockers = occ._[0] | file_mask_0._[0];
    // we & ~blockers to remove all blockers that haven't been bit flipped by
    // a substraction, so that all we're left with are subtraction rays we &
    // LOWER_HALF_MASK to remove anything in the center row
    u64 dests = (blockers - ((movers._[0]) << 1)) & ~blockers;
    // print_layer((layer){dests, 0});
    return __builtin_popcountll(dests);
    // printf("output: %d\n", output);
  }

  if (movers._[1] & UPPER_HALF_MASK) {
    u64 blockers = occ._[1] | file_mask_0._[1];
    // I need the upper half mask here to keep the king from passing through the
    // NW corner into the top 7 bits
    u64 dests = (blockers - ((movers._[1]) << 1)) & ~blockers & UPPER_HALF_MASK;
    // print_layer((layer){0, dests});
    return __builtin_popcountll(dests);
    // printf("output: %d\n", output);
  }

  // center
  if (GET_CENTER_ROW(movers)) {
    u16 blockers = GET_CENTER_ROW(occ);
    u64 dests =
        (blockers - (GET_CENTER_ROW(movers) << 1)) & ~blockers & 0b11111111111;
    // print_row((u16)dests);
    return __builtin_popcount(dests);
    // printf("output: %d\n", output);
  }

  return output;
}

static inline int leftward_moves_count(layer movers, layer occ) {
  return LAYER_POPCOUNT(leftward_moves_layer(movers, occ));
}

static inline int rightward_moves_count(layer movers, layer occ) {
  return LAYER_POPCOUNT(rightward_moves_layer(movers, occ));
}

int black_moves_count(const board *b) {
  int total = 0;
  const layer occ = board_occ(*b);
  const layer occ_r = board_occ_r(*b);
  total += leftward_moves_count(b->black, occ);
  total += leftward_moves_count(b->black_r, occ_r);
  total += rightward_moves_count(b->black, occ);
  total += rightward_moves_count(b->black_r, occ_r);
  return total;
}

int white_moves_count(const board *b) {
  int total = 0;
  const layer occ = board_occ(*b);
  const layer occ_r = board_occ_r(*b);
  total += leftward_moves_count(b->white, occ);
  total += leftward_moves_count(b->white_r, occ_r);
  total += rightward_moves_count(b->white, occ);
  total += rightward_moves_count(b->white_r, occ_r);
  return total;
}

static inline int rightward_moves_count_king(layer movers, layer occ) {
  int output = 0;

  if (movers._[0] & LOWER_HALF_MASK) {
    u64 blockers = occ._[0] | file_mask_10._[0];
    u64 movers_ext = _pext_u64(movers._[0], blockers);
    u64 movers_dep = _pdep_u64(movers_ext, 1 | (blockers << 1));
    // I use lower half mask here to prevent generating moves in the center
    // row
    u64 move_mask = (movers._[0] - movers_dep);
    return __builtin_popcountll(move_mask);
  }

  // upper
  if (movers._[1] & UPPER_HALF_MASK) {
    u64 blockers = (occ._[1] | file_mask_10._[1]);
    u64 movers_ext = _pext_u64(movers._[1], blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, blockers << 1);
    // I use upper half mask here to prevent generating moves in the center
    // row
    u64 move_mask = (movers._[1] - movers_dep) & UPPER_HALF_MASK;
    return __builtin_popcountll(move_mask);
  }

  // center
  if (GET_CENTER_ROW(movers)) {
    u16 blockers = GET_CENTER_ROW(occ);
    // print_row(blockers);

    // I can just remove the lowest bit because it by definition can't move
    // anywhere
    u16 movers_row = GET_CENTER_ROW(movers) & 0b11111111110;
    u16 tail = (blockers & 1) ^ 1;

    u16 movers_ext = _pext_u32(movers_row, tail | blockers) >> 1;
    u16 movers_dep = _pdep_u32(movers_ext, tail | (blockers << 1));
    u16 move_mask = (movers_row - movers_dep) & 0b11111111111;
    return __builtin_popcount(move_mask);
  }

  return output;
}

int king_moves_count(const board *b) {
  int total = 0;
  layer occ = king_board_occ(*b);
  layer occ_r = king_board_occ_r(*b);
  total += leftward_moves_count_king(b->king, occ);
  total += leftward_moves_count_king(b->king_r, occ_r);
  total += rightward_moves_count_king(b->king, occ);
  total += rightward_moves_count_king(b->king_r, occ_r);
  return total;
}

move_layers generate_black_move_layers(const board *b) {
  move_layers result = {0};
  layer occ = board_occ(*b);
  layer occ_r = board_occ_r(*b);
  result.leftward = leftward_moves_layer(b->black, occ);
  result.leftward_r = leftward_moves_layer(b->black_r, occ_r);
  result.rightward = rightward_moves_layer(b->black, occ);
  result.rightward_r = rightward_moves_layer(b->black_r, occ_r);
  return result;
}

move_layers generate_white_move_layers(const board *b) {
  move_layers result = {0};
  layer occ = board_occ(*b);
  layer occ_r = board_occ_r(*b);
  result.leftward = leftward_moves_layer(b->white, occ);
  result.leftward_r = leftward_moves_layer(b->white_r, occ_r);
  result.rightward = rightward_moves_layer(b->white, occ);
  result.rightward_r = rightward_moves_layer(b->white_r, occ_r);
  return result;
}

void mask_move_layers(layer l, layer l_r, move_layers *layers) {
  LAYER_AND_ASSG(layers->leftward, l);
  LAYER_AND_ASSG(layers->leftward_r, l_r);
  LAYER_AND_ASSG(layers->rightward, l);
  LAYER_AND_ASSG(layers->rightward_r, l_r);
}

void subtract_move_layers(move_layers *target, const move_layers *subtract) {
  target->leftward._[0] -= subtract->leftward._[0];
  target->leftward._[1] -= subtract->leftward._[1];
  target->leftward_r._[0] -= subtract->leftward_r._[0];
  target->leftward_r._[1] -= subtract->leftward_r._[1];
  target->rightward._[0] -= subtract->rightward._[0];
  target->rightward._[1] -= subtract->rightward._[1];
  target->rightward_r._[0] -= subtract->rightward_r._[0];
  target->rightward_r._[1] -= subtract->rightward_r._[1];
}

#define FIND_ORIG_LEFTWARD(_movers, _i, _dest_bit)                             \
  u8 orig = 63 - _lzcnt_u64(_blsmsk_u64(_dest_bit) & _movers._[_i]);           \
  __attribute__((unused)) u64 orig_bit = (u64)1 << orig;

#define FIND_ORIG_RIGHTWARD(_movers, _i, _dest_bit)                            \
  u64 orig_bit = _blsi_u64(_movers._[_i] & -_dest_bit);                        \
  u8 orig = _tzcnt_u64(orig_bit);

#define FIND_ORIG_CENTER_LEFTWARD(_movers, _dest_bit)                          \
  u8 orig = 15 - __lzcnt16((_dest_bit - 1) & GET_CENTER_ROW(_movers));         \
  orig += 55;

#define FIND_ORIG_CENTER_RIGHTWARD(_movers, _dest_bit)                         \
  u8 orig = _tzcnt_u16(GET_CENTER_ROW(_movers) & -_dest_bit) + 55;

#define EXTRACT_FROM_LAYERS_COMMON(_i, _r, _direction, _movers, _find_orig)    \
  {                                                                            \
    u64 dests = layers->_direction##_r._[_i] & HALF_MASK_##_i;                 \
    while (dests) {                                                            \
      u64 dest_bit = _blsi_u64(dests);                                         \
      u8 dest = _tzcnt_u64(dest_bit);                                          \
      OFFSET(dest, _i);                                                        \
      _find_orig(_movers, _i, dest_bit) OFFSET(orig, _i);                      \
      u8 orig_r = ROTATE_DIR(_r)[orig];                                        \
      u8 dest_r = ROTATE_DIR(_r)[dest];                                        \
      BOOKKEEP##_r(_i);                                                        \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }

#define EXTRACT_FROM_LAYERS_CENTER_COMMON(_r, _direction, _movers, _find_orig) \
  {                                                                            \
    u16 dests = GET_CENTER_ROW(layers->_direction##_r);                        \
    while (dests) {                                                            \
      u16 dest_bit = dests & -dests;                                           \
      u8 dest = _tzcnt_u16(dest_bit);                                          \
      dest += 55;                                                              \
      _find_orig(_movers, dest_bit) u8 orig_r = ROTATE_DIR(_r)[orig];          \
      u8 dest_r = ROTATE_DIR(_r)[dest];                                        \
      BOOKKEEP_CENTER##_r();                                                   \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }

#define EXTRACT_FROM_LAYERS_LEFTWARD(_i, _r, _movers)                          \
  EXTRACT_FROM_LAYERS_COMMON(_i, _r, leftward, _movers, FIND_ORIG_LEFTWARD)

#define EXTRACT_FROM_LAYERS_RIGHTWARD(_i, _r, _movers)                         \
  EXTRACT_FROM_LAYERS_COMMON(_i, _r, rightward, _movers, FIND_ORIG_RIGHTWARD)

#define EXTRACT_FROM_LAYERS_LEFTWARD_CENTER(_r, _movers)                       \
  EXTRACT_FROM_LAYERS_CENTER_COMMON(                                           \
      _r,                                                                      \
      leftward,                                                                \
      _movers,                                                                 \
      FIND_ORIG_CENTER_LEFTWARD)

#define EXTRACT_FROM_LAYERS_RIGHTWARD_CENTER(_r, _movers)                      \
  EXTRACT_FROM_LAYERS_CENTER_COMMON(                                           \
      _r,                                                                      \
      rightward,                                                               \
      _movers,                                                                 \
      FIND_ORIG_CENTER_RIGHTWARD)

void moves_from_layers(
    const move_layers *layers,
    const layer movers,
    const layer movers_r,
    move *ms,
    layer *ls,
    layer *ls_r,
    int *total) {

  int _local_total = *total;
  int *_total_out = total;
  total = &_local_total;

  EXTRACT_FROM_LAYERS_LEFTWARD(0, , movers);
  EXTRACT_FROM_LAYERS_LEFTWARD(1, , movers);
  EXTRACT_FROM_LAYERS_LEFTWARD_CENTER(, movers);

  EXTRACT_FROM_LAYERS_LEFTWARD(0, _r, movers_r);
  EXTRACT_FROM_LAYERS_LEFTWARD(1, _r, movers_r);
  EXTRACT_FROM_LAYERS_LEFTWARD_CENTER(_r, movers_r);

  EXTRACT_FROM_LAYERS_RIGHTWARD(0, , movers);
  EXTRACT_FROM_LAYERS_RIGHTWARD(1, , movers);
  EXTRACT_FROM_LAYERS_RIGHTWARD_CENTER(, movers);

  EXTRACT_FROM_LAYERS_RIGHTWARD(0, _r, movers_r);
  EXTRACT_FROM_LAYERS_RIGHTWARD(1, _r, movers_r);
  EXTRACT_FROM_LAYERS_RIGHTWARD_CENTER(_r, movers_r);

  *_total_out = _local_total;
}

/* all black moves, with any illegal repetitions removed */
move *all_black_moves(board b, position_set *ps, int *total) {
  u64 board_hash = hash_for_board(b, false);
  moves_to_t results = {0};
  layer occ = board_occ(b);
  layer occ_r = board_occ_r(b);
  layer targets = LAYER_NOT(occ);
  layer targets_r = LAYER_NOT(occ_r);
  results.total = 0;
  moves_to(
      targets,
      targets_r,
      b.black,
      b.black_r,
      occ,
      occ_r,
      results.ms,
      results.ls,
      results.ls_r,
      &results.total);

  move *output = malloc(sizeof(move) * results.total);
  int j = 0;
  for (int i = 0; i < results.total; i++) {
    move m = results.ms[i];
    u64 hash = next_hash_black(board_hash, m.orig, m.dest);
    apply_captures_z_black(&b, &hash, m.dest);
    if (check_position(ps, hash)) {
      continue;
    }
    output[j] = m;
    j++;
  }

  *total = j;
  return output;
}

/* all white moves, with any illegal repetitions removed */
move *all_white_moves(board b, position_set *ps, int *total) {
  u64 board_hash = hash_for_board(b, false);
  moves_to_t results = {0};
  layer occ = board_occ(b);
  layer occ_r = board_occ_r(b);
  layer targets = LAYER_NOT(occ);
  layer targets_r = LAYER_NOT(occ_r);
  results.total = 0;
  moves_to(
      targets,
      targets_r,
      b.white,
      b.white_r,
      occ,
      occ_r,
      results.ms,
      results.ls,
      results.ls_r,
      &results.total);

  move *output = malloc(sizeof(move) * results.total);
  int j = 0;
  for (int i = 0; i < results.total; i++) {
    move m = results.ms[i];
    u64 hash = next_hash_white(board_hash, m.orig, m.dest);
    apply_captures_z_white(&b, &hash, m.dest);
    if (check_position(ps, hash)) {
      continue;
    }
    output[j] = m;
    j++;
  }

  *total = j;
  return output;
}

/* all king moves, with any illegal repetitions removed */
move *all_king_moves(board b, position_set *ps, int *total) {
  u64 board_hash = hash_for_board(b, false);
  moves_to_t results = {0};
  layer occ = king_board_occ(b);
  layer occ_r = king_board_occ_r(b);
  layer targets = LAYER_NOT(occ);
  layer targets_r = LAYER_NOT(occ_r);
  results.total = 0;
  moves_to_king_impl(
      targets,
      targets_r,
      b.king,
      b.king_r,
      occ,
      occ_r,
      results.ms,
      results.ls,
      results.ls_r,
      &results.total);

  move *output = malloc(sizeof(move) * results.total);
  int j = 0;
  for (int i = 0; i < results.total; i++) {
    move m = results.ms[i];
    u64 hash = next_hash_king(board_hash, m.orig, m.dest);
    apply_captures_z_white(&b, &hash, m.dest);
    if (check_position(ps, hash)) {
      continue;
    }
    output[j] = m;
    j++;
  }

  *total = j;
  return output;
}
