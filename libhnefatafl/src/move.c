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
 * TODO: We end up recalculating the origin for each destination,
 *     which seems like it should be inefficient. one idea would be to
 *     iterate over each mover, isolate its destinations, and then
 *     iterate over those. I think I've tried this with little speed
 *     difference, but it would be nice to try again in macro
 *     benchmark
 */
#define LEFTWARD(_i, _r)                                                       \
  {                                                                            \
    u64 dests = targets##_r._[_i] &                                            \
                (leftward_occ##_r._[_i] - (movers##_r._[_i] << 1)) &           \
                DROP_1_EAST_##_i;                                              \
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
    u16 dests = GET_CENTER_ROW(targets##_r) &                                  \
                (center_occ##_r - (center_movers##_r << 1));                   \
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

#define BOOKKEEP(_i)                                                           \
  ms[(*total)] = (move){orig, dest};                                           \
  ls[(*total)]._[_i] |= orig_bit;                                              \
  ls[(*total)]._[_i] |= dest_bit;                                              \
  OP_LAYER_BIT(ls_r[(*total)], orig_r, |=);                                    \
  OP_LAYER_BIT(ls_r[(*total)], dest_r, |=);                                    \
  (*total)++;

#define BOOKKEEP_R(_i)                                                         \
  ms[(*total)] = (move){orig_r, dest_r};                                       \
  OP_LAYER_BIT(ls_r[(*total)], orig, |=);                                      \
  OP_LAYER_BIT(ls_r[(*total)], dest, |=);                                      \
  OP_LAYER_BIT(ls[(*total)], orig_r, |=);                                      \
  OP_LAYER_BIT(ls[(*total)], dest_r, |=);                                      \
  (*total)++;
#define BOOKKEEP_r BOOKKEEP_R
#define BOOKKEEP_CENTER_r BOOKKEEP_R

#define BOOKKEEP_CENTER()                                                      \
  ms[(*total)] = (move){orig, dest};                                           \
  OP_LAYER_BIT(ls[(*total)], orig, |=);                                        \
  OP_LAYER_BIT(ls[(*total)], dest, |=);                                        \
  OP_LAYER_BIT(ls_r[(*total)], orig_r, |=);                                    \
  OP_LAYER_BIT(ls_r[(*total)], dest_r, |=);                                    \
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

static inline layer leftward_moves_layer(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  u64 carryover;
  // lower
  {
    u64 blockers = occ._[0] | file_mask_0._[0];
    // we & ~blockers to remove all blockers that haven't been bit flipped by
    // a substraction, so that all we're left with are subtraction rays
    u64 dests = (blockers - ((movers._[0]) << 1)) & ~(blockers | throne._[0]);
    carryover = ((dests | (movers._[0] & (((u64)1) << 63))) >> 63) & ~occ._[1];
    output._[0] = dests;
  }

  // upper
  {
    u64 blockers = occ._[1] | file_mask_0._[1];
    u64 dests = (blockers - ((((movers._[1]) << 1)) | carryover)) & ~blockers;
    output._[1] = dests;
  }

  return output;
}

static inline layer rightward_moves_layer(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  u64 carryover;
  // upper
  {
    u64 blockers = (occ._[1] | file_mask_10._[1]);
    u64 movers_ext = _pext_u64(movers._[1], blockers);
    u64 movers_dep = _pdep_u64(movers_ext, 1 | (blockers << 1));
    u64 move_mask = (movers._[1] - movers_dep);
    carryover = (((move_mask | movers._[1]) & 1) << 63) & ~occ._[0];
    output._[1] = move_mask;
  }

  {
    u64 blockers = occ._[0] | file_mask_10._[0];
    // here I depend on the lower right corner being occupied to ensure that I
    // generate a ray towards it
    u64 movers_ext =
        _pext_u64(carryover | movers._[0], carryover | blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, (blockers << 1));
    u64 move_mask = (movers._[0] - movers_dep) & (~throne._[0]);
    output._[0] = move_mask;
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

typedef struct {
  u64 lower;
  u64 upper;
  u16 center;
} move_bits;

/* The idea for this will be to generate the whole struct up front and provide
 * it to search functions. These functions can then mask the portions they need,
 * such as king captures or other tactical moves, and then extract moves from
 * this sub-selection. We'll also calculate the remainder, so that follow-up
 * functions don't revisit explored moves. This paradigm also supports a
 * generator-style approach to extraction that allows stopping before all moves
 * have been extracted, albiet requiring more storage than the existing
 * implementation. */
typedef struct {
  layer leftward;
  layer rightward;
  layer leftward_r;
  layer rightward_r;
} move_layers;

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

// -----------------------------------------------------------------------------

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
