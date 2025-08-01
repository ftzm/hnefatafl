#include "move.h"
#include "board.h"
#include "capture.h"
#include "io.h"
#include "layer.h"
#include "limits.h"
#include "macro_util.h"
#include "stdbool.h"
#include "stdio.h"
#include "string.h"
#include "x86intrin.h" // IWYU pragma: export

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
// Move map methods

const layer file_mask_0 = {36046397799139329ULL, 70403120701444ULL};
const layer file_mask_1 = {72092795598278658ULL, 140806241402888ULL};
const layer file_mask_9 = {9011599449784832ULL, 36046397799139329ULL};
const layer file_mask_10 = {18023198899569664ULL, 72092795598278658ULL};

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

/*
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
#define EXTRACT_LEFTWARD(_i, _r)                                               \
  u64 dest_bit = _blsi_u64(dests);                                             \
  u8 dest = _tzcnt_u64(dest_bit);                                              \
  OFFSET(dest, _i);                                                            \
  u8 orig = 63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & leftward_occ##_r._[_i]);   \
  u64 orig_bit = (u64)1 << orig;                                               \
  OFFSET(orig, _i);

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
#define LEFTWARD(_i, _r)                                                       \
  {                                                                            \
    u64 dests = targets##_r._[_i] &                                            \
                (leftward_occ##_r._[_i] - (movers##_r._[_i] << 1)) &           \
                DROP_1_EAST_##_i;                                              \
    while (dests) {                                                            \
      EXTRACT_LEFTWARD(_i, _r);                                                \
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
      EXTRACT_CENTER_LEFTWARD(center_occ##_r);                                 \
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
      ~((u64)-1 >> _lzcnt_u64(rightward_occ##_r._[_i] & below));               \
  u64 dests = targets##_r._[_i] & below & above_highest_occ_mask;

#define RIGHTWARD_DESTS_CENTER(_r)                                             \
  u16 below = orig_bit - 1;                                                    \
  u16 above_highest_occ_mask =                                                 \
      (center_occ##_r & below)                                                 \
          ? ((u16)-1 << (16 - __lzcnt16(center_occ##_r & below)))              \
          : (u16)-1;                                                           \
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
    u64 movers_ext = _pext_u64(movers##_r._[_i], 1 | blockers) >> 1;           \
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
  layer rightward_occ = LAYER_OR(occ, file_mask_10);
  layer rightward_occ_r = LAYER_OR(occ_r, file_mask_10);
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
  layer rightward_occ = LAYER_OR(occ, file_mask_10);
  layer rightward_occ_r = LAYER_OR(occ_r, file_mask_10);
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
new plan for rightward moves:

I can still do this king of thing:
    u64 lefts = movers_r._[1] & ~(occ_r._[1] - targets_r._[1]);
to isolate movers that can actually reach targets. But I do need another
approach to extract moves.

The current approach is just broken because I zero the mover bit after
extracting just the closest target, which obviously isn't correct. Instead I
need a way to re-use the mover bit.

- I can't just start iterating up targets, because I don't have a way to isolate
only targets I can arrive at, I have to iterate over the movers and find a way
to locate targets relative to that. One idea:
1. find lowest mover bit
2. mask below that
3. apply mask to occ
4. find highest occ bit
5. mask above that
6. combine that with the mask below the mover to isolate the moving bits.

this feels like kind of a lot though. Given 2 or three targets on a u64
kogge-stone might actually be faster.

with kogge stone we can can limit generation to movers that can reach targets.
This means we won't need to exclude any target bits during extraction,
simplifying things. That leaves two options for extraction:

1. extract destinations bottom up, creating a mask below each bit, inverting
that, applying that mask to movers, and doing a tzcnt.

2. extract movers bottom up, creating a mask below each. for each mask, and it
with destinations, and extract those bottom up, reusing the value for the

One possibility would be to do a popcnt of the movers in play and select an
implementation based on that :man thinking:


*/

/*
kogge stone

gen is movers, pro is unoccupied squares
*/
#define RIGHTWARD_MOVES(_block)                                                \
  pro &= _block;                                                               \
  gen |= pro & (gen >> 1);                                                     \
  pro &= (pro >> 1);                                                           \
  gen |= pro & (gen >> 2);                                                     \
  pro &= (pro >> 2);                                                           \
  gen |= pro & (gen >> 4);                                                     \
  pro &= (pro >> 4);                                                           \
  gen |= pro & (gen >> 8);

layer rightward_moves_layer2(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  // lower
  {
    u64 gen = movers._[0] & LOWER_HALF_MASK;
    u64 pro = ~occ._[0] & LOWER_HALF_MASK;
    RIGHTWARD_MOVES(18428720874809981951ULL);
    output._[0] = (gen ^ movers._[0]) & LOWER_HALF_MASK;
  }

  // upper
  {
    u64 gen = movers._[1] & UPPER_HALF_MASK;
    u64 pro = ~occ._[1] & UPPER_HALF_MASK;
    RIGHTWARD_MOVES(72022392477577213ULL);
    output._[1] = (gen ^ movers._[1]) & UPPER_HALF_MASK;
  }

  // center
  {
    u16 center_movers = GET_CENTER_ROW(movers);
    u16 gen = center_movers;
    u16 pro = ~GET_CENTER_ROW(occ);
    RIGHTWARD_MOVES(~0);
    SET_CENTER_ROW(output, (gen ^ center_movers));
  }

  return output;
}

layer rightward_moves_layer(layer movers, layer occ) {
  layer output = EMPTY_LAYER;

  // lower
  {

    u64 blockers = occ._[0] | file_mask_10._[0];
    u64 movers_ext = _pext_u64(movers._[0], 1 | blockers) >> 1;
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

inline int rightward_moves_count(layer movers, layer occ) {
  int output = 0;

  {
    u64 blockers = occ._[0] | file_mask_10._[0];
    // print_layer((layer){blockers, 0});
    // here I depend on the lower right corner being occupied to ensure that I
    // generate a ray towards it
    u64 movers_ext = _pext_u64(movers._[0], blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, (blockers << 1));
    // print_layer((layer){movers_dep, 0});
    // I use lower half mask here to prevent generating moves in the center
    // row
    u64 move_mask = (movers._[0] - movers_dep) & LOWER_HALF_MASK;
    // print_layer((layer){move_mask, 0});
    output += __builtin_popcountll(move_mask);
    // printf("output: %d\n", output);
  }

  // upper
  {
    u64 blockers = (occ._[1] | file_mask_10._[1]);
    u64 movers_ext = _pext_u64(movers._[1], blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, blockers << 1);
    // I use upper half mask here to prevent generating moves in the center
    // row
    u64 move_mask = (movers._[1] - movers_dep) & UPPER_HALF_MASK;
    // print_layer((layer){0, move_mask});
    output += __builtin_popcountll(move_mask);
    // printf("output: %d\n", output);
  }

  // center
  {
    u16 blockers = GET_CENTER_ROW(occ);
    // print_row(blockers);

    // I can just remove the lowest bit because it by definition can't move
    // anywhere
    u16 movers_row = GET_CENTER_ROW(movers) & 0b11111111110;
    // print_row(movers_row);

    u16 tail = (blockers & 1) ^ 1;
    // print_row(tail);

    u16 movers_ext = _pext_u32(movers_row, tail | blockers) >> 1;
    u16 movers_dep = _pdep_u32(movers_ext, tail | (blockers << 1));
    // print_row(movers_dep);
    u16 move_mask = (movers_row - movers_dep) & INVERTED_THRONE_MASK;
    // print_row(move_mask);
    output += __builtin_popcount(move_mask);
    // printf("output: %d\n", output);
  }

  return output;
}

inline int rightward_moves_count_king(layer movers, layer occ) {
  int output = 0;

  if (movers._[0] & LOWER_HALF_MASK) {
    u64 blockers = occ._[0] | file_mask_10._[0];
    // print_layer((layer){blockers, 0});
    // here I depend on the lower right corner being occupied to ensure that I
    // generate a ray towards it
    u64 movers_ext = _pext_u64(movers._[0], 1 | blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, 1 | (blockers << 1));
    // print_layer((layer){movers_dep, 0});
    // I use lower half mask here to prevent generating moves in the center
    // row
    u64 move_mask = (movers._[0] - movers_dep) & LOWER_HALF_MASK;
    // print_layer((layer){move_mask, 0});
    return __builtin_popcountll(move_mask);
    // printf("output: %d\n", output);
  }

  // upper
  if (movers._[1] & UPPER_HALF_MASK) {
    u64 blockers = (occ._[1] | file_mask_10._[1]);
    u64 movers_ext = _pext_u64(movers._[1], blockers) >> 1;
    u64 movers_dep = _pdep_u64(movers_ext, blockers << 1);
    // I use upper half mask here to prevent generating moves in the center
    // row
    u64 move_mask = (movers._[1] - movers_dep) & UPPER_HALF_MASK;
    // print_layer((layer){0, move_mask});
    return __builtin_popcountll(move_mask);
    // printf("output: %d\n", output);
  }

  // center
  if (GET_CENTER_ROW(movers)) {
    u16 blockers = GET_CENTER_ROW(occ);
    // print_row(blockers);

    // I can just remove the lowest bit because it by definition can't move
    // anywhere
    u16 movers_row = GET_CENTER_ROW(movers) & 0b11111111110;
    // print_row(movers_row);

    u16 tail = (blockers & 1) ^ 1;
    // print_row(tail);

    u16 movers_ext = _pext_u32(movers_row, tail | blockers) >> 1;
    u16 movers_dep = _pdep_u32(movers_ext, tail | (blockers << 1));
    // print_row(movers_dep);
    u16 move_mask = (movers_row - movers_dep) & 0b11111111111;
    // print_row(move_mask);
    return __builtin_popcount(move_mask);
    // printf("output: %d\n", output);
  }

  return output;
}

inline int leftward_moves_count(layer movers, layer occ) {
  int output = 0;

  // lower
  {
    u64 blockers = occ._[0] | file_mask_0._[0];
    // we & ~blockers to remove all blockers that haven't been bit flipped by
    // a substraction, so that all we're left with are subtraction rays we &
    // LOWER_HALF_MASK to remove anything in the center row
    //
    u64 dests = (blockers - ((movers._[0]) << 1)) & ~blockers & LOWER_HALF_MASK;
    // print_layer((layer){dests, 0});
    output += __builtin_popcountll(dests);
    // printf("output: %d\n", output);
  }

  // upper
  {
    u64 blockers = occ._[1] | file_mask_0._[1];
    u64 dests = (blockers - ((movers._[1]) << 1)) & ~blockers & UPPER_HALF_MASK;
    // print_layer((layer){0, dests});
    output += __builtin_popcountll(dests);
    // printf("output: %d\n", output);
  }

  // center
  {
    u16 blockers = GET_CENTER_ROW(occ);
    u64 dests = blockers - (GET_CENTER_ROW(movers) << 1) & ~blockers &
                INVERTED_THRONE_MASK;
    // print_row((u16)dests);
    output += __builtin_popcount(dests);
    // printf("output: %d\n", output);
  }

  return output;
}

inline int leftward_moves_count_king(layer movers, layer occ) {
  int output = 0;

  // lower
  if (movers._[0] & LOWER_HALF_MASK) {
    u64 blockers = occ._[0] | file_mask_0._[0];
    // we & ~blockers to remove all blockers that haven't been bit flipped by
    // a substraction, so that all we're left with are subtraction rays we &
    // LOWER_HALF_MASK to remove anything in the center row
    u64 dests = (blockers - ((movers._[0]) << 1)) & ~blockers & LOWER_HALF_MASK;
    // print_layer((layer){dests, 0});
    return __builtin_popcountll(dests);
    // printf("output: %d\n", output);
  }

  if (movers._[1] & UPPER_HALF_MASK) {
    u64 blockers = occ._[1] | file_mask_0._[1];
    u64 dests = (blockers - ((movers._[1]) << 1)) & ~blockers & UPPER_HALF_MASK;
    // print_layer((layer){0, dests});
    return __builtin_popcountll(dests);
    // printf("output: %d\n", output);
  }

  // center
  if (GET_CENTER_ROW(movers)) {
    u16 blockers = GET_CENTER_ROW(occ);
    u64 dests =
        blockers - (GET_CENTER_ROW(movers) << 1) & ~blockers & 0b11111111111;
    // print_row((u16)dests);
    return __builtin_popcount(dests);
    // printf("output: %d\n", output);
  }

  return output;
}

int black_moves_count(const board *b) {
  int total = 0;
  layer occ = board_occ(*b);
  layer occ_r = board_occ_r(*b);
  total += leftward_moves_count(b->black, occ);
  total += leftward_moves_count(b->black_r, occ_r);
  total += rightward_moves_count(b->black, occ);
  total += rightward_moves_count(b->black_r, occ_r);
  return total;
}

int white_moves_count(const board *b) {
  int total = 0;
  layer occ = board_occ(*b);
  layer occ_r = board_occ_r(*b);
  // printf("total: %d\n", total);
  total += leftward_moves_count(b->white, occ);
  // printf("total: %d\n", total);
  total += leftward_moves_count(b->white_r, occ_r);
  // printf("total: %d\n", total);
  total += rightward_moves_count(b->white, occ);
  // printf("total: %d\n", total);
  total += rightward_moves_count(b->white_r, occ_r);
  // printf("total: %d\n", total);
  return total;
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

/*
  Idea for a move generator so we don't end up allocating so much memory for
move lists:

The general idea is to create new versions of the moves_to and the macros it
uses to return one move at a time. This means that instead of using a while loop
and looping over the dests as they're created, I split it up into stages, where
I first generate dests, and then separetaly I check if the u64 of dests is empty
and if not extract and return the next move.

In order to return one move at a time and pick up where I left off each time
I'll need a struct to hold onto state. It will contain a state int which tells
where we are in the progress. Another struct member with a u64 of positions to
extract. the movers, targets, occ, etc.

I'll then need a function that takes a reference to this state struct and
processes moves. It will consist mainly of a large switch statement. The switch
branches will have alternating purposes: the first will generate dests for one
direction for one layer portion (equivalent to say LEFTWARD(0, ) in moves_to,
say). so it will produce a single u64 of dests. It will also need to set a
pointer to the movers so that we have the information we need to extract moves.
There will be no breaks in any of the branches; they will all fall through to
the next. The next type of branch, which will come after each dest generating
branch, is the one in which we will extract a move. the logic is largely the
same as the existing, except when a move is found we return rather than adding
it to a list of moves. To keep the initial implementation simple we'll only
return a move as a move struct, not the layers. Before returning we'll also need
to set the state int to the number of the current switch state that we're in, so
that when the function is called again we continue to pull moves from that u64.

*/
