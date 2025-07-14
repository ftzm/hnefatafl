#include "board.h"
#include "capture.h"
#include "constants.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "x86intrin.h"
#include <stdio.h>

#define THRONE_MASK_0 1152921504606846976ULL

bool king_capture_check_ref(const board *b) {
  int king_index = LOWEST_INDEX(b->king);
  layer attackers = b->black;
  attackers._[0] |= THRONE_MASK_0;
  return NOT_EMPTY(LAYER_AND(b->king, INTERIOR)) &&
         CHECK_INDEX(attackers, king_index - 1) &&
         CHECK_INDEX(attackers, king_index + 1) &&
         CHECK_INDEX(attackers, king_index - 11) &&
         CHECK_INDEX(attackers, king_index + 11);
}

bool king_capture_check(const board *b) {
  // Bail out early if the king is at an edge, since the king can't be
  // fully surrounded at an edge. This also means that the remaining
  // computations can assume a valid capture position.
  if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
    return false;
  }
  // We get the index of the king minus 12 to use as the shift amount
  // for SURROUND MASK. SURROUND_MASK surrounds index 12 (the lowest
  // index at which 4 pieces can surround another), so we need an
  // offset king index for the shift.
  const int mask_shift = b->king._[0] ? _tzcnt_u64(b->king._[0]) - 12
                                      : _tzcnt_u64(b->king._[1]) + 52;

  // Shift the SURROUND_MASK constant into position such that set bits
  // surround the king.
  const layer surround_mask = layer_shift(SURROUND_MASK, mask_shift);


  // The throne also participates in king captures.
  const layer attackers = {b->black._[0] | THRONE_MASK_0, b->black._[1]};


  // Get a layer of the occupied positions.
  const layer present = LAYER_AND(attackers, surround_mask);

  /*
  print_layer(SURROUND_MASK);
  printf("mask_shift: %d\n", mask_shift);
  print_layer(surround_mask);
  print_layer(present);
  */

  return LAYERS_EQUAL(surround_mask, present);
}

// Also considers adjacents to be escapes to simplify code.
bool king_escaped(const board *b) {
  return b->king._[0] & corners._[0] || b->king._[1] & corners._[1];
}

bool king_effectively_escaped(const board *b) {
  return b->king._[0] & CORNERS_AND_ADJACENTS_0 || b->king._[1] & CORNERS_AND_ADJACENTS_1;
}

bool king_captured(const board *b) {
  if (IS_EMPTY(LAYER_AND(b->king, INTERIOR))) {
    return false;
  }

  int king_pos = LOWEST_INDEX(b->king);
  layer surround_mask = surround_masks[king_pos];
  layer attackers = b->black;
  attackers._[0] |= THRONE_MASK_0;
  layer present = LAYER_AND(attackers, surround_mask);

  return LAYERS_EQUAL(surround_mask, present);
}
