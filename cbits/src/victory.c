#include "board.h"
#include "capture.h"
#include "constants.h"
#include "layer.h"
#include "stdbool.h"
#include "x86intrin.h" // IWYU pragma: export

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
  return b->king._[0] & CORNERS_AND_ADJACENTS_0 ||
         b->king._[1] & CORNERS_AND_ADJACENTS_1;
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

bool surrounded(const board *b) {
  layer white = LAYER_OR(b->white, b->king);
  layer open = LAYER_NOT(board_occ(*b));
  layer prev = EMPTY_LAYER;

  do {
    prev = white;

    // Check if any white pieces are at the edge, in which case they can't be
    // surrounded.
    if ((white._[0] & EDGE_POSITIONS_0) | (white._[1] & EDGE_POSITIONS_1)) {
      return false;
    }

    layer temp = white;
    LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(temp, 1)));
    LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTL_SHORT(temp, 11)));
    LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTR(temp, 1)));
    LAYER_OR_ASSG(white, LAYER_AND(open, LAYER_SHIFTR(temp, 11)));

    // If the white layer remains unchanged after expansion then available space
    // must be filled, in which case white is surrounded.
  } while (!LAYERS_EQUAL(white, prev));

  return true;
}

#define U64_CONTAINS(_haystack, _needle) ((_haystack & _needle) == _needle)

bool exit_fort(const board *b) {
  if (NOT_EMPTY(LAYER_AND(b->king, EXIT_FORT_ELIGIBLE))) {
    int king_pos = LOWEST_INDEX(b->king);
    int king_rank = RANK(king_pos);
    int king_file = FILE(king_pos);
    if (king_rank == 0) {
      int adjust = king_file - 2;
      u64 white_adjusted = b->white._[0] >> adjust;
      u64 black_adjusted = b->black._[0] >> adjust;
      // if any of the adjacent squares to the king contain black pieces then
      // either there isn't an exit fort or the exit fort contains a black piece
      // and thus is invalid.
      if (BLACK_FREE_0 & black_adjusted) {
        return false;
      }
      if (U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_A_0) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_B_0) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_A_0) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_B_0)) {
        return true;
      }
    } else if (king_file == 0) {
      int adjust = 8 - king_rank;
      u64 white_adjusted = b->white_r._[0] >> adjust;
      u64 black_adjusted = b->black_r._[0] >> adjust;
      if (BLACK_FREE_0 & black_adjusted) {
        return false;
      }
      if (U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_A_0) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_B_0) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_A_0) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_B_0)) {
        return true;
      }
    } else if (king_rank == 10) {
      int adjust = king_file - 2;
      u64 white_adjusted = b->white._[1] >> adjust;
      u64 black_adjusted = b->black._[1] >> adjust;
      if (BLACK_FREE_UPPER_1 & black_adjusted) {
        return false;
      }
      if (U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_A_UPPER_1) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_B_UPPER_1) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_A_UPPER_1) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_B_UPPER_1)) {
        return true;
      }
    } else if (king_file == 10) {
      int adjust = 8 - king_rank;
      u64 white_adjusted = b->white_r._[1] >> adjust;
      u64 black_adjusted = b->black_r._[1] >> adjust;
      if (BLACK_FREE_UPPER_1 & black_adjusted) {
        return false;
      }
      if (U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_A_UPPER_1) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_SHORT_B_UPPER_1) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_A_UPPER_1) ||
          U64_CONTAINS(white_adjusted, EXIT_FORT_TALL_B_UPPER_1)) {
        return true;
      }
    }
  }
  return false;
}

bool black_victory(const board *b) {
  return king_capture_check(b) || surrounded(b);
}

bool white_victory(const board *b) {
  return king_effectively_escaped(b) || exit_fort(b);
}
