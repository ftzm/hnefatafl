#include <stdint.h>
#include <string>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <x86intrin.h>

#include <array>

/*******************************************************************************
 * Layer
 *
 * A layer is a representation of a set of board positions. Only
 * presence or absence is indicated. The two 64-bit integers making up
 * a layer are termed "sub layers", with the one at index 0 being
 * "lower" and the one at index 1 being "upper".
 ******************************************************************************/

// typedef uint64_t layer[2];

typedef std::array<uint64_t, 2> layer;

constexpr unsigned char sub_layer[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
};

constexpr unsigned int sub_layer_offset[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
};

const unsigned int sub_layer_offset_direct[121] = {
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
  22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
  33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
  44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
  55, 56, 57, 58, 59, 60, 61, 62, 63, 0,  1,
  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12,
  13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
  24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
  35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
};

constexpr unsigned char rotate_right[121] = {
  10, 21, 32, 43, 54, 65, 76, 87, 98, 109, 120,
  9,  20, 31, 42, 53, 64, 75, 86, 97, 108, 119,
  8,  19, 30, 41, 52, 63, 74, 85, 96, 107, 118,
  7,  18, 29, 40, 51, 62, 73, 84, 95, 106, 117,
  6,  17, 28, 39, 50, 61, 72, 83, 94, 105, 116,
  5,  16, 27, 38, 49, 60, 71, 82, 93, 104, 115,
  4,  15, 26, 37, 48, 59, 70, 81, 92, 103, 114,
  3,  14, 25, 36, 47, 58, 69, 80, 91, 102, 113,
  2,  13, 24, 35, 46, 57, 68, 79, 90, 101, 112,
  1,  12, 23, 34, 45, 56, 67, 78, 89, 100, 111,
  0,  11, 22, 33, 44, 55, 66, 77, 88, 99,  110
};

const unsigned char rotate_left[121] = {
  110, 99,  88, 77, 66, 55, 44, 33, 22, 11, 0,
  111, 100, 89, 78, 67, 56, 45, 34, 23, 12, 1,
  112, 101, 90, 79, 68, 57, 46, 35, 24, 13, 2,
  113, 102, 91, 80, 69, 58, 47, 36, 25, 14, 3,
  114, 103, 92, 81, 70, 59, 48, 37, 26, 15, 4,
  115, 104, 93, 82, 71, 60, 49, 38, 27, 16, 5,
  116, 105, 94, 83, 72, 61, 50, 39, 28, 17, 6,
  117, 106, 95, 84, 73, 62, 51, 40, 29, 18, 7,
  118, 107, 96, 85, 74, 63, 52, 41, 30, 19, 8,
  119, 108, 97, 86, 75, 64, 53, 42, 31, 20, 9,
  120, 109, 98, 87, 76, 65, 54, 43, 32, 21, 10
};

const uint64_t inverted_throne_mask = 0b11111011111;

constexpr layer rotate_layer(const layer input) {
  layer output = {0};
  for (int i = 0; i < 121; i++) {
    if (input[sub_layer[i]] & ((uint64_t)1 << (i - sub_layer_offset[i]))) {
      int r = rotate_right[i];
      output[sub_layer[r]] |= ((uint64_t)1 << (r - sub_layer_offset[r]));
    }
  }
  return output;
}

constexpr layer read_layer(const char *string, unsigned char symbol) {
  layer output = {0};
  int len = std::char_traits<char>::length(string);
  int index = 0;
  for (int i = 0; i < len; i++) {
    char c = string[i];
    if (c == symbol) {
      output[sub_layer[index]] |= ((uint64_t) 1 << (index - sub_layer_offset[index]));
      index++;
    } else if (c == ' ') {
      // skip space
    } else {
      index++; // skip other chars but increment
    }
  }
  return output;
}

void print_layer(layer layer) {
  char string[374];

  // initialize empty string
  memset(string, ' ', 373);
  string[373] = '\0';


  // insert newlines
  for (int i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate unsigned char
  for (int i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = ((i * 3) + 1) + newline_offset;
    if (layer[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}

/*******************************************************************************
 * Board
 *
 ******************************************************************************/

typedef struct board {
  layer black;
  layer black_r;
  layer white;
  layer white_r;
  // king can maybe also just be a char
  layer king;
  layer king_r;
} board;

constexpr board read_board(const char *string) {
  layer black = read_layer(string, 'X');
  layer white = read_layer(string, 'O');
  layer king = read_layer(string, '#');
  layer black_r = rotate_layer(black);
  layer white_r = rotate_layer(white);
  layer king_r = rotate_layer(king);
  board board = {black, black_r, white, white_r, king, king_r};
  return board;
}

void print_board(board board) {
  char string[374];

  // initialize empty string
  memset(string, ' ', 373);
  string[373] = '\0';


  // insert newlines
  for (int i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate unsigned char
  for (int i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = ((i * 3) + 1) + newline_offset;
    if (board.black[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else if (board.white[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'O';
    } else if (board.king[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = '#';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}

void print_board_r(board board) {
  char string[374];

  // initialize empty string
  memset(string, ' ', 373);
  string[373] = '\0';


  // insert newlines
  for (int i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate unsigned char
  for (int i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = ((i * 3) + 1) + newline_offset;
    if (board.black_r[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else if (board.white_r[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'O';
    } else if (board.king_r[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = '#';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}

layer corners = {1025, 72127962782105600};

/*******************************************************************************
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

layer foe_masks[120];
layer foe_masks_r[120];

void gen_foe_masks() {
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

layer ally_masks[120];
layer ally_masks_r[120];

void gen_ally_masks() {
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

void capture_x(const layer &allies, const layer &allies_r, layer &foes,
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

void capture_y(const layer &allies, const layer &allies_r, layer &foes,
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

void capture_b(const layer &allies, const layer &allies_r, layer &foes,
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

void capture_B(const layer &allies, const layer &allies_r, layer &foes,
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

void capture_62(const layer &allies, const layer &allies_r, layer &foes,
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

void (*capture_functions[121])(const layer &, const layer &, layer &, layer &,
                               const unsigned char) = {
    // 0
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

inline void apply_captures_niave(const layer friends, layer foes, layer output, int dest) {
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
      //printf("north");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }

  //southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 23 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("south");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }

  //westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 8 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("west\n");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }
   
  //eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 2 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("east\n");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }
}

inline void dispatch_capture(const layer allies, const layer allies_r, layer foes, layer foes_r, const unsigned char pos) {
  switch (pos) {
  case 0:
    capture_se(allies, allies_r, foes, foes_r, pos);
    break;
  case 1:
    capture_se(allies, allies_r, foes, foes_r, pos);
    break;
  case 2:
    capture_se(allies, allies_r, foes, foes_r, pos);
    break;
  case 3:
    capture_s(allies, allies_r, foes, foes_r, pos);
    break;
  case 4:
    capture_s(allies, allies_r, foes, foes_r, pos);
    break;
  case 5:
    capture_s(allies, allies_r, foes, foes_r, pos);
    break;
  case 6:
    capture_s(allies, allies_r, foes, foes_r, pos);
    break;
  case 7:
    capture_s(allies, allies_r, foes, foes_r, pos);
    break;
  case 8:
    capture_sw(allies, allies_r, foes, foes_r, pos);
    break;
  case 9:
    capture_sw(allies, allies_r, foes, foes_r, pos);
    break;
  case 10:
    capture_sw(allies, allies_r, foes, foes_r, pos);
    break;
  case 110:
    capture_ne(allies, allies_r, foes, foes_r, pos);
    break;
  case 111:
    capture_ne(allies, allies_r, foes, foes_r, pos);
    break;
  case 112:
    capture_ne(allies, allies_r, foes, foes_r, pos);
    break;
  case 113:
    capture_n(allies, allies_r, foes, foes_r, pos);
    break;
  case 114:
    capture_n(allies, allies_r, foes, foes_r, pos);
    break;
  case 115:
    capture_n(allies, allies_r, foes, foes_r, pos);
    break;
  case 116:
    capture_n(allies, allies_r, foes, foes_r, pos);
    break;
  case 117:
    capture_n(allies, allies_r, foes, foes_r, pos);
    break;
  case 118:
    capture_nw(allies, allies_r, foes, foes_r, pos);
    break;
  case 119:
    capture_nw(allies, allies_r, foes, foes_r, pos);
    break;
  case 120:
    capture_nw(allies, allies_r, foes, foes_r, pos);
    break;
  default:
    apply_captures_niave(allies, foes, foes, pos);
    break;
  }
  /*
    // 0
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
  */
};

/*******************************************************************************
 * Moves
 *
 ******************************************************************************/
constexpr uint8_t clz(uint16_t input) {
  uint8_t sum = 0;
  for (int i = 15; i >= 0; i--) {
    if (input & ((uint16_t)1 << i)) {
      break;
    }
    sum++;
  }
  return sum;
}

constexpr uint8_t leading_zero(uint16_t x)
{
    uint8_t n = 0;
    if (x == 0 ) {return 16;}
    if (x <= 0x00ff) n +=  8, x <<= 8;
    if (x <= 0x0fff) n +=  4, x <<= 4;
    if (x <= 0x3fff) n +=  2, x <<= 2;
    if (x <= 0x7fff) n ++;
    return n;
}


unsigned short get_row_moves(const unsigned short occ, const unsigned char pos) {
  static const unsigned short lowers[12] = {
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
    0b01111111111,
    // The below is never used as a mask, only by `rightward` when
    // `upper` is empty
    0b11111111111
  };
  /*
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
    0b00000000000
  };
  */
  unsigned short lower = occ & lowers[pos];
  unsigned short upper = occ & (0b11111111110 << pos);
  unsigned short rightward = lowers[__tzcnt_u16(upper | 0x800)];
  // unsigned short rightward = lowers[__builtin_ctz(upper | 0x800)];
  unsigned short blocked = 0xFFFF >> __lzcnt16(lower);
  // unsigned short blocked = 0xFFFF >> leading_zero(lower);
  return (rightward - blocked) ^ (1 << pos);
}

unsigned short get_row_moves_2(const unsigned short occ, const unsigned char pos) {
  static const unsigned short lowers[12] = {
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
    0b01111111111,
    // The below is never used as a mask, only by `rightward` when
    // `upper` is empty
    0b11111111111
  };
  /*
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
    0b00000000000
  };
  */
  uint16_t lower_barrier = 0b10000000000000000 >> __lzcnt16(lowers[pos] & occ);
  // return lower_barrier;
  uint16_t capped_occ = 0b100000000000 | occ;
  return capped_occ ^ (capped_occ | (capped_occ - lower_barrier - (1 << pos)));
}


typedef struct move {
  unsigned char orig;
  unsigned char dest;
} move;

typedef struct moves {
  int num;
  move *moves;
} moves;

void print_row(unsigned short row) {
  char output[17];
  memset(output, '0', 16);
  output[16] = '\0';
  int index;
  while (row) {
    index = _tzcnt_u16(row);
    output[15 - index] = '1';
    row &= ~(1 << index);
  }
  puts(output);
  printf("\n");
}

const unsigned char row_indexes[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
  22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
  33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
  44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,
  55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
  66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
  77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
  88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
  99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
  110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110
}; 

uint16_t row_moves_table[2048][11];
uint16_t center_row_moves_table[2048][11];

void gen_row_moves() {
  uint16_t row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      row_moves_table[row][pos] = get_row_moves(row, pos);
    }
  }
}

void gen_center_row_moves() {
  uint16_t row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      center_row_moves_table[row][pos] = get_row_moves(row, pos) & inverted_throne_mask;
    }
  }
}

/*
// -------------
// Testing

constexpr std::array<std::array<uint8_t, 11>, 2048> gen_row_move_counts_c() {
  std::array<std::array<uint8_t, 11>, 2048> res{};
  for (uint16_t row = 0; row < 2048; row++) {
    for (uint8_t pos = 0; pos < 11; pos++) {
      res[row][pos] = __builtin_popcount(get_row_moves(row, pos));
    }
  }
  return res;
}

constexpr std::array<std::array<uint8_t, 11>, 2048> test_row_move_count_table = gen_row_move_counts_c();

// -------------
*/

uint8_t row_move_count_table[2048][11];

uint8_t center_row_move_count_table[2048][11];

void gen_row_move_counts() {
  uint16_t row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      row_move_count_table[row][pos] = __builtin_popcount(get_row_moves(row, pos));
    }
  }
}

void gen_center_row_move_counts() {
  uint16_t row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      center_row_move_count_table[row][pos] =
	__builtin_popcount(get_row_moves(row, pos)
			   & inverted_throne_mask);
    }
  }
}
 
template <int OFFSET>
void get_row_total_moves(const uint64_t *team, const uint64_t *occ, uint8_t *total) {
  uint16_t movers = ((uint64_t) *team >> OFFSET) & 0b11111111111;
  const uint16_t blockers = ((uint64_t) *occ >> OFFSET) & 0b11111111111;
  while (movers) {
    (*total) += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }
}

// ----------------------------------------------------------------------
// Integrated

//inline void get_next_row_boards_black(const uint64_t team, const uint64_t occ,
template <int index, unsigned char sub_index, int row_offset>
inline __attribute__((always_inline)) void
get_next_row_boards_black(const uint64_t occ, const board &base_board,
                          int *total, move *moves, board *boards) {
  unsigned short movers =
      (base_board.black[index] >> row_offset) & 0b11111111111;
  uint64_t row_moves;
  unsigned char row_orig, sub_orig, orig, orig_r, sub_dest, dest, dest_r;
  while (movers) {
    row_orig = _tzcnt_u16(movers);
    sub_orig = row_offset + row_orig;
    orig = sub_orig + sub_index;
    orig_r = rotate_right[orig];
    const unsigned short blockers = ((uint64_t)occ >> row_offset) & 0b11111111111;
    row_moves = (uint64_t)row_moves_table[blockers][row_orig] << row_offset;
    while (row_moves) {
      sub_dest = _tzcnt_u64(row_moves);
      dest = sub_dest + sub_index;
      dest_r = rotate_right[dest];

      // register move
      moves[*total] = (struct move){orig, dest};

      // Generate board
      board new_board = base_board;
      new_board.black[index] -= (uint64_t)1 << sub_orig;
      new_board.black[index] |= (uint64_t)1 << sub_dest;
      new_board.black_r[sub_layer[orig_r]] -=
	  // (uint64_t)1 << (orig_r - sub_layer_offset[orig_r]);
          (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
      new_board.black_r[sub_layer[dest_r]] |=
	  // (uint64_t)1 << (dest_r - sub_layer_offset[dest_r]);
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

      // handle captures
      capture_functions[dest](new_board.black, new_board.black_r,
                              new_board.white, new_board.white_r, dest);
      // dispatch_capture(new_board.black, new_board.black_r,
      //                         new_board.white, new_board.white_r, dest);

      boards[*total] = new_board;
      (*total)++;

      // increment
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

template <int index, unsigned char sub_offset, int row_offset>
inline __attribute__((always_inline)) void
get_next_row_boards_black_r(const uint64_t occ, const board base_board,
                            int *total, move *moves,
                            board *boards) {
  unsigned short movers = (base_board.black_r[index] >> row_offset) & 0b11111111111;
  uint64_t row_moves;
  unsigned char local_orig, orig, orig_r, dest, dest_r;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig_r = row_offset + local_orig;
    orig = rotate_left[orig_r + sub_offset];
    const unsigned short blockers = ((uint64_t)occ >> row_offset) & 0b11111111111;
    row_moves = (uint64_t)row_moves_table[blockers][local_orig] << row_offset;
    while (row_moves) {
      // get destination
      dest_r = _tzcnt_u64(row_moves);
      dest = rotate_left[dest_r + sub_offset];

      // register move
      moves[*total] = (struct move){orig, dest};

      // Generate board
      board new_board = base_board;
      // TODO: move orig removal out and bench to see if it's faster
      new_board.black_r[index] ^= (uint64_t)1 << orig_r;
      new_board.black_r[index] |= (uint64_t)1 << dest_r;
      new_board.black[sub_layer[orig]] ^= (uint64_t)1
                                          << (orig - sub_layer_offset[orig]);
      new_board.black[sub_layer[dest]] |= (uint64_t)1
                                          << (dest - sub_layer_offset[dest]);
      capture_functions[dest](new_board.black, new_board.black_r,
                              new_board.white, new_board.white_r, dest);
      // dispatch_capture(new_board.black, new_board.black_r,
      //                         new_board.white, new_board.white_r, dest);
      boards[*total] = new_board;
      (*total)++;

      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

// -----------------------------------------------------------------------------
// integrated

inline __attribute__((always_inline)) void
get_team_moves_black(const board current, int *total, move *moves,
                     board *boards) {
  *total = 0;

  const layer occ = {
      current.black[0] | current.white[0] | current.king[0] | corners[0],
      current.black[1] | current.white[1] | current.king[1] | corners[1]};

  // lower 5 rows
  get_next_row_boards_black<0, 0, 0>(occ[0], current,  total, moves, boards);
  get_next_row_boards_black<0, 0, 11>(occ[0], current, total, moves, boards);
  get_next_row_boards_black<0, 0, 22>(occ[0], current, total, moves, boards);
  get_next_row_boards_black<0, 0, 33>(occ[0], current, total, moves, boards);
  get_next_row_boards_black<0, 0, 44>(occ[0], current, total, moves, boards);

  // upper 5 rows
  get_next_row_boards_black<1, 64, 2>(occ[1], current,  total, moves, boards);
  get_next_row_boards_black<1, 64, 13>(occ[1], current, total, moves, boards);
  get_next_row_boards_black<1, 64, 24>(occ[1], current, total, moves, boards);
  get_next_row_boards_black<1, 64, 35>(occ[1], current, total, moves, boards);
  get_next_row_boards_black<1, 64, 46>(occ[1], current, total, moves, boards);

  const layer occ_r = {
      current.black_r[0] | current.white_r[0] | current.king_r[0] | corners[0],
      current.black_r[1] | current.white_r[1] | current.king_r[1] | corners[1]};

  // lower 5 rows
  get_next_row_boards_black_r<0, 0, 0>(occ_r[0], current, total, moves, boards);
  get_next_row_boards_black_r<0, 0, 11>(occ_r[0], current, total, moves, boards);
  get_next_row_boards_black_r<0, 0, 22>(occ_r[0], current, total, moves, boards);
  get_next_row_boards_black_r<0, 0, 33>(occ_r[0], current, total, moves, boards);
  get_next_row_boards_black_r<0, 0, 44>(occ_r[0], current, total, moves, boards);

  // // upper 5 rows
  get_next_row_boards_black_r<1, 64, 2>( occ_r[1], current,  total, moves, boards);
  get_next_row_boards_black_r<1, 64, 13>( occ_r[1], current, total, moves, boards);
  get_next_row_boards_black_r<1, 64, 24>( occ_r[1], current, total, moves, boards);
  get_next_row_boards_black_r<1, 64, 35>( occ_r[1], current, total, moves, boards);
  get_next_row_boards_black_r<1, 64, 46>( occ_r[1], current, total, moves, boards);

  unsigned short movers, row_moves;
  unsigned char local_orig, orig, orig_r, dest;

  // center horizontal
  movers = (current.black[0] >> 55) | (((current.black[1] & 0x3) << 9) & 0b11111111111);
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = local_orig + 55;
    orig_r = rotate_right[orig];
    const unsigned short blockers_h =
      (occ[0] >> 55) | (((occ[1] & 0x3) << 9) & 0b11111111111);
    row_moves = center_row_moves_table[blockers_h][_tzcnt_u16(movers)];
    while (row_moves) {
      dest = _tzcnt_u16(row_moves) + 55;
      // pawns can't land on the throne
      if (dest == 60) {
        continue;
      }
      moves[*total] = (struct move){orig, dest};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }

  // center vertical
  movers = ((current.black_r[0] >> 55) | ((current.black_r[1] & 0x3) << 9)) & 0b11111111111;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = local_orig + 55;
    orig_r = rotate_left[orig];
    const unsigned short blockers_v =
      ((occ_r[0] >> 55) | ((occ_r[1] & 0x3) << 9)) & 0b11111111111;
    row_moves = center_row_moves_table[blockers_v][local_orig];
    while (row_moves) {
      dest = _tzcnt_u16(row_moves) + 55;
      // pawns can't land on the throne
      if (dest == 60) {
        continue;
      }
      moves[*total] = (struct move) {orig_r, rotate_left[dest]};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}
// -----------------------------------------------------------------------------

inline uint8_t get_team_move_count(const layer occ, const layer team,
			const layer occ_90, const layer team_90) {

  uint16_t prog;
  uint8_t total;
  unsigned short row;

  total = 0;

  //TODO: try having these functions assign to a single int and then extract positions at the end rather than doing it for each row
  // upper 5 rows
  get_row_total_moves<0>(&team[0], &occ[0],  &total);
  get_row_total_moves<11>(&team[0], &occ[0], &total);
  get_row_total_moves<22>(&team[0], &occ[0], &total);
  get_row_total_moves<33>(&team[0], &occ[0], &total);
  get_row_total_moves<44>(&team[0], &occ[0], &total);

  // lower 5 rows
  get_row_total_moves<2>(&team[1], &occ[1], &total);
  get_row_total_moves<13>(&team[1], &occ[1],  &total);
  get_row_total_moves<24>(&team[1], &occ[1],  &total);
  get_row_total_moves<35>(&team[1], &occ[1],  &total);
  get_row_total_moves<46>(&team[1], &occ[1],  &total);

  // upper 5 rows rotated
  get_row_total_moves<0>(&team_90[0], &occ_90[0], &total);
  get_row_total_moves<11>(&team_90[0], &occ_90[0],  &total);
  get_row_total_moves<22>(&team_90[0], &occ_90[0],  &total);
  get_row_total_moves<33>(&team_90[0], &occ_90[0],  &total);
  get_row_total_moves<44>(&team_90[0], &occ_90[0],  &total);

  // lower 5 rows rotated
  get_row_total_moves<2>(&team_90[1], &occ_90[1], &total);
  get_row_total_moves<13>(&team_90[1], &occ_90[1], &total);
  get_row_total_moves<24>(&team_90[1], &occ_90[1], &total);
  get_row_total_moves<35>(&team_90[1], &occ_90[1], &total);
  get_row_total_moves<46>(&team_90[1], &occ_90[1], &total);

  // center horizontal
  row = ((occ[0] >> 55) | ((occ[1] & 0x3) << 9));
  prog = (team[0] >> 55) | ((team[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  // center vertical
  row = ((occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9));
  prog = (team_90[0] >> 55) | ((team_90[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  return total;
}
static inline

//******************************************************************************


const char* start_board_string = \
  " .  .  .  X  X  X  X  X  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " X  .  .  .  .  O  .  .  .  .  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  X  .  O  O  #  O  O  .  X  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  .  .  .  .  O  .  .  .  .  X "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  X  X  X  X  X  .  .  . ";

void layer_or(layer base, const layer input) {
  base[0] |= input[0];
  base[1] |= input[1];
}

const char* corners_string =
  "X  .  .  .  .  .  .  .  .  .  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  .  .  .  .  .  .  .  .  .  X";


// void bench() {
//   printf("New 2: Running test\n");
// 
//   // read and verify boards
//   layer corners = {0,0};
//   read_layer(corners_string, 'X', corners);
//   print_layer(corners);
//   layer black = {0,0};
//   read_layer(start_board_string, 'X', black);
//   print_layer(black);
//   printf("\n");
//   layer white = {0,0};
//   read_layer(start_board_string, 'O', white);
//   print_layer(white);
//   printf("\n");
//   layer occ = {0,0};
//   layer_or(occ, corners);
//   layer_or(occ, black);
//   layer_or(occ, white);
//   print_layer(occ);
//   printf("\n");
//   /*
//   */
// 
//   // begin time
//   clock_t start, end;
//   double cpu_time_used;
//   start = clock();
// 
//   // setup
//   gen_row_moves();
//   gen_center_row_moves();
//   gen_row_move_counts();
//   gen_center_row_move_counts();
// 
// 
//   /*
//   move moves[235]; // 235 is a generous max move count
//   move moves_r[235]; // 235 is a generous max move count
//   int total;
//   get_team_moves(occ, black, occ, black, &total, moves, moves_r);
//   printf("move_count: %d\n", total);
// 
// 
//   // run for bench
//   int bench_count = 5000000;
//   while (bench_count) { 
//     get_team_moves(occ, black, occ, black, &total, moves, moves_r);
//     bench_count--;
//   }
//   */
// 
// 
//   // run for result
//   short total = get_team_move_count(occ, black, occ, black);
//   printf("move_count: %d\n", total);
// 
//   // run for bench
//   int bench_count = 25000000;
//   while (bench_count) { 
//     get_team_move_count(occ, black, occ, black);
//     bench_count--;
//   }
// 
//   // end time
//   end = clock();
//   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
//   printf("bench took %f seconds to execute \n", cpu_time_used); 
// }
