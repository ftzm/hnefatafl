#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <x86intrin.h>
#include <immintrin.h>
#include <avx2intrin.h>

/*******************************************************************************
 * Layer
 *
 * A layer is a representation of a set of board positions. Only
 * presence or absence is indicated. The two 64-bit integers making up
 * a layer are termed "sub layers", with the one at index 0 being
 * "lower" and the one at index 1 being "upper".
 ******************************************************************************/

typedef uint64_t layer[2];

const unsigned char sub_layer[121] = {
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

const unsigned int sub_layer_offset[121] = {
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

const unsigned char rotate_right[121] = {
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

void rotate_layer(const layer input, layer output) {
  int i, r;
   for (i = 0; i < 121; i++) {
     if (input[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
       r = rotate_right[i];
       output[sub_layer[r]] |= ((uint64_t) 1 << (r - sub_layer_offset[r]));
     }
  }
}

void read_layer(const char *string, unsigned char symbol, layer output) {
  int len = strlen(string);
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
  layer white_pawns;
  layer king;
  layer black_pawns;
} board;

/*******************************************************************************
 * Capture
 * 
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

uint64_t foe_masks[120][2];
uint64_t foe_masks_r[120][2];

void gen_foe_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    //printf("-------------------------------------------------\n");
    //printf("i: %d\n", i);
    modDest = i % 11;
    //printf("modDest: %d\n", modDest);
    if (i < 99) {
      //printf("north\n");
      target = i + 11;
      //printf("target: %d\n", target);
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      //printf("target_r: %d\n", target_r);
      foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      //foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r - sub_layer_offset[target_r]));
    }
    if (i > 21) {
      target = i - 11;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      //foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r - sub_layer_offset[target_r]));
    }
    if (modDest < 9) {
      target = i + 1;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      //foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r - sub_layer_offset[target_r]));
    }
    if (modDest > 1) {
      target = i - 1;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      //foe_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << (target_r - sub_layer_offset[target_r]));
    }
    //print_layer(foe_masks[i]);
    //print_layer(foe_masks_r[i]);
  }
}

uint64_t ally_masks[120][2];
uint64_t ally_masks_r[120][2];

void gen_ally_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 22;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 21) {
      target = i - 22;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 2;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 2;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      ally_masks_r[i][sub_layer[target_r]] |= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
    }
  }
}

typedef struct ally_layers {
  layer allies;
  layer allies_r;
} ally_layers;

//******************************************************************************
// Capture functions

/**
 * One might expect that it would be faster to repostion the ally bits
 * to the position of the foe bits and then do the bitwise and there,
 * but surprisingly it seems to be faster to extract both down to the
 * lowest 4 bits, do bitwise and, and then move the result back into
 * position.
 */
void capture_l(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const uint64_t *foe_mask = foe_masks[pos];
  const uint64_t attackers_0 = _pext_u64(ally_layers.allies[0], ally_masks[pos][0]);
  const uint64_t attackees_ext_0 = _pext_u64(foes[0], foe_mask[0]) & attackers_0;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  if (attackees_dep_0) {
    foes[0] -= attackees_dep_0;
    unsigned char r;
    while (attackees_dep_0) {
      r = rotate_right[_tzcnt_u64(attackees_dep_0)];
      foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
      //foes_r[sub_layer[r]] -= ((uint64_t) 1 << (r - sub_layer_offset[r]));
      attackees_dep_0 = _blsr_u64(attackees_dep_0);
    }
  }
}

void capture_u(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const uint64_t *foe_mask = foe_masks[pos];

  const uint64_t attackers_1 = _pext_u64(ally_layers.allies[1], ally_masks[pos][1]);
  const uint64_t attackees_ext_1 = _pext_u64(foes[1], foe_mask[1]) & attackers_1;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  if (attackees_dep_1) {
    foes[1] -= attackees_dep_1;

    unsigned char r;
    while (attackees_dep_1) {
      // TODO: could have a rotate lookup that is already offset by 64. though zobrist needs the real index so maybe a moot point.
      r = rotate_right[_tzcnt_u64(attackees_dep_1) + 64];
      foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
      //foes_r[sub_layer[r]] -= ((uint64_t) 1 << (r - sub_layer_offset[r]));
      attackees_dep_1 = _blsr_u64(attackees_dep_1);
    }
  }
}

void capture_x(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t *foe_mask = foe_masks[pos];
  uint64_t *ally_mask = ally_masks[pos];

  const uint64_t attackers_0 = _pext_u64(ally_layers.allies[0], ally_mask[0]);
  const uint64_t attackees_ext_0 = _pext_u64(foes[0], foe_mask[0]) & attackers_0;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes[0] -= attackees_dep_0;

  unsigned char r;
  while (attackees_dep_0) {
    r = rotate_right[_tzcnt_u64(attackees_dep_0)];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }

  const uint64_t attackers_1 = _pext_u64(ally_layers.allies[1], ally_mask[1]);
  const uint64_t attackees_ext_1 = _pext_u64(foes[1], foe_mask[1]) & attackers_1;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  foes[1] -= attackees_dep_1;

  while (attackees_dep_1) {
    // TODO: could have a rotate lookup that is already offset by 64. though zobrist needs the real index so maybe a moot point.
    r = rotate_right[_tzcnt_u64(attackees_dep_1) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
}

void capture_y(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {

  //print_layer(ally_layers.allies);

  //printf("foe_mask\n");
  uint64_t *foe_mask = foe_masks_r[pos];
  uint64_t *ally_mask = ally_masks_r[pos];

  //print_layer(foe_mask);
  //print_layer(ally_mask);

  const uint64_t attackers_0 = _pext_u64(ally_layers.allies_r[0], ally_mask[0]);
  //print_row(attackers_0);
  const uint64_t attackees_ext_0 = _pext_u64(foes_r[0], foe_mask[0]) & attackers_0;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes_r[0] -= attackees_dep_0;

  const uint64_t attackers_1 = _pext_u64(ally_layers.allies_r[1], ally_mask[1]);
  const uint64_t attackees_ext_1 = _pext_u64(foes_r[1], foe_mask[1]) & attackers_1;
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
    foes[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }


  while (attackees_dep_1) {
    r = rotate_left[_tzcnt_u64(attackees_dep_1) + 64];
    foes[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
  /*
  */

}

void capture_r(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {

  uint64_t *foe_mask = foe_masks_r[pos];
  uint64_t *ally_mask = ally_masks_r[pos];

  const uint64_t attackers_1 = _pext_u64(ally_layers.allies_r[1], ally_mask[1]);
  const uint64_t attackees_ext_1 = _pext_u64(foes_r[1], foe_mask[1]) & attackers_1;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  if (attackees_dep_1) {
    foes_r[1] -= attackees_dep_1;
    
    unsigned char r;
    while (attackees_dep_1) {
      r = rotate_left[_tzcnt_u64(attackees_dep_1) + 64];
      foes[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
      attackees_dep_1 = _blsr_u64(attackees_dep_1);
    }
  }
}

void capture_R(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {

  uint64_t *foe_mask = foe_masks_r[pos];
  uint64_t *ally_mask = ally_masks_r[pos];

  const uint64_t attackers_0 = _pext_u64(ally_layers.allies_r[0], ally_mask[0]);
  const uint64_t attackees_ext_0 = _pext_u64(foes_r[0], foe_mask[0]) & attackers_0;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  if (attackees_dep_0) {
    foes_r[0] -= attackees_dep_0;

    unsigned char r;
    while (attackees_dep_0) {
      r = rotate_left[_tzcnt_u64(attackees_dep_0)];
      foes[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
      attackees_dep_0 = _blsr_u64(attackees_dep_0);
    }
  }

}
void capture_b(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t *foe_mask = foe_masks[pos];
  uint64_t *ally_mask = ally_masks[pos];

  const uint64_t attackers_0 = _pext_u64(ally_layers.allies[0], ally_mask[0]);
  const uint64_t attackers_1 = _pext_u64(ally_layers.allies[1], ally_mask[1]);
  const uint64_t attackers_all = attackers_0 | (attackers_1 << 3);
  const uint64_t attackees_ext_0 = _pext_u64(foes[0], foe_mask[0]) & attackers_all;
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes[0] -= attackees_dep_0;

  unsigned char r;
  while (attackees_dep_0) {
    r = rotate_right[_tzcnt_u64(attackees_dep_0)];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }
}

void capture_B(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {

  /*
  printf("allies\n");
  print_layer(ally_layers.allies);
  printf("foes\n");
  print_layer(foes);
  */

  uint64_t *foe_mask = foe_masks[pos];
  uint64_t *ally_mask = ally_masks[pos];

  /*
  printf("foe mask\n");
  print_layer(foe_mask);
  printf("ally mask\n");
  print_layer(ally_mask);
  */

  const uint64_t attackers_0 = _pext_u64(ally_layers.allies[0], ally_mask[0]);
  const uint64_t attackers_1 = _pext_u64(ally_layers.allies[1], ally_mask[1]);
  const uint64_t attackers_all = attackers_0 | (attackers_1 << 1);
  const uint64_t attackees_ext_1 = _pext_u64(foes[1], foe_mask[1]) & attackers_all;
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
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
}

void capture_62(ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {

  /*
  printf("allies\n");
  print_layer(ally_layers.allies);
  printf("foes\n");
  print_layer(foes);
  */

  uint64_t *foe_mask = foe_masks[pos];
  uint64_t *ally_mask = ally_masks[pos];

  /*
  printf("foe mask\n");
  print_layer(foe_mask);
  printf("ally mask\n");
  print_layer(ally_mask);
  */

  const uint64_t attackers_0 = _pext_u64(ally_layers.allies[0], ally_mask[0]);
  const uint64_t attackers_1 = _pext_u64(ally_layers.allies[1], ally_mask[1]);

  const uint64_t borrowed = (attackers_1 & 1) << 2;
  const uint64_t attackees_ext_0 = _pext_u64(foes[0], foe_mask[0]) & (attackers_0 | borrowed);
  uint64_t attackees_dep_0 = _pdep_u64(attackees_ext_0, foe_mask[0]);
  foes[0] -= attackees_dep_0;

  const uint64_t attackees_ext_1 = _pext_u64(foes[1], foe_mask[1]) & (attackers_1 >> 1);
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
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_0 = _blsr_u64(attackees_dep_0);
  }


  while (attackees_dep_1) {
    // TODO: could have a rotate lookup that is already offset by 64. though zobrist needs the real index so maybe a moot point.
    r = rotate_right[_tzcnt_u64(attackees_dep_1) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }
  /*
  */

}

//******************************************************************************
// Components

inline uint64_t half_captures(const layer allies,
			      const layer foes,
			      const unsigned char pos,
			      const unsigned char half) {
  const uint64_t *foe_mask = foe_masks[pos];
  const uint64_t *ally_mask = ally_masks[pos];
  const uint64_t attackers = _pext_u64(allies[half], ally_mask[half]);
  const uint64_t attackees_ext = _pext_u64(foes[half], foe_mask[half]) & attackers;
  return _pdep_u64(attackees_ext, foe_mask[half]);
}

/**
   Given an unrotated lower layer half holding captures, rotate and
   apply those captures to a foe layer.
 */
inline void distribute_lower_right(uint64_t captures, layer foes_r) {
  unsigned char r;
  while (captures) {
    r = rotate_right[_tzcnt_u64(captures)];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    captures = _blsr_u64(captures);
  }
}

/**
   Given a rotated lower layer half holding captures, rotate left and
   apply those captures to a foe layer.
 */
inline void distribute_lower_left(uint64_t captures, layer foes_r) {
  unsigned char r;
  while (captures) {
    r = rotate_left[_tzcnt_u64(captures)];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    captures = _blsr_u64(captures);
  }
}

inline void distribute_upper_right(uint64_t captures, layer foes_r) {
  unsigned char r;
  while (captures) {
    r = rotate_right[_tzcnt_u64(captures) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    captures = _blsr_u64(captures);
  }
}

inline void distribute_upper_left(uint64_t captures, layer foes_r) {
  unsigned char r;
  while (captures) {
    r = rotate_left[_tzcnt_u64(captures) + 64];
    foes_r[sub_layer[r]] -= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    captures = _blsr_u64(captures);
  }
}


inline void lower_right_shield_captures(const unsigned short flank,
				 const unsigned short wall,
				 const unsigned short foes,
				 const unsigned char pos,
				 uint64_t *captures) {
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
  const unsigned short leftward = uppers[pos];
  //printf("leftward\n");
  //print_row(leftward);
  const unsigned short blockers = flank & leftward;
  //printf("blockers\n");
  //print_row(blockers);
  const unsigned short until = (blockers & -blockers) - 1;
  //printf("until\n");
  //print_row(until);
  const unsigned short mask = leftward & until;
  //printf("mask\n");
  //print_row(mask);
  const unsigned short candidates = mask & foes & wall;
  //printf("candidates\n");
  //print_row(candidates);
  if (mask == candidates) {
    (*captures) |= mask;
  }
}

inline void upper_right_shield_captures(const uint64_t allies,
					const uint64_t foes,
					const unsigned char pos,
					uint64_t *captures) {
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
  //printf("adjust pos: %d\n", pos - 110);
  //print_row(allies >> 35);
  //print_row(foes >> 46);
  const unsigned short rightward = lowers[pos - 110];
  //printf("rightward\n");
  //print_row(rightward);
  const unsigned short blockers = (allies >> 46) & rightward;
  //printf("blockers\n");
  //print_row(blockers);
  const unsigned short blocked = 0xFFFF >> __lzcnt16(blockers);
  //printf("blocked\n");
  //print_row(blocked);
  const unsigned short mask = (rightward - blocked);
  //printf("mask\n");
  //print_row(mask);
  const unsigned short candidates = mask & (foes >> 46) & (allies >> 35);
  if (mask == candidates) {
    (*captures) |= ((uint64_t) mask << 46);
  }
}

inline void upper_left_shield_captures(const uint64_t allies,
				       const uint64_t foes,
				       const unsigned char pos,
				       uint64_t *captures) {
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
  const unsigned short leftward = uppers[pos-110];
  //printf("leftward\n");
  //print_row(leftward);
  const unsigned short blockers = (allies >> 46) & leftward;
  //printf("blockers\n");
  //print_row(blockers);
  const unsigned short until = (blockers & -blockers) - 1;
  //printf("until\n");
  //print_row(until);
  const unsigned short mask = leftward & until;
  //printf("mask\n");
  //print_row(mask);
  const unsigned short candidates = mask & (foes >> 46) & (allies >> 35);
  //printf("candidates\n");
  //print_row(candidates);
  if (mask == candidates) {
    (*captures) |= ((uint64_t) mask << 46);
  }
}



//******************************************************************************
// side captures


//**************************************
// south

void capture_s(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t captures = half_captures(ally_layers.allies, foes, pos, 0);
  const uint64_t ally_lower = ally_layers.allies[0];
  lower_left_shield_captures(ally_lower, ally_lower >> 11, foes[0], pos, &captures);
  lower_right_shield_captures(ally_lower, ally_lower >> 11, foes[0], pos, &captures);
  foes[0] -= captures;
  distribute_lower_right(captures, foes_r);
}

void capture_se(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t captures = half_captures(ally_layers.allies, foes, pos, 0);
  const uint64_t ally_lower = ally_layers.allies[0];
  lower_left_shield_captures(ally_lower, ally_lower >> 11, foes[0], pos, &captures);
  foes[0] -= captures;
  distribute_lower_right(captures, foes_r);
}

void capture_sw(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t captures = half_captures(ally_layers.allies, foes, pos, 0);
  const uint64_t ally_lower = ally_layers.allies[0];
  lower_right_shield_captures(ally_lower, ally_lower >> 11, foes[0], pos, &captures);
  foes[0] -= captures;
  distribute_lower_right(captures, foes_r);
}

//**************************************
// east

void capture_e(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const unsigned char pos_r = rotate_right[pos];
  uint64_t captures = half_captures(ally_layers.allies_r, foes_r, pos_r, 0);
  const uint64_t ally_lower = ally_layers.allies_r[0];
  lower_left_shield_captures(ally_lower, ally_lower >> 11, foes_r[0], pos_r, &captures);
  lower_right_shield_captures(ally_lower, ally_lower >> 11, foes_r[0], pos_r, &captures);
  foes_r[0] -= captures;
  distribute_lower_left(captures, foes);
}

void capture_en(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const unsigned char pos_r = rotate_right[pos];
  uint64_t captures = half_captures(ally_layers.allies_r, foes_r, pos_r, 0);
  const uint64_t ally_lower = ally_layers.allies_r[0];
  lower_right_shield_captures(ally_lower, ally_lower >> 11, foes_r[0], pos_r, &captures);
  foes_r[0] -= captures;
  distribute_lower_left(captures, foes);
}

void capture_es(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const unsigned char pos_r = rotate_right[pos];
  uint64_t captures = half_captures(ally_layers.allies_r, foes_r, pos_r, 0);
  const uint64_t ally_lower = ally_layers.allies_r[0];
  lower_left_shield_captures(ally_lower, ally_lower >> 11, foes_r[0], pos_r, &captures);
  foes_r[0] -= captures;
  distribute_lower_left(captures, foes);
}

//**************************************
// west

void capture_w(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const unsigned char pos_r = rotate_right[pos];
  uint64_t captures = half_captures(ally_layers.allies_r, foes_r, pos_r, 1);
  const uint64_t ally_upper = ally_layers.allies_r[1];
  upper_left_shield_captures(ally_upper, foes_r[1], pos_r, &captures);
  upper_right_shield_captures(ally_upper, foes_r[1], pos_r, &captures);
  foes_r[1] -= captures;
  distribute_upper_left(captures, foes);
}

void capture_wn(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const unsigned char pos_r = rotate_right[pos];
  uint64_t captures = half_captures(ally_layers.allies_r, foes_r, pos_r, 1);
  const uint64_t ally_upper = ally_layers.allies_r[1];
  upper_right_shield_captures(ally_upper, foes_r[1], pos_r, &captures);
  foes_r[1] -= captures;
  distribute_upper_left(captures, foes);
}

void capture_ws(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  const unsigned char pos_r = rotate_right[pos];
  uint64_t captures = half_captures(ally_layers.allies_r, foes_r, pos_r, 1);
  const uint64_t ally_upper = ally_layers.allies_r[1];
  upper_left_shield_captures(ally_upper, foes_r[1], pos_r, &captures);
  foes_r[1] -= captures;
  distribute_upper_left(captures, foes);
}

//**************************************
// north

void capture_n(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t captures = half_captures(ally_layers.allies, foes, pos, 1);
  const uint64_t ally_upper = ally_layers.allies[1];
  upper_left_shield_captures(ally_upper, foes[1], pos, &captures);
  upper_right_shield_captures(ally_upper, foes[1], pos, &captures);
  foes[1] -= captures;
  distribute_upper_right(captures, foes_r);
}

void capture_ne(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t captures = half_captures(ally_layers.allies, foes, pos, 1);
  const uint64_t ally_upper = ally_layers.allies[1];
  upper_right_shield_captures(ally_upper, foes[1], pos, &captures);
  foes[1] -= captures;
  distribute_upper_right(captures, foes_r);
}

void capture_nw(const ally_layers ally_layers, layer foes, layer foes_r, const unsigned char pos) {
  uint64_t captures = half_captures(ally_layers.allies, foes, pos, 1);
  const uint64_t ally_upper = ally_layers.allies[1];
  upper_left_shield_captures(ally_upper, foes[1], pos, &captures);
  foes[1] -= captures;
  distribute_upper_right(captures, foes_r);
}


//******************************************************************************

void (*index_functions[121])(const ally_layers, layer, layer, const unsigned char) = {
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


/*
 capture map

  (rotated 180)
 
  00 | _,  se, se, s,  s,  s,  s,  s,  sw, sw, _,
  11 | es, l,  l,  l,  l,  l,  l,  l,  l,  l,  ws,
   2 | es, l,  l,  l,  l,  l,  l,  l,  l,  l,  ws,
   3 | e,  l,  l,  l,  l,  l,  l,  l,  l,  r,  w,
   4 | e,  R,  R,  R,  b,  y,  y,  b,  r,  r,  w,
   5 | e,  R,  R,  R,  x,  x,  x,  62, r,  r,  w,
   6 | e,  R,  R,  R,  x,  x,  x,  x,  r,  r,  w,
   7 | e,  R,  R,  R,  B,  y,  y,  B,  r,  u,  w,
   8 | en, u,  u,  u,  u,  u,  u,  u,  u,  u,  wn,
   9 | en, u,  u,  u,  u,  u,  u,  u,  u,  u,  wn,
  10 | _,  ne, ne, n,  n,  n,  n,  n,  nw, nw, _,

l: capture check on [0] only
u: capture check on [1] only
n: u + north shield wall check both directions
ne: u + north shield wall check westwards
nw: u + north shield wall check eastwards
s: l + south shield wall check both directions
r: rotated u
R: rotated l
e: R + east shield wall check both directions
w: r + west shield wall check both directions
x: capture check on both [0] and [1]
y: rotated x
b: capture check on lower, ally check on above and overlay the maybe present bit from upper position in highest spot
B: capture check on upper, ally check on ally and overlay the maybe present bit from lower position in lowest spot, shifting to make space
62: here the north capture is entirely in [1], but the ally for the west capture also is, so we need to move that down.
*/

void apply_captures_niave(layer friends, layer foes, layer output, int dest) {
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
/*******************************************************************************
 * Moves
 *
 ******************************************************************************/

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
  unsigned short rightward = lowers[_tzcnt_u16(upper | 0x800)];
  unsigned short blocked = 0xFFFF >> __lzcnt16(lower);
  return (rightward - blocked) ^ (1 << pos);
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
  char output[12];
  memset(output, '0', 11);
  output[11] = '\0';
  int index;
  while (row) {
    index = _tzcnt_u16(row);
    output[10 - index] = '1';
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
 
inline void get_row_total_moves(const uint64_t team, const uint64_t occ,
			        const int offset, uint8_t *total) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
  while (movers) {
    (*total) += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }
}

inline void get_row_total_moves_2(const uint64_t team, const uint64_t occ, const int offset, uint8_t *total) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
  while (movers) {
    (*total) += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }
}

inline uint8_t get_row_total_moves_3(const uint64_t team, const uint64_t occ, const int offset) {
  uint8_t total = 0;
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }
  return total;
}

inline void get_all_row_moves(const uint64_t team, const uint64_t occ,
			      const int offset, int *total, move *moves, move *moves_r) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  //const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
  uint64_t row_moves;
  unsigned char local_orig, orig, orig_r, dest;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = offset + local_orig;
    orig_r = rotate_right[orig];
    const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
    row_moves = (uint64_t) row_moves_table[blockers][local_orig] << offset;
    while (row_moves) {
      dest = _tzcnt_u16(row_moves);
      moves[*total] = (struct move) {orig, dest};
      moves_r[*total] = (struct move) {orig_r, rotate_right[dest]};
      (*total)++;
      // todo: use "faster" version of this and bench
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

inline void get_all_row_moves_r(const uint64_t team, const uint64_t occ,
				const int offset, int *total, move *moves, move *moves_r) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  unsigned short row_moves;
  unsigned char local_orig, orig, orig_r, dest;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = offset + local_orig;
    orig_r = rotate_left[orig];
    const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
    row_moves = row_moves_table[blockers][local_orig];
    while (row_moves) {
      dest = offset + _tzcnt_u16(row_moves);
      moves[*total] = (struct move) {orig_r, rotate_left[dest]};
      moves_r[*total] = (struct move) {orig, dest};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

void get_team_moves(const layer occ, const layer team,
		    const layer occ_90, const layer team_90,
		    int *total, move *moves, move *moves_r) {
  *total = 0;

  // upper 5 rows
  get_all_row_moves(team[0], occ[0], 0, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 11, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 22, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 33, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 44, total, moves, moves_r);

  // lower 5 rows
  get_all_row_moves(team[1], occ[1], 2, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 13, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 24, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 35, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 46, total, moves, moves_r);

  // upper 5 rows
  get_all_row_moves_r(team_90[0], occ_90[0], 0, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 11, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 22, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 33, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 44, total, moves, moves_r);

  // lower 5 rows
  get_all_row_moves_r(team_90[1], occ_90[1], 2, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 13, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 24, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 35, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 46, total, moves, moves_r);

  unsigned short movers, row_moves;
  unsigned char local_orig, orig, orig_r, dest;

  // center horizontal
  movers = (team[0] >> 55) | (((team[1] & 0x3) << 9) & 0b11111111111);
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = local_orig + 55;
    orig_r = rotate_right[orig];
    const unsigned short blockers_h =
      (occ[0] >> 55) | (((occ[1] & 0x3) << 9) & 0b11111111111);
    row_moves = center_row_moves_table[blockers_h][_tzcnt_u16(movers)];
    while (row_moves) {
      dest = _tzcnt_u16(row_moves) + 55;
      moves[*total] = (struct move) {orig, dest};
      moves_r[*total] = (struct move) {orig_r, rotate_right[dest]};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }

  // center vertical
  movers = ((team_90[0] >> 55) | ((team_90[1] & 0x3) << 9)) & 0b11111111111;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = local_orig + 55;
    orig_r = rotate_left[orig];
    const unsigned short blockers_v =
      ((occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9)) & 0b11111111111;
    row_moves = center_row_moves_table[blockers_v][local_orig];
    while (row_moves) {
      dest = _tzcnt_u16(row_moves) + 55;
      moves[*total] = (struct move) {orig_r, rotate_left[dest]};
      moves_r[*total] = (struct move) {orig, dest};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

uint8_t get_team_move_count(const layer occ, const layer team,
			const layer occ_90, const layer team_90) {

  uint16_t prog;
  uint8_t total;
  unsigned short row;

  total = 0;

  //TODO: try having these functions assign to a single int and then extract positions at the end rather than doing it for each row
  // upper 5 rows
  get_row_total_moves(team[0], occ[0], 0, &total);
  get_row_total_moves(team[0], occ[0], 11, &total);
  get_row_total_moves(team[0], occ[0], 22, &total);
  get_row_total_moves(team[0], occ[0], 33, &total);
  get_row_total_moves(team[0], occ[0], 44, &total);

  // lower 5 rows
  get_row_total_moves(team[1], occ[1], 2, &total);
  get_row_total_moves(team[1], occ[1], 13, &total);
  get_row_total_moves(team[1], occ[1], 24, &total);
  get_row_total_moves(team[1], occ[1], 35, &total);
  get_row_total_moves(team[1], occ[1], 46, &total);

  // upper 5 rows rotated
  get_row_total_moves(team_90[0], occ_90[0], 0, &total);
  get_row_total_moves(team_90[0], occ_90[0], 11, &total);
  get_row_total_moves(team_90[0], occ_90[0], 22, &total);
  get_row_total_moves(team_90[0], occ_90[0], 33, &total);
  get_row_total_moves(team_90[0], occ_90[0], 44, &total);

  // lower 5 rows rotated
  get_row_total_moves(team_90[1], occ_90[1], 2, &total);
  get_row_total_moves(team_90[1], occ_90[1], 13, &total);
  get_row_total_moves(team_90[1], occ_90[1], 24, &total);
  get_row_total_moves(team_90[1], occ_90[1], 35, &total);
  get_row_total_moves(team_90[1], occ_90[1], 46, &total);

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

// -----------------
// https://stackoverflow.com/questions/54541129/how-to-count-character-occurrences-using-simd
__m256i hsum_epu8_epu64(__m256i v) {
    return _mm256_sad_epu8(v, _mm256_setzero_si256());  // SAD against zero is a handy trick
}

static inline
uint64_t hsum_epu64_scalar(__m256i v) {
    __m128i lo = _mm256_castsi256_si128(v);
    __m128i hi = _mm256_extracti128_si256(v, 1);
    __m128i sum2x64 = _mm_add_epi64(lo, hi);   // narrow to 128

    hi = _mm_unpackhi_epi64(sum2x64, sum2x64);
    __m128i sum = _mm_add_epi64(hi, sum2x64);  // narrow to 64
    return _mm_cvtsi128_si64(sum);
}
// -----------------

short get_team_move_count_2(const layer occ, const layer team,
			  const layer occ_90, const layer team_90) {

  uint16_t prog;
  uint8_t results[32] = {0};
  unsigned short row;

  //TODO: try having these functions assign to a single int and then extract positions at the end rather than doing it for each row
  // upper 5 rows
  get_row_total_moves_2(team[0], occ[0], 0,  &results[0]);
  get_row_total_moves_2(team[0], occ[0], 11, &results[1]);
  get_row_total_moves_2(team[0], occ[0], 22, &results[2]);
  get_row_total_moves_2(team[0], occ[0], 33, &results[3]);
  get_row_total_moves_2(team[0], occ[0], 44, &results[4]);

  // lower 5 rows
  get_row_total_moves_2(team[1], occ[1], 2,  &results[5]);
  get_row_total_moves_2(team[1], occ[1], 13, &results[6]);
  get_row_total_moves_2(team[1], occ[1], 24, &results[7]);
  get_row_total_moves_2(team[1], occ[1], 35, &results[8]);
  get_row_total_moves_2(team[1], occ[1], 46, &results[9]);

  // upper 5 rows rotated
  get_row_total_moves_2(team_90[0], occ_90[0], 0,  &results[10]);
  get_row_total_moves_2(team_90[0], occ_90[0], 11, &results[11]);
  get_row_total_moves_2(team_90[0], occ_90[0], 22, &results[12]);
  get_row_total_moves_2(team_90[0], occ_90[0], 33, &results[13]);
  get_row_total_moves_2(team_90[0], occ_90[0], 44, &results[14]);

  // lower 5 rows rotated
  get_row_total_moves_2(team_90[1], occ_90[1], 2,  &results[15]);
  get_row_total_moves_2(team_90[1], occ_90[1], 13, &results[16]);
  get_row_total_moves_2(team_90[1], occ_90[1], 24, &results[17]);
  get_row_total_moves_2(team_90[1], occ_90[1], 35, &results[18]);
  get_row_total_moves_2(team_90[1], occ_90[1], 46, &results[19]);

  // center horizontal
  row = ((occ[0] >> 55) | ((occ[1] & 0x3) << 9));
  prog = (team[0] >> 55) | ((team[1] & 0x3) << 9);
  while (prog) {
    results[20] += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  // center vertical
  row = ((occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9));
  prog = (team_90[0] >> 55) | ((team_90[1] & 0x3) << 9);
  while (prog) {
    results[21] += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  return hsum_epu64_scalar(hsum_epu8_epu64(_mm256_load_si256((__m256i*)&results[0])));
}

uint8_t get_team_move_count_3(const layer occ, const layer team,
			const layer occ_90, const layer team_90) {

  uint16_t prog;
  unsigned short row;


  //TODO: try having these functions assign to a single int and then extract positions at the end rather than doing it for each row
  // upper 5 rows
  uint8_t r0 = get_row_total_moves_3(team[0], occ[0], 0);
  uint8_t r1 = get_row_total_moves_3(team[0], occ[0], 11);
  uint8_t r2 = get_row_total_moves_3(team[0], occ[0], 22);
  uint8_t r3 = get_row_total_moves_3(team[0], occ[0], 33);
  uint8_t r4 = get_row_total_moves_3(team[0], occ[0], 44);

  // lower 5 rows
  uint8_t r5 = get_row_total_moves_3(team[1], occ[1], 2);
  uint8_t r6 = get_row_total_moves_3(team[1], occ[1], 13);
  uint8_t r7 = get_row_total_moves_3(team[1], occ[1], 24);
  uint8_t r8 = get_row_total_moves_3(team[1], occ[1], 35);
  uint8_t r9 = get_row_total_moves_3(team[1], occ[1], 46);

  // upper 5 rows rotated
  uint8_t r10 = get_row_total_moves_3(team_90[0], occ_90[0], 0);
  uint8_t r11 = get_row_total_moves_3(team_90[0], occ_90[0], 11);
  uint8_t r12 = get_row_total_moves_3(team_90[0], occ_90[0], 22);
  uint8_t r13 = get_row_total_moves_3(team_90[0], occ_90[0], 33);
  uint8_t r14 = get_row_total_moves_3(team_90[0], occ_90[0], 44);

  // lower 5 rows rotated
  uint8_t r15 = get_row_total_moves_3(team_90[1], occ_90[1], 2);
  uint8_t r16 = get_row_total_moves_3(team_90[1], occ_90[1], 13);
  uint8_t r17 = get_row_total_moves_3(team_90[1], occ_90[1], 24);
  uint8_t r18 = get_row_total_moves_3(team_90[1], occ_90[1], 35);
  uint8_t r19 = get_row_total_moves_3(team_90[1], occ_90[1], 46);

  // center horizontal
  row = ((occ[0] >> 55) | ((occ[1] & 0x3) << 9));
  prog = (team[0] >> 55) | ((team[1] & 0x3) << 9);
  uint8_t r20 = 0;
  while (prog) {
    r20 += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  // center vertical
  row = ((occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9));
  prog = (team_90[0] >> 55) | ((team_90[1] & 0x3) << 9);
  uint8_t r21 = 0;
  while (prog) {
    r21 += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  return (r0 + r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10 + r11 + r12 + r13 + r14 + r15 + r16 + r17 + r18 + r19 + r20 + r21);
}


//******************************************************************************


const char* start_board_string =
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


void bench() {
  printf("New 2: Running test\n");

  // read and verify boards
  layer corners = {0,0};
  read_layer(corners_string, 'X', corners);
  print_layer(corners);
  layer black = {0,0};
  read_layer(start_board_string, 'X', black);
  print_layer(black);
  printf("\n");
  layer white = {0,0};
  read_layer(start_board_string, 'O', white);
  print_layer(white);
  printf("\n");
  layer occ = {0,0};
  layer_or(occ, corners);
  layer_or(occ, black);
  layer_or(occ, white);
  print_layer(occ);
  printf("\n");
  /*
  */

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  // setup
  //gen_row_moves();
  //gen_center_row_moves();
  gen_row_move_counts();
  gen_center_row_move_counts();


  /*
  move moves[235]; // 235 is a generous max move count
  move moves_r[235]; // 235 is a generous max move count
  int total;
  get_team_moves(occ, black, occ, black, &total, moves, moves_r);
  printf("move_count: %d\n", total);


  // run for bench
  int bench_count = 5000000;
  while (bench_count) { 
    get_team_moves(occ, black, occ, black, &total, moves, moves_r);
    bench_count--;
  }
  /*
  */


  // run for result
  short total = get_team_move_count_3(occ, black, occ, black);
  printf("move_count: %d\n", total);


  // run for bench
  int bench_count = 25000000;
  while (bench_count) { 
    get_team_move_count_3(occ, black, occ, black);
    bench_count--;
  }

  /*
  */

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
}
