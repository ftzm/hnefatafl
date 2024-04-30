#pragma once

#include <x86intrin.h>
#include <string>
#include "layer.cpp"
#include "capture.cpp"

using std::string;

/*******************************************************************************
 * Moves
 *
 ******************************************************************************/

// Use if no intrinsics or need fewer levels for constexpr
/*
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
*/

/**
 * find positions that can be moved to from a position given the
 * occupancy of a row.
 * @param occ occupancy of the row, including the bit at `pos`.
 * @param pos the index of the starting position of the moves. 
 * @return a uint16_t where the set bits represent positions that can
 * be moved to.
 */
uint16_t get_row_moves(const uint16_t occ, const uint16_t pos) {
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
    // The below is only used by `rightward` when `upper` is empty
    0b11111111111
  };

  // set bits to the right of the position
  uint16_t lower = occ & lowers[pos];
  // set bits to the left of the position
  uint16_t upper = occ & (0b11111111110 << pos);
  // a mask which begins to the right of the lowest set bit in `upper`
  // (or at the very leftmost position of the row if there are no bits
  // set in `upper`) and extends to the right of the row
  uint16_t rightward = lowers[__tzcnt_u16(upper | 0x800)];
  // a mask which begins at the highest set bit of `lower` and extends
  // to the right of the row. "blocked" because it represents the
  // positions that can't be reached when moving rightwards
  uint16_t blocked = 0xFFFF >> __lzcnt16(lower);
  // subtract the blocked positions from the rightward set of
  // positions, leaving only those that can be reached. Also remove
  // the current position.
  return (rightward - blocked) - (1 << pos);
}

void print_row(uint16_t row) {
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

/*
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
*/

uint16_t row_moves_table[2048][11];
uint16_t center_row_moves_table[2048][11];

/**
 * Populate the global lookup table of row moves
 */
void gen_row_moves() {
  uint16_t row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      row_moves_table[row][pos] = get_row_moves(row, pos);
    }
  }
}

/**
 * Populate the global lookup table of row moves specifically for the
 * center row (excludes the center square)
 */
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

constexpr array<array<uint8_t, 11>, 2048> gen_row_move_counts_c() {
  array<array<uint8_t, 11>, 2048> res{};
  for (uint16_t row = 0; row < 2048; row++) {
    for (uint8_t pos = 0; pos < 11; pos++) {
      res[row][pos] = __builtin_popcount(get_row_moves(row, pos));
    }
  }
  return res;
}

constexpr array<array<uint8_t, 11>, 2048> test_row_move_count_table = gen_row_move_counts_c();

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

//******************************************************************************
// Capture Destinations
//******************************************************************************

constexpr layer drop_2_west = read_layer(".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X"
                                         ".  .  X  X  X  X  X  X  X  X  X",
                                         'X');

constexpr layer drop_2_east = read_layer("X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  ."
                                         "X  X  X  X  X  X  X  X  X  .  .",
                                         'X');

// layer find_capture_destinations(const layer allies, const layer foes) {
//   layer north = layer_shiftl<11>(layer_and(layer_shiftl<11>(allies), foes));
//   layer south = layer_shiftr<11>(layer_and(layer_shiftr<11>(allies), foes));
//   layer east = layer_shiftr<1>(
//       layer_and(layer_shiftr<1>(layer_and(allies, drop_2_east)), foes));
//   layer west = layer_shiftl<1>(
//       layer_and(layer_shiftl<1>(layer_and(allies, drop_2_west)), foes));
//   layer empty = layer_negate(layer_or(allies, foes));
//   return {(north[0] | south[0] | east[0] | west[0]) & empty[0],
//           (north[1] | south[1] | east[1] | west[1]) & empty[1]};
// }

layer find_capture_destinations_op(const layer allies, const layer foes) {
  layer north = (((allies << 11) & foes) << 11);
  layer south = (((allies >> 11) & foes) >> 11);
  layer east = ((((allies & drop_2_east) >> 1) & foes) >> 1);
  layer west = ((((allies & drop_2_west) << 1) & foes) << 1);
  return {(north[0] | south[0] | east[0] | west[0]) & (~(allies[0] | foes[0])),
          (north[1] | south[1] | east[1] | west[1]) & (~(allies[1] | foes[1]))};
}


//******************************************************************************
// Quiescence
//******************************************************************************

/**
 * Requires that the bit at pos is 0.
 */
uint16_t find_neighbors(const uint16_t occ, const int pos) {
  // printf("occ\n");
  // print_row(occ);
  /*
  static const unsigned short lower_masks[11] = {
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
  };
  */
  // uint16_t lower = 0b1000000000000000 >> __lzcnt16(lower_masks[pos] & occ);
  uint16_t lower = 0b1000000000000000 >> __lzcnt16(((1 << pos) - 1) & occ);
  uint16_t uppers = occ & (0b11111111110 << pos);
  uint16_t upper = uppers & -uppers;
  return lower | upper;
}

#define board_layer(is_black, is_rotated)                                      \
  (is_black ? (is_rotated ? board.black_r : board.black)                       \
            : (is_rotated ? board.white_r : board.white))

#define offset_coords(pred, amount)			\
  (pred ? (orig += amount, dest += amount, noop) : noop)

#define select_rotation(is_rotated) (is_rotated ? rotate_left : rotate_right)

#define select_index(is_lower, is_upper, var) (is_lower ? 0 : (is_upper ? 1 : sub_layer[var]))

/** process a capture move
 * 
 * @tparam is_black whether the moving piece is black
 * @tparam is_rotated whether the move is rotated, in which case
 * selection of rotated or unrotated layers, rotation direction, etc.,
 * is reversed
 *
 * @param board the board to be updated
 * @param orig
 * @param dest
 */
template <bool is_black, bool is_rotated, bool is_lower, bool is_upper>
void process_capture_move(const board *base_board, board *boards, move *moves,
                  int *total, uint8_t orig, uint8_t dest) {

  board board = *base_board;

  // if neither upper nor lower then this is a center row, and we should offset
  // by 55 from the start
  offset_coords(!is_lower && !is_upper, 55);

  board_layer(is_black, is_rotated)[select_index(is_lower, is_upper, orig)] -=
      (uint64_t)1 << orig;
  board_layer(is_black, is_rotated)[select_index(is_lower, is_upper, dest)] |=
      (uint64_t)1 << dest;

  // if upper we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  offset_coords(is_upper, 64);

  uint8_t orig_r = select_rotation(is_rotated)[orig];
  uint8_t dest_r = select_rotation(is_rotated)[dest];

  board_layer(is_black, !is_rotated)[sub_layer[orig_r]] -=
      (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
  board_layer(is_black, !is_rotated)[sub_layer[dest_r]] |=
      (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

  capture_functions[(is_rotated ? dest_r : dest)](board_layer(is_black, false),
						  board_layer(is_black, true),
						  board_layer(!is_black, false),
						  board_layer(!is_black, true),
						  (is_rotated ? dest_r : dest));


  shield_wall<is_black>(&board, (is_rotated ? dest_r : dest));

  moves[*total] =
      (struct move){(is_rotated ? orig_r : orig), (is_rotated ? dest_r : dest)};
  boards[*total] = board;
  (*total)++;
}

/**
 * generate captures moves for one half of the board, excluding center
 * rank or file.
 */
template <bool is_black, bool is_rotated, bool is_lower, bool is_upper>
void capture_moves(board *boards, const board board, int *total,
                             move *moves, layer occ, layer capt_dests) {
  uint64_t masked_capt_dests = capt_dests[(is_lower ? 0 : 1)] & (is_lower ? lower_rows_mask : upper_rows_mask);
  while (masked_capt_dests) {
    uint8_t dest = _tzcnt_u64(masked_capt_dests);
    int row_offset = (is_lower ? sub_layer_row_offset : sub_layer_row_offset_upper)[dest];
    int row_dest = dest - row_offset;
    uint16_t neighbors = find_neighbors(
         0b11111111111 & (occ[(is_lower ? 0 : 1)] >> row_offset), row_dest);
    uint64_t origs = ((uint64_t)neighbors << row_offset) &
                     board_layer(is_black, is_rotated)[(is_lower ? 0 : 1)];
    while (origs) {
      uint8_t orig = _tzcnt_u64(origs);
      process_capture_move<is_black, is_rotated, is_lower, is_upper>(
          &board, boards, moves, total, orig, dest);
      origs = _blsr_u64(origs);
    }
    masked_capt_dests = _blsr_u64(masked_capt_dests);
  }
}

#define get_center_row(layer) (layer[0] >> 55) | (((layer[1] & 0x3) << 9) & 0b11111111111);

/**
 * generate captures moves for the center rank or file.
 */
template <bool is_black, bool is_rotated, bool is_lower, bool is_upper>
void capture_moves_center(board *boards, const board board, int *total, move *moves, layer occ, layer capture_dests) {
  uint16_t center_capt_dests = get_center_row(capture_dests);
  uint16_t center_occ = get_center_row(occ);
  while (center_capt_dests) {
    uint8_t dest = _tzcnt_u16(center_capt_dests);
    uint16_t neighbors = find_neighbors(center_occ, dest);
    uint64_t row_origs = neighbors & get_center_row(board_layer(is_black, is_rotated));
    while (row_origs) {
      uint8_t orig = _tzcnt_u16(row_origs);
      process_capture_move<is_black, is_rotated, false, false>(&board, boards, moves, total, orig, dest);
      row_origs &= row_origs - 1;
    }
    center_capt_dests &= center_capt_dests - 1;
  }
}

/**
 * generate moves which result in captures.
 */
template <bool is_black>
void get_capture_move_boards(board *boards, const board board, int *total, move *moves) {
  layer occ = board.black | board.white | board.king;
  layer occ_r = board.black_r | board.white_r | board.king_r;
  layer capt_dests = find_capture_destinations_op(board_layer(is_black, false) | corners, board_layer(!is_black, false));
  layer capt_dests_r = rotate_layer(capt_dests); // maybe faster to do above rather than rotate

  capture_moves<is_black, false, true, false>(boards, board, total, moves, occ, capt_dests);
  capture_moves<is_black, false, false, true>(boards, board, total, moves, occ, capt_dests);
  capture_moves_center<is_black, false, false, false>(boards, board, total, moves, occ, capt_dests);
  capture_moves<is_black, true, true, false>(boards, board, total, moves, occ_r, capt_dests_r);
  capture_moves<is_black, true, false, true>(boards, board, total, moves, occ_r, capt_dests_r);
  capture_moves_center<is_black, true, false, false>(boards, board, total, moves, occ_r, capt_dests_r);

}

//******************************************************************************
// Quiescence
//******************************************************************************

template <int index, unsigned char sub_index, int row_offset>
inline __attribute__((always_inline)) void
get_next_row_boards_black(const uint64_t occ, const board &base_board,
                          int *total, move *moves, board *boards, const uint64_t cap_dests) {
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

      moves[*total] = (struct move){orig, dest};

      board new_board = base_board;
      new_board.black[index] -= (uint64_t)1 << sub_orig;
      new_board.black[index] |= (uint64_t)1 << sub_dest;
      new_board.black_r[sub_layer[orig_r]] -=
          (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
      new_board.black_r[sub_layer[dest_r]] |=
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

      if (cap_dests & (1 << sub_dest)) {
        capture_functions[dest](new_board.black, new_board.black_r,
                                new_board.white, new_board.white_r, dest);
      }
      shield_wall<true>(&new_board, dest);

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
                            board *boards, const layer cap_dests) {
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

      
      if (cap_dests[sub_layer[dest]] & (1 << (dest - sub_layer_offset[dest]))) {
	capture_functions[dest](new_board.black, new_board.black_r,
				new_board.white, new_board.white_r, dest);
      }

      shield_wall<true>(&new_board, dest);

      boards[*total] = new_board;
      (*total)++;

      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

template <int index, unsigned char sub_index, int row_offset>
inline __attribute__((always_inline)) void
get_next_row_boards_white(const uint64_t occ, const board &base_board,
                          int *total, move *moves, board *boards, const uint64_t cap_dests) {
  unsigned short movers =
      (base_board.white[index] >> row_offset) & 0b11111111111;
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
      new_board.white[index] -= (uint64_t)1 << sub_orig;
      new_board.white[index] |= (uint64_t)1 << sub_dest;
      new_board.white_r[sub_layer[orig_r]] -=
	  // (uint64_t)1 << (orig_r - sub_layer_offset[orig_r]);
          (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
      new_board.white_r[sub_layer[dest_r]] |=
	  // (uint64_t)1 << (dest_r - sub_layer_offset[dest_r]);
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

      if (cap_dests & (1 << sub_dest)) {
        capture_functions[dest](new_board.white | new_board.king,
                                new_board.white_r | new_board.king_r,
                                new_board.black, new_board.black_r, dest);
      }

      shield_wall<false>(&new_board, dest);

      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

template <int index, unsigned char sub_offset, int row_offset>
inline __attribute__((always_inline)) void
get_next_row_boards_white_r(const uint64_t occ, const board base_board,
                            int *total, move *moves,
                            board *boards, const layer cap_dests) {
  unsigned short movers = (base_board.white_r[index] >> row_offset) & 0b11111111111;
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
      new_board.white_r[index] ^= (uint64_t)1 << orig_r;
      new_board.white_r[index] |= (uint64_t)1 << dest_r;
      new_board.white[sub_layer[orig]] ^= (uint64_t)1
                                          << (orig - sub_layer_offset[orig]);
      new_board.white[sub_layer[dest]] |= (uint64_t)1
                                          << (dest - sub_layer_offset[dest]);

      
      if (cap_dests[sub_layer[dest]] & (1 << (dest - sub_layer_offset[dest]))) {
        capture_functions[dest](new_board.white | new_board.king,
                                new_board.white_r | new_board.king_r,
                                new_board.black, new_board.black_r, dest);
      }

      shield_wall<false>(&new_board, dest);

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

  const layer capture_dests = find_capture_destinations_op(current.black, current.white);

  // lower 5 rows
  get_next_row_boards_black<0, 0, 0>(occ[0], current,  total, moves, boards, capture_dests[0]);
  get_next_row_boards_black<0, 0, 11>(occ[0], current, total, moves, boards, capture_dests[0]);
  get_next_row_boards_black<0, 0, 22>(occ[0], current, total, moves, boards, capture_dests[0]);
  get_next_row_boards_black<0, 0, 33>(occ[0], current, total, moves, boards, capture_dests[0]);
  get_next_row_boards_black<0, 0, 44>(occ[0], current, total, moves, boards, capture_dests[0]);

  // upper 5 rows
  get_next_row_boards_black<1, 64, 2>(occ[1], current,  total, moves, boards, capture_dests[1]);
  get_next_row_boards_black<1, 64, 13>(occ[1], current, total, moves, boards, capture_dests[1]);
  get_next_row_boards_black<1, 64, 24>(occ[1], current, total, moves, boards, capture_dests[1]);
  get_next_row_boards_black<1, 64, 35>(occ[1], current, total, moves, boards, capture_dests[1]);
  get_next_row_boards_black<1, 64, 46>(occ[1], current, total, moves, boards, capture_dests[1]);

  const layer occ_r = {
      current.black_r[0] | current.white_r[0] | current.king_r[0] | corners[0],
      current.black_r[1] | current.white_r[1] | current.king_r[1] | corners[1]};

  // lower 5 rows
  get_next_row_boards_black_r<0, 0, 0>(occ_r[0],  current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<0, 0, 11>(occ_r[0], current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<0, 0, 22>(occ_r[0], current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<0, 0, 33>(occ_r[0], current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<0, 0, 44>(occ_r[0], current, total, moves, boards, capture_dests);

  // // upper 5 rows
  get_next_row_boards_black_r<1, 64, 2>( occ_r[1],  current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<1, 64, 13>( occ_r[1], current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<1, 64, 24>( occ_r[1], current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<1, 64, 35>( occ_r[1], current, total, moves, boards, capture_dests);
  get_next_row_boards_black_r<1, 64, 46>( occ_r[1], current, total, moves, boards, capture_dests);

  // center horizontal
  {
    uint16_t movers = (current.black[0] >> 55) |
                      (((current.black[1] & 0x3) << 9) & 0b11111111111);
    while (movers) {
      uint8_t local_orig = _tzcnt_u16(movers);
      uint8_t orig = local_orig + 55;
      uint8_t orig_r = rotate_right[orig];
      const unsigned short blockers_h =
          (occ[0] >> 55) | (((occ[1] & 0x3) << 9) & 0b11111111111);
      uint16_t row_moves =
          center_row_moves_table[blockers_h][_tzcnt_u16(movers)];
      while (row_moves) {
        uint8_t dest = _tzcnt_u16(row_moves) + 55;
        // pawns can't land on the throne
        if (dest == 60) {
          continue;
        }
        uint8_t dest_r = rotate_right[dest];

        moves[*total] = (struct move){orig, dest};

        // Generate board
        board new_board = current;
        new_board.black[sub_layer[orig]] -= (uint64_t)1
                                            << (sub_layer_offset_direct[orig]);
        new_board.black[sub_layer[dest]] |= (uint64_t)1
                                            << (sub_layer_offset_direct[dest]);
        new_board.black_r[sub_layer[orig_r]] -=
            (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
        new_board.black_r[sub_layer[dest_r]] |=
            (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

        // if (capture_dests[sub_layer[dest]] &
	// (1 << sub_layer_offset_direct[dest])) {
          capture_functions[dest](new_board.black, new_board.black_r,
                                  new_board.white, new_board.white_r, dest);
	  // }
	  
	  shield_wall<true>(&new_board, dest);

        boards[*total] = new_board;
        (*total)++;

        row_moves &= row_moves - 1;
      }
      movers &= movers - 1;
    }
  }

  // center vertical
  {
    uint16_t movers = ((current.black_r[0] >> 55) | ((current.black_r[1] & 0x3) << 9)) &
             0b11111111111;
    while (movers) {
      uint8_t local_orig = _tzcnt_u16(movers);
      uint8_t orig_r = local_orig + 55;
      uint8_t orig = rotate_left[orig_r];
      const unsigned short blockers_v =
          ((occ_r[0] >> 55) | ((occ_r[1] & 0x3) << 9)) & 0b11111111111;
      uint16_t row_moves = center_row_moves_table[blockers_v][local_orig];
      while (row_moves) {
        uint8_t dest_r = _tzcnt_u16(row_moves) + 55;
        uint8_t dest = rotate_left[dest_r];
        // pawns can't land on the throne
        if (dest == 60) {
          continue;
        }
        moves[*total] = (struct move){orig, dest};

        // Generate board
        board new_board = current;
        new_board.black_r[sub_layer[orig_r]] ^= (uint64_t)1 << orig_r;
        new_board.black_r[sub_layer[dest_r]] |= (uint64_t)1 << dest_r;
        new_board.black[sub_layer[orig]] ^= (uint64_t)1
                                            << (orig - sub_layer_offset[orig]);
        new_board.black[sub_layer[dest]] |= (uint64_t)1
                                            << (dest - sub_layer_offset[dest]);

        // if (capture_dests[sub_layer[dest]] &
	// (1 << (dest - sub_layer_offset[dest]))) {
          capture_functions[dest](new_board.black, new_board.black_r,
                                  new_board.white, new_board.white_r, dest);
	  // }

	  shield_wall<true>(&new_board, dest);

        boards[*total] = new_board;
        (*total)++;

        row_moves &= row_moves - 1;
      }
      movers &= movers - 1;
    }
  }
}

inline __attribute__((always_inline)) void
get_team_moves_white(const board current, int *total, move *moves,
                     board *boards) {
  *total = 0;

  const layer occ = {
      current.white[0] | current.black[0] | current.king[0] | corners[0],
      current.white[1] | current.black[1] | current.king[1] | corners[1]};

  const layer capture_dests = find_capture_destinations_op(current.white, current.black);

  // lower 5 rows
  get_next_row_boards_white<0, 0, 0>(occ[0], current,  total, moves, boards, capture_dests[0]);
  get_next_row_boards_white<0, 0, 11>(occ[0], current, total, moves, boards, capture_dests[0]);
  get_next_row_boards_white<0, 0, 22>(occ[0], current, total, moves, boards, capture_dests[0]);
  get_next_row_boards_white<0, 0, 33>(occ[0], current, total, moves, boards, capture_dests[0]);
  get_next_row_boards_white<0, 0, 44>(occ[0], current, total, moves, boards, capture_dests[0]);

  // upper 5 rows
  get_next_row_boards_white<1, 64, 2>(occ[1], current,  total, moves, boards, capture_dests[1]);
  get_next_row_boards_white<1, 64, 13>(occ[1], current, total, moves, boards, capture_dests[1]);
  get_next_row_boards_white<1, 64, 24>(occ[1], current, total, moves, boards, capture_dests[1]);
  get_next_row_boards_white<1, 64, 35>(occ[1], current, total, moves, boards, capture_dests[1]);
  get_next_row_boards_white<1, 64, 46>(occ[1], current, total, moves, boards, capture_dests[1]);

  const layer occ_r = {
      current.white_r[0] | current.black_r[0] | current.king_r[0] | corners[0],
      current.white_r[1] | current.black_r[1] | current.king_r[1] | corners[1]};

  // lower 5 rows
  get_next_row_boards_white_r<0, 0, 0>(occ_r[0],  current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<0, 0, 11>(occ_r[0], current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<0, 0, 22>(occ_r[0], current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<0, 0, 33>(occ_r[0], current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<0, 0, 44>(occ_r[0], current, total, moves, boards, capture_dests);

  // // upper 5 rows
  get_next_row_boards_white_r<1, 64, 2>( occ_r[1],  current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<1, 64, 13>( occ_r[1], current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<1, 64, 24>( occ_r[1], current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<1, 64, 35>( occ_r[1], current, total, moves, boards, capture_dests);
  get_next_row_boards_white_r<1, 64, 46>( occ_r[1], current, total, moves, boards, capture_dests);

  // center horizontal
  {
    uint16_t movers = (current.white[0] >> 55) |
                      (((current.white[1] & 0x3) << 9) & 0b11111111111);
    while (movers) {
      uint8_t local_orig = _tzcnt_u16(movers);
      uint8_t orig = local_orig + 55;
      uint8_t orig_r = rotate_right[orig];
      const unsigned short blockers_h =
          (occ[0] >> 55) | (((occ[1] & 0x3) << 9) & 0b11111111111);
      uint16_t row_moves =
          center_row_moves_table[blockers_h][_tzcnt_u16(movers)];
      while (row_moves) {
        uint8_t dest = _tzcnt_u16(row_moves) + 55;
        // pawns can't land on the throne
        if (dest == 60) {
          continue;
        }
        uint8_t dest_r = rotate_right[dest];

        moves[*total] = (struct move){orig, dest};

        // Generate board
        board new_board = current;
        new_board.white[sub_layer[orig]] -= (uint64_t)1
                                            << (sub_layer_offset_direct[orig]);
        new_board.white[sub_layer[dest]] |= (uint64_t)1
                                            << (sub_layer_offset_direct[dest]);
        new_board.white_r[sub_layer[orig_r]] -=
            (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
        new_board.white_r[sub_layer[dest_r]] |=
            (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

        // if (capture_dests[sub_layer[dest]] &
	// (1 << sub_layer_offset_direct[dest])) {
          capture_functions[dest](new_board.white, new_board.white_r,
                                  new_board.black, new_board.black_r, dest);
	  // }

	shield_wall<false>(&new_board, dest);

        boards[*total] = new_board;
        (*total)++;

        row_moves &= row_moves - 1;
      }
      movers &= movers - 1;
    }
  }

  // center vertical
  {
    uint16_t movers = ((current.white_r[0] >> 55) | ((current.white_r[1] & 0x3) << 9)) &
             0b11111111111;
    while (movers) {
      uint8_t local_orig = _tzcnt_u16(movers);
      uint8_t orig_r = local_orig + 55;
      uint8_t orig = rotate_left[orig_r];
      const unsigned short blockers_v =
          ((occ_r[0] >> 55) | ((occ_r[1] & 0x3) << 9)) & 0b11111111111;
      uint16_t row_moves = center_row_moves_table[blockers_v][local_orig];
      while (row_moves) {
        uint8_t dest_r = _tzcnt_u16(row_moves) + 55;
        uint8_t dest = rotate_left[dest_r];
        // pawns can't land on the throne
        if (dest == 60) {
          continue;
        }
        moves[*total] = (struct move){orig, dest};

        // Generate board
        board new_board = current;
        new_board.white_r[sub_layer[orig_r]] ^= (uint64_t)1 << orig_r;
        new_board.white_r[sub_layer[dest_r]] |= (uint64_t)1 << dest_r;
        new_board.white[sub_layer[orig]] ^= (uint64_t)1
                                            << (orig - sub_layer_offset[orig]);
        new_board.white[sub_layer[dest]] |= (uint64_t)1
                                            << (dest - sub_layer_offset[dest]);

        // if (capture_dests[sub_layer[dest]] &
	// (1 << (dest - sub_layer_offset[dest]))) {
          capture_functions[dest](new_board.white, new_board.white_r,
                                  new_board.black, new_board.black_r, dest);
	  // }

	shield_wall<false>(&new_board, dest);

        boards[*total] = new_board;
        (*total)++;

        row_moves &= row_moves - 1;
      }
      movers &= movers - 1;
    }
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


/* For quiescence:

   find pieces in danger:
   for ex black north:
   - & black, white, king, and negate to get a mask of open squares
   - shift black left 11, & with white, shift left 11, & with open squares to
   get a map of squares that, if moved to, will cause captures.
   - for each destination square:
     - re-use the lookup table for move destinations where the occ is only the
     opposing team (here white) to get open lines to the destination which go
     either to the closest foe piece or to the board edge.
     - If the resulting move masks contain any friendly pieces, it means those
     can move into the square and trigger a capture.
     - we can extract the appropriate pieces to become moves to generate from
     the result.
   - I can actually generate the destination squares for all directions and then combine them to only check arrival once.

   - do checks to see if the king can move into corner adjacent squares
 */


void init_move_globals() {
  gen_foe_masks();
  gen_ally_masks();
  gen_row_moves();
  gen_center_row_moves();
  gen_row_move_counts();
  gen_center_row_move_counts();
}
