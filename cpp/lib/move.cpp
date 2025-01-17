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


/**
 * find positions that can be moved to from a position given the
 * occupancy of a row.
 * @param occ occupancy of the row, including the bit at `pos`.
 * @param pos the index of the starting position of the moves. 
 * @return a uint16_t where the set bits represent positions that can
 * be moved to.
 */
uint16_t get_row_moves_b(uint16_t occ, const uint8_t pos) {
  // TODO: need to test this
    static const uint16_t lowers[11]  = {
        0,
        0b1,
        0b11,
        0b111,
        0b1111,
        0b11111,
        0b111111,
        0b1111111,
        0b11111111,
        0b111111111,
        0b1111111111,
    };
    occ |= 0b1111100000000000;
    uint16_t upper_mask = 0b11111111110 << pos;
    uint16_t lower_mask = lowers[pos];
    uint16_t lower = lower_mask & occ;
    uint16_t upper = upper_mask & occ;
    uint16_t lower_neighbor = 32768 >> __lzcnt16(lower);
    return (upper - lower_neighbor) & ~occ;
}


void print_row(uint16_t row) {
  char output[18];
  memset(output, '0', 17);
  output[17] = '\0';
  output[5] = '|';
  int index;
  while (row) {
    index = _tzcnt_u16(row);
    if (index > 10) index++;
    output[16 - index] = '1';
    row &= row - 1;
  }
  puts(output);
  printf("\n");
}

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

layer find_capture_destinations_op(const layer allies, const layer foes, const layer occ) {
  layer north = (((allies << 11) & foes) << 11);
  layer south = (((allies >> 11) & foes) >> 11);
  layer east = ((((allies & drop_2_east) >> 1) & foes) >> 1);
  layer west = ((((allies & drop_2_west) << 1) & foes) << 1);
  return {(north[0] | south[0] | east[0] | west[0]) & (~occ[0]),
          (north[1] | south[1] | east[1] | west[1]) & (~occ[1])};
}


//******************************************************************************
// Quiescence
//******************************************************************************

/**
 * Requires that the bit at pos is 0.
 */
uint16_t find_neighbors(const uint16_t occ, const int pos) {
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

#define name_layer(name, is_black, is_rotated)                                 \
  (is_black ? (is_rotated ? name.black_r : name.black)                         \
            : (is_rotated ? name.white_r : name.white))

#define board_layer(is_black, is_rotated)                                      \
  name_layer(board, is_black, is_rotated)

#define name_allies(name, is_black, is_rotated)                                \
  (is_black                                                                    \
       ? (is_rotated ? name.black_r : name.black)                              \
       : (is_rotated ? name.white_r | name.king_r : name.white | name_king))

#define board_allies(is_black, is_rotated)                                      \
  name_allies(board, is_black, is_rotated)

#define new_board_layer(is_black, is_rotated)                                  \
  name_layer(new_board, is_black, is_rotated)

#define offset_coords(pred, amount)                                            \
  (pred ? (orig += amount, dest += amount, noop) : noop)

#define select_rotation(is_rotated) (is_rotated ? rotate_left : rotate_right)

#define select_index(is_lower, is_upper, var) (is_lower ? 0 : (is_upper ? 1 : sub_layer[var]))

/** process a capture move
 * 
 * The only difference between this and `process_move` is that we know there will be a capture, so we can skip the check.
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
void process_capture_move(const board *base_board, board *boards, move *moves, uint8_t *cap_counts,
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

  // capture_functions[(is_rotated ? dest_r : dest)](board_layer(is_black, false),
  // 						  board_layer(is_black, true),
  // 						  board_layer(!is_black, false),
  // 						  board_layer(!is_black, true),
  // 						  (is_rotated ? dest_r : dest));

  cap_counts[*total] = apply_captures_niave_count(
		       board_layer(is_black, false) | corners, board_layer(!is_black, false),
		       board_layer(!is_black, true), (is_rotated ? dest_r : dest));

  shield_wall<is_black>(&board, (is_rotated ? dest_r : dest));

  moves[*total] =
      (struct move){(is_rotated ? orig_r : orig), (is_rotated ? dest_r : dest)};
  boards[*total] = board;
  (*total)++;
}

template <bool is_black, bool is_rotated, bool is_lower, bool is_upper, bool always_capture>
void process_move(const board *base_board, board *boards, move *moves, uint8_t *cap_counts,
                  int *total, uint8_t orig, uint8_t dest, const layer cap_dests) {

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

  if constexpr (always_capture) {
    cap_counts[*total] = apply_captures_niave_count(
        board_layer(is_black, false) | corners,
        board_layer(!is_black, false),
        board_layer(!is_black, true),
        (is_rotated ? dest_r : dest));

    shield_wall<is_black>(&board, (is_rotated ? dest_r : dest));
  } else {
    bool is_capture = cap_dests[sub_layer[dest]] &
                      ((uint64_t)1 << sub_layer_offset_direct[dest]);
    if (is_capture) {
      cap_counts[*total] = apply_captures_niave_count(
          board_layer(is_black, false) | corners,
          board_layer(!is_black, false),
          board_layer(!is_black, true),
          (is_rotated ? dest_r : dest));

      shield_wall<is_black>(&board, (is_rotated ? dest_r : dest));
    }
  }

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
                   move *moves, uint8_t *cap_counts, layer occ, layer capt_dests) {
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
								     &board, boards, moves, cap_counts, total, orig, dest);
      origs = _blsr_u64(origs);
    }
    masked_capt_dests = _blsr_u64(masked_capt_dests);
  }
}

/**
 * generate captures moves for the center rank or file.
 */
template <bool is_black, bool is_rotated>
void capture_moves_center(board *boards, const board board, int *total, move *moves, uint8_t *cap_counts, const layer occ, layer capture_dests) {
  uint16_t center_capt_dests = get_center_row(capture_dests);
  const uint16_t center_occ = get_center_row(occ);
  while (center_capt_dests) {
    uint8_t dest = _tzcnt_u16(center_capt_dests);
    uint16_t neighbors = find_neighbors(center_occ, dest);
    uint16_t row_origs = neighbors & get_center_row(board_layer(is_black, is_rotated));
    while (row_origs) {
      uint8_t orig = _tzcnt_u16(row_origs);
      process_capture_move<is_black, is_rotated, false, false>(&board, boards, moves, cap_counts, total, orig, dest);
      row_origs &= row_origs - 1;
    }
    center_capt_dests &= center_capt_dests - 1;
  }
}

/**
 * generate captures moves for one half of the board, excluding center
 * rank or file.
 */
template <
    bool is_black,
    bool is_rotated,
    bool is_lower,
    bool is_upper,
    bool always_capture>
void destination_moves_half(
    board *boards,
    const board board,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    layer occ,
    layer capt_dests,
    layer destinations) {
  uint64_t masked_dests = destinations[(is_lower ? 0 : 1)] &
                               (is_lower ? lower_rows_mask : upper_rows_mask);
  while (masked_dests) {
    uint8_t dest = _tzcnt_u64(masked_dests);
    int row_offset =
        (is_lower ? sub_layer_row_offset : sub_layer_row_offset_upper)[dest];
    int row_dest = dest - row_offset;
    uint16_t neighbors = find_neighbors(
        0b11111111111 & (occ[(is_lower ? 0 : 1)] >> row_offset), row_dest);
    uint64_t origs = ((uint64_t)neighbors << row_offset) &
                     board_layer(is_black, is_rotated)[(is_lower ? 0 : 1)];
    while (origs) {
      uint8_t orig = _tzcnt_u64(origs);
      process_move<is_black, is_rotated, is_lower, is_upper, always_capture>(
          &board, boards, moves, cap_counts, total, orig, dest, capt_dests);
      origs = _blsr_u64(origs);
    }
    masked_dests = _blsr_u64(masked_dests);
  }
}

/**
 * generate captures moves for the center rank or file.
 */
template <bool is_black, bool is_rotated, bool always_capture>
void destination_moves_center(board *boards, const board board, int *total, move *moves, uint8_t *cap_counts, const layer occ, layer capture_dests, layer destinations) {
  uint16_t center_capt_dests = get_center_row(destinations);
  const uint16_t center_occ = get_center_row(occ);
  while (center_capt_dests) {
    uint8_t dest = _tzcnt_u16(center_capt_dests);
    uint16_t neighbors = find_neighbors(center_occ, dest);
    uint16_t row_origs = neighbors & get_center_row(board_layer(is_black, is_rotated));
    while (row_origs) {
      uint8_t orig = _tzcnt_u16(row_origs);
      process_move<is_black, is_rotated, false, false, always_capture>(&board, boards, moves, cap_counts, total, orig, dest, capture_dests);
      row_origs &= row_origs - 1;
    }
    center_capt_dests &= center_capt_dests - 1;
  }
}

template <bool is_black, bool always_capture>
void get_destination_move_boards(board *boards, const board board, int *total, move *moves, uint8_t *cap_counts, layer destinations) {
  // TODO: make this also generate shield wall capture moves. Might be best to generate a mask of destinations _next_ to opponent pieces, run shield_wall, and add the move if the board has changed.
  layer occ = board.black | board.white | board.king | corners;
  layer occ_r = board.black_r | board.white_r | board.king_r | corners;

  layer allies = board_layer(is_black, false) | corners;
  layer foes = board_layer(!is_black, false);
  if constexpr (!is_black) {
    allies[0] |= board.king[0];
    allies[1] |= board.king[1];
  }
  layer capt_dests = find_capture_destinations_op(allies | corners, foes, occ);
  layer capt_dests_r = rotate_layer(capt_dests); // TODO: maybe faster to do above rather than rotate
  layer destinations_r = rotate_layer(destinations);

  destination_moves_half<is_black, false, true, false, always_capture>(boards, board, total, moves, cap_counts, occ, capt_dests, destinations);
  destination_moves_half<is_black, false, false, true, always_capture>(boards, board, total, moves, cap_counts, occ, capt_dests, destinations);
  destination_moves_center<is_black, false, always_capture>(boards, board, total, moves, cap_counts, occ, capt_dests, destinations);
  destination_moves_half<is_black, true, true, false, always_capture>(boards, board, total, moves, cap_counts, occ_r, capt_dests_r, destinations_r);
  destination_moves_half<is_black, true, false, true, always_capture>(boards, board, total, moves, cap_counts, occ_r, capt_dests_r, destinations_r);
  destination_moves_center<is_black, true, always_capture>(boards, board, total, moves, cap_counts, occ_r, capt_dests_r, destinations_r);
}

/**
 * generate moves which result in captures.
 */
template <bool is_black>
void get_capture_move_boards(board *boards, const board board, int *total, move *moves, uint8_t *cap_counts) {
  // TODO: make this also generate shield wall capture moves. Might be best to generate a mask of destinations _next_ to opponent pieces, run shield_wall, and add the move if the board has changed.
  layer occ = board.black | board.white | board.king | corners;
  layer occ_r = board.black_r | board.white_r | board.king_r | corners;

  layer allies = board_layer(is_black, false) | corners;
  layer foes = board_layer(!is_black, false);
  if constexpr (!is_black) {
    allies[0] |= board.king[0];
    allies[1] |= board.king[1];
  }
  layer capt_dests = find_capture_destinations_op(allies | corners, foes, occ);
  layer capt_dests_r = rotate_layer(capt_dests); // TODO: maybe faster to do above rather than rotate



  capture_moves<is_black, false, true, false>(boards, board, total, moves, cap_counts, occ, capt_dests);
  capture_moves<is_black, false, false, true>(boards, board, total, moves, cap_counts, occ, capt_dests);
  capture_moves_center<is_black, false>(boards, board, total, moves, cap_counts, occ, capt_dests);
  capture_moves<is_black, true, true, false>(boards, board, total, moves, cap_counts, occ_r, capt_dests_r);
  capture_moves<is_black, true, false, true>(boards, board, total, moves, cap_counts, occ_r, capt_dests_r);
  capture_moves_center<is_black, true>(boards, board, total, moves, cap_counts, occ_r, capt_dests_r);

}

//******************************************************************************
// Moves
//******************************************************************************

template <bool is_black, bool is_rotated, bool is_lower, bool is_upper>
inline __attribute__((always_inline)) void
process_move(const board *base_board, board *boards, move *moves, uint8_t *cap_counts,
                  int *total, uint8_t orig, uint8_t dest, const layer cap_dests) {

  board board = *base_board;

  bool is_capture;

  // if neither upper nor lower then this is a center row, and we should offset
  // by 55 from the start
  // offset_coords(!is_lower && !is_upper, 55);
  if constexpr (!is_lower && !is_upper) {
    orig += 55;
    dest += 55;
    board_layer(is_black, is_rotated)[sub_layer[orig]] -=
        (uint64_t)1 << sub_layer_offset_direct[orig];
    board_layer(is_black, is_rotated)[sub_layer[dest]] |=
        (uint64_t)1 << sub_layer_offset_direct[dest];

    is_capture = cap_dests[sub_layer[dest]] &
                      ((uint64_t)1 << sub_layer_offset_direct[dest]);

  } else {
    board_layer(is_black, is_rotated)[is_upper] -= (uint64_t)1 << orig;
    board_layer(is_black, is_rotated)[is_upper] |= (uint64_t)1 << dest;

    // use the pre-offset value to check the capture destinations
    is_capture = cap_dests[is_upper] & ((uint64_t)1 << dest);
  }

  // if upper we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  if constexpr (is_upper) {
    orig += 64, dest += 64, noop;
  }

  uint8_t orig_r = select_rotation(is_rotated)[orig];
  uint8_t dest_r = select_rotation(is_rotated)[dest];

  board_layer(is_black, !is_rotated)[sub_layer[orig_r]] -=
      (uint64_t)1 << (sub_layer_offset_direct[orig_r]);
  board_layer(is_black, !is_rotated)[sub_layer[dest_r]] |=
      (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

  // this can maybe actually be moved up to where the check is done? actually no the check relies on the un-adjusted dest and the capture relies on the adjusted dest
  if (is_capture) {
    layer friends = board_layer(is_black, false) | corners;
    if constexpr (!is_black) {
      friends = friends | board.king;
    } 
    cap_counts[*total] = apply_captures_niave_count(
        friends, board_layer(!is_black, false),
        board_layer(!is_black, true), (is_rotated ? dest_r : dest));
  }

  shield_wall<is_black>(&board, (is_rotated ? dest_r : dest));

  moves[*total] =
      (struct move){(is_rotated ? orig_r : orig), (is_rotated ? dest_r : dest)};
  boards[*total] = board;
  (*total)++;
}

struct move_result {
  move m;
  board b;
  uint8_t cap_count;
  uint64_t z;
};

template <bool is_black, bool is_rotated, int index, int row_offset>
inline __attribute__((always_inline)) void
get_next_row_boards(board *boards, const uint64_t occ, const board &board,
                    int *total, move *moves, uint8_t *cap_counts, const layer cap_dests) {
  unsigned short movers =
    (board_layer(is_black, is_rotated)[index] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move<is_black, is_rotated, !index, index>(&board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

template <bool is_black, bool is_rotated>
inline __attribute__((always_inline)) void
get_next_row_boards_center(board *boards, const layer occ, const board &board,
                           int *total, move *moves, uint8_t *cap_counts, const layer cap_dests) {
  unsigned short movers = get_center_row(board_layer(is_black, is_rotated));
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers = get_center_row(occ);
    uint16_t row_moves = row_moves_table[blockers][orig];
    while (row_moves) {
      uint8_t dest = _tzcnt_u16(row_moves);
        if (dest == 5) {
	  row_moves &= row_moves - 1;
          continue;
        }
      process_move<is_black, is_rotated, false, false>(
						       &board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

template <bool is_black>
inline __attribute__((always_inline)) void
get_team_moves(const board current, int *total, move *moves, uint8_t *cap_counts,
                     board *boards) {
  const layer occ = {
      current.black[0] | current.white[0] | current.king[0] | corners[0],
      current.black[1] | current.white[1] | current.king[1] | corners[1]};

  layer allies = name_layer(current, is_black, false) | corners;
  if (!is_black) {
    allies = allies | current.king;
  }
  const layer capture_dests =
    find_capture_destinations_op(allies, name_layer(current, !is_black, false), occ);

  // print_layer(capture_dests);

  // // lower 5 rows
  get_next_row_boards<is_black, false, 0, 0>(boards, occ[0], current,  total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 0, 11>(boards, occ[0], current, total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 0, 22>(boards, occ[0], current, total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 0, 33>(boards, occ[0], current, total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 0, 44>(boards, occ[0], current, total, moves, cap_counts, capture_dests);

  get_next_row_boards_center<is_black, false>(boards, occ, current, total, moves, cap_counts, capture_dests);

  // // upper 5 rows
  get_next_row_boards<is_black, false, 1, 2>(boards, occ[1], current,  total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 1, 13>(boards, occ[1], current, total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 1, 24>(boards, occ[1], current, total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 1, 35>(boards, occ[1], current, total, moves, cap_counts, capture_dests);
  get_next_row_boards<is_black, false, 1, 46>(boards, occ[1], current, total, moves, cap_counts, capture_dests);

  const layer occ_r = {
      current.black_r[0] | current.white_r[0] | current.king_r[0] | corners[0],
      current.black_r[1] | current.white_r[1] | current.king_r[1] | corners[1]};

  layer allies_r = name_layer(current, is_black, true) | corners;
  if (!is_black) {
    allies_r = allies_r | current.king_r;
  }
  const layer capture_dests_r =
      find_capture_destinations_op(allies_r, name_layer(current, !is_black, true), occ_r);

  // lower 5 rows
  get_next_row_boards<is_black, true, 0, 0>( boards, occ_r[0], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 0, 11>(boards, occ_r[0], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 0, 22>(boards, occ_r[0], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 0, 33>(boards, occ_r[0], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 0, 44>(boards, occ_r[0], current, total, moves, cap_counts, capture_dests_r);

  get_next_row_boards_center<is_black, true>(boards, occ_r, current, total, moves, cap_counts, capture_dests_r);

  // upper 5 rows
  get_next_row_boards<is_black, true, 1, 2>( boards, occ_r[1], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 1, 13>(boards, occ_r[1], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 1, 24>(boards, occ_r[1], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 1, 35>(boards, occ_r[1], current, total, moves, cap_counts, capture_dests_r);
  get_next_row_boards<is_black, true, 1, 46>(boards, occ_r[1], current, total, moves, cap_counts, capture_dests_r);
}

inline __attribute__((always_inline)) void
get_king_moves(const board current, int *total, move *moves,
               uint8_t *cap_counts, board *boards) {
  const layer occ = {current.white[0] | current.black[0] | current.king[0],
                     current.white[1] | current.black[1] | current.king[1]};

  // const layer capture_dests = find_capture_destinations_op(current.white,
  // current.black);

  uint8_t orig = current.king[0] ? _tzcnt_u64(current.king[0])
                                 : _tzcnt_u64(current.king[1]) + 64;

  if (orig < 55) {
    const uint row_offset = sub_layer_row_offset[orig];
    const uint8_t row_orig = orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ[0] >> row_offset) & 0x7FF;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][row_orig]
                         << row_offset;
    while (row_moves) {
      const uint8_t dest = _tzcnt_u64(row_moves);
      const uint8_t dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king[0] = (uint64_t)1 << dest;
      new_board.king[1] = 0;
      new_board.king_r[!sub_layer[dest_r]] = 0;
      new_board.king_r[sub_layer[dest_r]] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);
      // handle captures
      // if (capture_dests[0] & (1 << dest)) {
      cap_counts[*total] = apply_captures_niave_count(
          new_board.white | corners, new_board.black, new_board.black_r, dest);
      // }
      shield_wall<false>(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else if (orig > 65) {
    const uint8_t sub_orig = orig - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const uint8_t row_orig = sub_orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ[1] >> row_offset) & 0x7FF;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][row_orig]
                         << row_offset;
    while (row_moves) {
      const uint8_t sub_dest = _tzcnt_u64(row_moves);
      const uint8_t dest = sub_dest + 64;
      const uint8_t dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king[0] = 0;
      new_board.king[1] = (uint64_t)1 << sub_dest;
      new_board.king_r[!sub_layer[dest_r]] = 0;
      new_board.king_r[sub_layer[dest_r]] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);
      cap_counts[*total] = apply_captures_niave_count(
          new_board.white | corners, new_board.black, new_board.black_r, dest);
      shield_wall<false>(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else {
    // center

    uint8_t local_orig = orig - 55;
    uint16_t blockers = get_center_row(occ);
    // we can use the normal one because we're the king
    uint16_t row_moves = row_moves_table[blockers][local_orig];
    while (row_moves) {
      const uint8_t dest = _tzcnt_u16(row_moves) + 55;
      const uint8_t dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king[!sub_layer[dest]] = 0;
      new_board.king[sub_layer[dest]] = (uint64_t)1
                                        << (sub_layer_offset_direct[dest]);
      new_board.king_r[!sub_layer[dest_r]] = 0;
      new_board.king_r[sub_layer[dest_r]] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);
      // handle captures
      // if (capture_dests[1] & (1 << sub_dest)) {
      cap_counts[*total] = apply_captures_niave_count(
          new_board.white | corners, new_board.black, new_board.black_r, dest);
      // }
      shield_wall<false>(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  }

  const layer occ_r = {
      current.white_r[0] | current.black_r[0] | current.king_r[0],
      current.white_r[1] | current.black_r[1] | current.king_r[1]};

  uint8_t orig_r = rotate_right[orig];

  if (orig_r < 55) {
    const uint row_offset = sub_layer_row_offset[orig_r];
    const uint8_t row_orig = orig_r - row_offset;
    const uint16_t blockers = ((uint64_t)occ_r[0] >> row_offset) & 0x7FF;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][row_orig]
                         << row_offset;
    while (row_moves) {
      const uint8_t dest_r = _tzcnt_u64(row_moves);
      const uint8_t dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king[sub_layer[dest]] = (uint64_t)1
                                        << (sub_layer_offset_direct[dest]);
      new_board.king[!sub_layer[dest]] = 0;
      new_board.king_r[0] = (uint64_t)1 << dest_r;
      new_board.king_r[1] = 0;
      // handle captures
      // if (capture_dests[0] & (1 << dest)) {
      cap_counts[*total] = apply_captures_niave_count(
          new_board.white | corners, new_board.black, new_board.black_r, dest);
      // }
      shield_wall<false>(&new_board, dest);
      // APPLY_CAPTURES_king;
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else if (orig_r > 65) {
    const uint8_t sub_orig = orig_r - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const uint8_t row_orig = sub_orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ_r[1] >> row_offset) & 0x7FF;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][row_orig]
                         << row_offset;
    while (row_moves) {
      const uint8_t sub_dest = _tzcnt_u64(row_moves);
      const uint8_t dest_r = sub_dest + 64;
      const uint8_t dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king[!sub_layer[dest]] = 0;
      new_board.king[sub_layer[dest]] = (uint64_t)1
                                        << (sub_layer_offset_direct[dest]);
      new_board.king_r[0] = 0;
      new_board.king_r[1] = (uint64_t)1 << sub_dest;
      cap_counts[*total] = apply_captures_niave_count(
          new_board.white | corners, new_board.black, new_board.black_r, dest);
      shield_wall<false>(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else {
    // center

    uint8_t local_orig = orig_r - 55;
    uint16_t blockers = get_center_row(occ_r);
    // we can use the normal one because we're the king
    uint16_t row_moves = row_moves_table[blockers][local_orig];
    while (row_moves) {
      const uint8_t dest_r = _tzcnt_u16(row_moves) + 55;
      const uint8_t dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king[!sub_layer[dest]] = 0;
      new_board.king[sub_layer[dest]] = (uint64_t)1
                                        << (sub_layer_offset_direct[dest]);
      new_board.king_r[!sub_layer[dest_r]] = 0;
      new_board.king_r[sub_layer[dest_r]] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

      cap_counts[*total] = apply_captures_niave_count(
          new_board.white | corners, new_board.black, new_board.black_r, dest);
      shield_wall<false>(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  }
}

inline __attribute__((always_inline)) int
get_king_move_count(const board current) {
  int total = 0;

  const layer occ = {
      current.white[0] | current.black[0] | current.king[0] | corners[0],
      current.white[1] | current.black[1] | current.king[1] | corners[1]};

  uint8_t orig = current.king[0] ? _tzcnt_u64(current.king[0])
                                 : _tzcnt_u64(current.king[1]) + 64;

  if (orig < 55) {
    const uint row_offset = sub_layer_row_offset[orig];
    const uint8_t row_orig = orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ[0] >> row_offset) & 0x7FF;
    total += row_move_count_table[blockers][row_orig];
  } else if (orig > 65) {
    const uint8_t sub_orig = orig - 64;
    /*
    if (sub_orig > 56) {
      printf("king orig: %d\n", orig);
      printf("king sub orig: %d\n", sub_orig);
      print_layer(current.white);
      print_layer(current.black);
      print_board(current);
    }
    */
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const uint8_t row_orig = sub_orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ[1] >> row_offset) & 0x7FF;
    total += row_move_count_table[blockers][row_orig];
  } else {
    // center
    uint8_t local_orig = orig - 55;
    uint16_t blockers = get_center_row(occ);
    total += row_move_count_table[blockers][local_orig];
  }
  const layer occ_r = {
      current.white_r[0] | current.black_r[0] | current.king_r[0] | corners[0],
      current.white_r[1] | current.black_r[1] | current.king_r[1] | corners[1]};

  uint8_t orig_r = rotate_right[orig];

  if (orig_r < 55) {
    const uint row_offset = sub_layer_row_offset[orig_r];
    const uint8_t row_orig = orig_r - row_offset;
    const uint16_t blockers = ((uint64_t)occ_r[0] >> row_offset) & 0x7FF;
    total += row_move_count_table[blockers][row_orig];
  } else if (orig_r > 65) {
    const uint8_t sub_orig = orig_r - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const uint8_t row_orig = sub_orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ_r[1] >> row_offset) & 0x7FF;
    total += row_move_count_table[blockers][row_orig];
  } else {
    // center
    const uint8_t local_orig = orig_r - 55;
    const uint16_t blockers = get_center_row(occ_r);
    const uint16_t row_moves = row_moves_table[blockers][local_orig];
    total += row_move_count_table[blockers][local_orig];
  }
  return total;
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

 */


void init_move_globals() {
  gen_foe_masks();
  gen_ally_masks();
  gen_surround_masks();
  gen_row_moves();
  gen_center_row_moves();
  gen_row_move_counts();
  gen_center_row_move_counts();
}

board make_move(const board b, const move m, const uint64_t z, const bool is_black_turn) {
  board result = b;
  if (is_black_turn) {
    toggle_index(result.black, m.orig);
    toggle_index(result.black, m.dest);
    toggle_index(result.black_r, rotate_right[m.orig]);
    toggle_index(result.black_r, rotate_right[m.dest]);
    apply_captures_niave_count(b.black | corners, result.white, result.white_r, m.dest);
    shield_wall<true>(&result, m.dest);
    layer capture_diff = b.white ^ result.white;
    while (capture_diff[0]) {
      int capture_index = _tzcnt_u64(capture_diff[0]);
      capture_diff[0] = _blsr_u64(capture_diff[0]);
    }
    while (capture_diff[1]) {
      int capture_index = _tzcnt_u64(capture_diff[1]) + 64;
      capture_diff[1] = _blsr_u64(capture_diff[1]);
    }
  } else if (check_index(b.white, m.orig)) {
    toggle_index(result.white, m.orig);
    toggle_index(result.white, m.dest);
    toggle_index(result.white_r, rotate_right[m.orig]);
    toggle_index(result.white_r, rotate_right[m.dest]);
    apply_captures_niave_count(b.white | b.king | corners, result.black, result.black_r, m.dest);
    shield_wall<false>(&result, m.dest);
    layer capture_diff = b.black ^ result.black;
    while (capture_diff[0]) {
      int capture_index = _tzcnt_u64(capture_diff[0]);
      capture_diff[0] = _blsr_u64(capture_diff[0]);
    }
    while (capture_diff[1]) {
      int capture_index = _tzcnt_u64(capture_diff[1]) + 64;
      capture_diff[1] = _blsr_u64(capture_diff[1]);
    }
  } else {
    toggle_index(result.king, m.orig);
    toggle_index(result.king, m.dest);
    toggle_index(result.king_r, rotate_right[m.orig]);
    toggle_index(result.king_r, rotate_right[m.dest]);
    apply_captures_niave_count(b.white | corners, result.black, result.black_r, m.dest);
    shield_wall<false>(&result, m.dest);
    layer capture_diff = b.black ^ result.black;
    while (capture_diff[0]) {
      int capture_index = _tzcnt_u64(capture_diff[0]);
      capture_diff[0] = _blsr_u64(capture_diff[0]);
    }
    while (capture_diff[1]) {
      int capture_index = _tzcnt_u64(capture_diff[1]) + 64;
      capture_diff[1] = _blsr_u64(capture_diff[1]);
    }
  }
  return result;
}
