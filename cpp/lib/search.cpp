#include "board.cpp"
#include "layer.cpp"
#include "move.cpp"
#include <cassert>
#include <climits>
#include <cstdint>
#include <iostream>

enum PieceType : uint8_t {
  black_type = 1,
  white_type = 2,
  king_type = 3,
};

int32_t BLACK_PAWN_VALUE = 10000;
int32_t WHITE_PAWN_VALUE = 10000;

struct score_state {
  int32_t get_score(bool is_black_turn) const {
    int32_t score_as_black =
        guard_score + (BLACK_PAWN_VALUE * black_pawn_count) -
        (WHITE_PAWN_VALUE * white_pawn_count) - pst_white_score;
    return is_black_turn ? score_as_black : -score_as_black;
  };
  uint8_t nw_guard_count;
  uint8_t ne_guard_count;
  uint8_t sw_guard_count;
  uint8_t se_guard_count;
  int32_t guard_score;
  uint8_t black_pawn_count;
  uint8_t white_pawn_count;
  int32_t pst_black_score;
  int32_t pst_white_score;
  int32_t pst_king_score;
};

constexpr layer corner_guard_nw = read_layer(
    ".  .  X  .  .  .  .  .  .  .  ."
    ".  X  .  .  .  .  .  .  .  .  ."
    "X  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer corner_guard_ne = read_layer(
    ".  .  .  .  .  .  .  .  X  .  ."
    ".  .  .  .  .  .  .  .  .  X  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer corner_guard_sw = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    "X  .  .  .  .  .  .  .  .  .  ."
    ".  X  .  .  .  .  .  .  .  .  ."
    ".  .  X  .  .  .  .  .  .  .  .",
    'X');

constexpr layer corner_guard_se = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  X  ."
    ".  .  .  .  .  .  .  .  X  .  .",
    'X');

uint8_t nw_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_nw[1] & b.black[1]);
}

uint8_t ne_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_ne[1] & b.black[1]);
}

uint8_t sw_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_sw[0] & b.black[0]);
}

uint8_t se_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_se[0] & b.black[0]);
}

// -----------------------------------------------------------------------------
// King corner access

#define mask_rightward(x) (((uint16_t)1 << x) - 1)
#define mask_leftward(x) ((0x7fe << x) & 0x7fe)
#define drop_edge(x) (x & 0b1111111110)

/**
 * Check if the king can reach the se corner in 1 move.
 *
 * because this is used on white's turn we can skip actually
 * generating moves, because it's enough to know that the king can
 * reach the corner. Instead it should be used as a static heuristic.
 */
bool corner_moves_1(
    const layer occ, const layer occ_r, const int rank, const int file) {

  uint16_t row;

  if (rank == 0) {
    row = dirty_get_row_0(occ);
    if (!(row & mask_leftward(file)) || !(row & mask_rightward(file))) {
      return true;
    }
  } else if (rank == 1) {
    row = dirty_get_row_1(occ);
    if (!(row & mask_leftward(file)) || !(row & mask_rightward(file))) {
      return true;
    }
  } else if (rank == 9) {
    row = dirty_get_row_9(occ);
    if (!(row & mask_leftward(file)) || !(row & mask_rightward(file))) {
      return true;
    }
  } else if (rank == 10) {
    row = dirty_get_row_10(occ);
    if (!(row & mask_leftward(file)) || !(row & mask_rightward(file))) {
      return true;
    }
  }

  if (file == 0) {
    row = dirty_get_row_0(occ_r);
    if (!(row & mask_leftward(rank)) || !(row & mask_rightward(rank))) {
      return true;
    }
  } else if (file == 1) {
    row = dirty_get_row_1(occ_r);
    if (!(row & mask_leftward(rank)) || !(row & mask_rightward(rank))) {
      return true;
    }
  } else if (file == 9) {
    row = dirty_get_row_9(occ_r);
    if (!(row & mask_leftward(rank)) || !(row & mask_rightward(rank))) {
      return true;
    }
  } else if (file == 10) {
    row = dirty_get_row_10(occ_r);
    if (!(row & mask_leftward(rank)) || !(row & mask_rightward(rank))) {
      return true;
    }
  }

  return false;
}

constexpr layer full_file_0 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer inner_file_mask = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    "X  X  X  X  X  X  X  X  X  X  X"
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_0 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_1 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_2 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_3 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_4 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_5 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_6 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_7 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_8 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_9 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_above_10 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_below_0 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
    'X');

constexpr layer file_below_1 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_2 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_3 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_4 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_5 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_6 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_7 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_8 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_9 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_below_10 = read_layer(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  X",
    'X');

constexpr layer file_mask_0 = rotate_layer({(uint64_t)0x7ff, 0}) >> 10;
constexpr layer file_mask_1 = file_mask_0 << 1;
constexpr layer file_mask_2 = file_mask_0 << 2;
constexpr layer file_mask_3 = file_mask_0 << 3;
constexpr layer file_mask_4 = file_mask_0 << 4;
constexpr layer file_mask_5 = file_mask_0 << 5;
constexpr layer file_mask_6 = file_mask_0 << 6;
constexpr layer file_mask_7 = file_mask_0 << 7;
constexpr layer file_mask_8 = file_mask_0 << 8;
constexpr layer file_mask_9 = file_mask_0 << 9;
constexpr layer file_mask_10 = file_mask_0 << 10;

constexpr layer below_10 = layer{(uint64_t)~0, (uint64_t)~0 >> 7} >> 11;
constexpr layer below_9 = below_10 >> 11;
constexpr layer below_8 = below_9 >> 11;
constexpr layer below_7 = below_8 >> 11;
constexpr layer below_6 = below_7 >> 11;
constexpr layer below_5 = below_6 >> 11;
constexpr layer below_4 = below_5 >> 11;
constexpr layer below_3 = below_4 >> 11;
constexpr layer below_2 = below_3 >> 11;
constexpr layer below_1 = below_2 >> 11;
constexpr layer below_0 = below_1 >> 11;

layer below_n[] = {
    below_0,
    below_1,
    below_2,
    below_3,
    below_4,
    below_5,
    below_6,
    below_7,
    below_8,
    below_9,
    below_10};

constexpr layer above_0 = ~layer{0, 0} << 11;
constexpr layer above_1 = above_0 << 11;
constexpr layer above_2 = above_1 << 11;
constexpr layer above_3 = above_2 << 11;
constexpr layer above_4 = above_3 << 11;
constexpr layer above_5 = above_4 << 11;
constexpr layer above_6 = above_5 << 11;
constexpr layer above_7 = above_6 << 11;
constexpr layer above_8 = above_7 << 11;
constexpr layer above_9 = above_8 << 11;
constexpr layer above_10 = above_9 << 11;

layer above_n[] = {
    above_0,
    above_1,
    above_2,
    above_3,
    above_4,
    above_5,
    above_6,
    above_7,
    above_8,
    above_9,
    above_10};

/**
 * Get layer masks for all open 1-move routes to a corner.
 */
void corner_paths_1(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    int *count,
    layer *paths,
    layer *paths_r) {

  const uint16_t legal_edge_mask = 0b01111111110;
  // TODO: use the above to exclude corners from paths

  switch (rank) {
  case 0: {
    uint16_t row = dirty_get_row_0(occ);
    const uint16_t mask_left = mask_leftward(file);
    if (!(row & mask_left)) {
      paths[*count] = {(uint64_t)mask_left & legal_edge_mask, 0};
      paths_r[*count] = not_corners & file_mask_10 & above_n[file];
      (*count)++;
    }
    uint16_t mask_right = mask_rightward(file);
    if (!(row & mask_right)) {
      paths[*count] = {(uint64_t)mask_right & legal_edge_mask, 0};
      paths_r[*count] = not_corners & file_mask_10 & below_n[file];
      (*count)++;
    }
  }; break;
  case 1: {
    uint16_t row = dirty_get_row_1(occ);
    uint16_t mask_left = mask_leftward(file);
    if (!(row & mask_left)) {
      paths[*count] = {(uint64_t)mask_left << 11, 0};
      paths_r[*count] = file_mask_9 & above_n[file];
      (*count)++;
    }
    uint16_t mask_right = mask_rightward(file);
    if (!(row & mask_right)) {
      paths[*count] = {(uint64_t)mask_right << 11, 0};
      paths_r[*count] = file_mask_9 & below_n[file];
      (*count)++;
    }
  }; break;
  case 9: {
    uint16_t row = dirty_get_row_9(occ);
    uint16_t rank_mask_right = mask_rightward(file);
    if (!(row & rank_mask_right)) {
      paths[*count] = {0, (uint64_t)rank_mask_right << 35};
      paths_r[*count] = file_mask_1 & below_n[file];
      (*count)++;
    }
    uint16_t rank_mask_left = mask_leftward(file);
    if (!(row & rank_mask_left)) {
      paths[*count] = {0, (uint64_t)rank_mask_left << 35};
      paths_r[*count] = file_mask_1 & above_n[file];
      (*count)++;
    }
  }; break;
  case 10: {
    uint16_t row = dirty_get_row_10(occ);
    uint16_t rank_mask_right = mask_rightward(file);
    if (!(row & rank_mask_right)) {
      paths[*count] = {0, (uint64_t)(rank_mask_right & legal_edge_mask) << 46};
      paths_r[*count] = not_corners & file_mask_0 & below_n[file];
      (*count)++;
    }
    uint16_t rank_mask_left = mask_leftward(file);
    if (!(row & rank_mask_left)) {
      paths[*count] = {0, (uint64_t)(rank_mask_left & legal_edge_mask) << 46};
      paths_r[*count] = not_corners & file_mask_0 & above_n[file];
      (*count)++;
    }
  }; break;
  }

  switch (file) {
  case 0: {
    uint16_t row = dirty_get_row_0(occ_r);
    uint16_t mask_left = mask_leftward(rank);
    if (!(row & mask_left)) {
      paths[*count] = not_corners & file_mask_0 & below_n[rank];
      paths_r[*count] = {(uint64_t)(mask_left & legal_edge_mask), 0};
      (*count)++;
    }
    uint16_t mask_right = mask_rightward(rank);
    if (!(row & mask_right)) {
      paths[*count] = not_corners & file_mask_0 & above_n[rank];
      paths_r[*count] = {(uint64_t)(mask_right & legal_edge_mask), 0};
      (*count)++;
    }
  }; break;
  case 1: {
    uint16_t row = dirty_get_row_1(occ_r);
    uint16_t mask_left = mask_leftward(rank);
    if (!(row & mask_left)) {
      paths[*count] = not_corners & file_mask_1 & below_n[rank];
      paths_r[*count] = {(uint64_t)mask_left << 11, 0};
      (*count)++;
    }
    uint16_t mask_right = mask_rightward(rank);
    if (!(row & mask_right)) {
      paths[*count] = not_corners & file_mask_1 & above_n[rank];
      paths_r[*count] = {(uint64_t)mask_right << 11, 0};
      (*count)++;
    }
  }; break;
  case 9: {
    uint16_t row = dirty_get_row_9(occ_r);
    uint16_t rank_mask_right = mask_rightward(rank);
    if (!(row & rank_mask_right)) {
      paths[*count] = file_mask_9 & above_n[rank];
      paths_r[*count] = {0, (uint64_t)rank_mask_right << 35};
      (*count)++;
    }
    uint16_t rank_mask_left = mask_leftward(rank);
    if (!(row & rank_mask_left)) {
      paths[*count] = file_mask_9 & below_n[rank];
      paths_r[*count] = {0, (uint64_t)rank_mask_left << 35};
      (*count)++;
    }
  }; break;
  case 10: {
    uint16_t row = dirty_get_row_10(occ_r);
    uint16_t rank_mask_right = mask_rightward(rank);
    if (!(row & rank_mask_right)) {
      paths[*count] = not_corners & file_mask_10 & above_n[rank];
      paths_r[*count] = {0, (uint64_t)rank_mask_right << 46 & not_corners[1]};
      (*count)++;
    }
    uint16_t rank_mask_left = mask_leftward(rank);
    if (!(row & rank_mask_left)) {
      paths[*count] = not_corners & file_mask_10 & below_n[rank];
      paths_r[*count] = {0, (uint64_t)rank_mask_left << 46 & not_corners[1]};
      (*count)++;
    }
  }; break;
  }
}

/* Get the destination of each unique move towards a corner.
 *
 * This can be used to generate next boards for king-corner quiescence.
 */
void corner_moves_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    int *count,
    int *moves) {

  const int rank_r = 10 - rank;
  uint16_t first_lane;
  uint16_t second_lane;

  const uint16_t south_edge = dirty_get_row_0(occ);
  const uint16_t south_inner = dirty_get_row_1(occ);
  const uint16_t east_edge = dirty_get_row_0(occ_r);
  const uint16_t east_inner = dirty_get_row_1(occ_r);
  const uint16_t west_edge = dirty_get_row_10(occ_r);
  const uint16_t west_inner = dirty_get_row_9(occ_r);
  const uint16_t rank_row = 0;
  const uint16_t rank_mask_left = mask_leftward(file);
  // const uint16_t rank_mask_left_short = 0;
  const uint16_t rank_mask_right = mask_rightward(file);
  // const uint16_t rank_mask_right_short = 0;
  const uint16_t file_row = dirty_get_row(occ_r, file);
  const uint16_t file_mask_left = 0;
  const uint16_t file_mask_left_short = 0;
  const uint16_t file_mask_right = 0;
  const uint16_t file_mask_right_short = 0;

  //// north
  // edge
  first_lane = file_row & mask_rightward(rank_r);
  second_lane = dirty_get_row_10(occ);
  if (!(first_lane || ((second_lane & mask_leftward(file)) &&
                       (second_lane & mask_rightward(file))))) {
    moves[*count] = 110 + file;
    (*count)++;
  }

  // inner
  first_lane = drop_edge(first_lane);
  second_lane = dirty_get_row_9(occ);
  if (!(first_lane || ((second_lane & mask_leftward(file)) &&
                       (second_lane & mask_rightward(file))))) {
    moves[*count] = 99 + file;
    (*count)++;
  }
  // south
  // east
  // west
}

/* Get the path of each move towards a corner.
 *
 * This can be used to generate next boards for king-corner quiescence.
 */
void corner_paths_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    int *count,
    layer *paths) {
  uint16_t north_edge = dirty_get_row_10(occ);
  uint16_t north_inner = dirty_get_row_9(occ);
  uint16_t south_edge = dirty_get_row_0(occ);
  uint16_t south_inner = dirty_get_row_1(occ);
  uint16_t east_edge = dirty_get_row_0(occ_r);
  uint16_t east_inner = dirty_get_row_1(occ_r);
  uint16_t west_edge = dirty_get_row_10(occ_r);
  uint16_t west_inner = dirty_get_row_9(occ_r);
  uint16_t rank_row = 0;
  uint16_t rand_mask_left = 0;
  uint16_t rand_mask_left_short = 0;
  uint16_t rand_mask_right_short = 0;
  uint16_t file_row = 0;
}

bool corner_access_2_se(
    const layer occ, const layer occ_r, const int rank, const int file) {
  const uint16_t rank_mask = mask_rightward(rank);
  const uint16_t file_mask = mask_leftward(file);

  // south->east to corner
  const uint16_t inner_file_blockers = get_row(occ_r, file) & file_mask;
  const uint16_t outer_rank_blockers = occ[0] & rank_mask;

  // east->south to corner
  const uint16_t inner_rank_blockers = get_row(occ, rank) & rank_mask;
  const uint16_t outer_file_blockers = occ_r[0] & file_mask;

  // south->east to corner north neighbor
  const uint16_t short_inner_file_blockers = drop_edge(inner_file_blockers);
  const uint16_t adjacent_outer_rank_blockers = (occ[0] >> 11) & rank_mask;

  // east->south to corner west neighbor
  const uint16_t short_inner_rank_blockers = drop_edge(inner_rank_blockers);
  const uint16_t adjacent_outer_file_blockers = (occ_r[0] >> 11) & file_mask;

  return !(inner_rank_blockers || outer_file_blockers) ||
         !(inner_file_blockers || outer_rank_blockers) ||
         !(short_inner_file_blockers || adjacent_outer_rank_blockers) ||
         !(short_inner_rank_blockers || adjacent_outer_file_blockers);
}

bool corner_access_2_sw(
    const layer occ, const layer occ_r, const int rank, const int file) {
  const uint16_t rank_mask = 0x7fe << rank;
  const uint16_t file_mask = 0x7fe << file;

  // south access
  const uint16_t inner_file_blockers = get_row(occ_r, file) & file_mask;
  const uint16_t outer_rank_blockers = occ[0] & rank_mask;

  // west access
  const uint16_t inner_rank_blockers = get_row(occ, rank) & rank_mask;
  const uint16_t outer_file_blockers = (occ_r[1] >> 46) & file_mask;

  // south adjacent access
  const uint16_t short_file_mask = file_mask & (file_mask >> 1);
  const uint16_t short_inner_file_blockers =
      get_row(occ_r, file) & short_file_mask;
  const uint16_t adjacent_outer_rank_blockers = (occ[0] >> 11) & rank_mask;

  // west adjacent access
  const uint16_t short_rank_mask = rank_mask & (rank_mask << 1);
  const uint16_t short_inner_rank_blockers =
      get_row(occ, rank) & short_rank_mask;
  const uint16_t adjacent_outer_file_blockers = (occ_r[1] >> 35) & file_mask;

  return !(inner_rank_blockers || outer_file_blockers) ||
         !(inner_file_blockers || outer_rank_blockers) ||
         !(short_inner_file_blockers || adjacent_outer_rank_blockers) ||
         !(short_inner_rank_blockers || adjacent_outer_file_blockers);
}

bool corner_access_2_ne(
    const layer occ, const layer occ_r, const int rank, const int file) {
  const uint16_t rank_mask = ((uint16_t)1 << rank) - 1;
  const uint16_t file_mask = ((uint16_t)1 << file) - 1;

  // north access
  const uint16_t inner_file_blockers = get_row(occ_r, file) & file_mask;
  const uint16_t outer_rank_blockers = (occ[1] >> 46) & rank_mask;

  // east access
  const uint16_t inner_rank_blockers = get_row(occ, rank) & rank_mask;
  const uint16_t outer_file_blockers = occ_r[0] & file_mask;

  // north adjacent access
  const uint16_t short_file_mask = file_mask & (file_mask << 1);
  const uint16_t short_inner_file_blockers =
      get_row(occ_r, file) & short_file_mask;
  const uint16_t adjacent_outer_rank_blockers = (occ[1] >> 35) & rank_mask;

  // east adjacent access
  const uint16_t short_rank_mask = rank_mask & (rank_mask << 1);
  const uint16_t short_inner_rank_blockers =
      get_row(occ, rank) & short_rank_mask;
  const uint16_t adjacent_outer_file_blockers = (occ_r[0] >> 11) & file_mask;

  return !(inner_rank_blockers || outer_file_blockers) ||
         !(inner_file_blockers || outer_rank_blockers) ||
         !(short_inner_file_blockers || adjacent_outer_rank_blockers) ||
         !(short_inner_rank_blockers || adjacent_outer_file_blockers);
}

bool corner_access_2_nw(
    const layer occ, const layer occ_r, const int rank, const int file) {
  const uint16_t rank_mask = 0x7fe << rank;
  const uint16_t file_mask = ((uint16_t)1 << file) - 1;

  // north access
  const uint16_t inner_file_blockers = get_row(occ_r, file) & file_mask;
  const uint16_t outer_rank_blockers = (occ[1] >> 46) & rank_mask;

  // west access
  const uint16_t inner_rank_blockers = get_row(occ, rank) & rank_mask;
  const uint16_t outer_file_blockers = (occ_r[1] >> 46) & file_mask;

  // north adjacent access
  const uint16_t short_file_mask = file_mask & (file_mask << 1);
  const uint16_t short_inner_file_blockers =
      get_row(occ_r, file) & short_file_mask;
  const uint16_t adjacent_outer_rank_blockers = (occ[1] >> 35) & rank_mask;

  // west adjacent access
  const uint16_t short_rank_mask = rank_mask & (rank_mask >> 1);
  const uint16_t short_inner_rank_blockers =
      get_row(occ, rank) & short_rank_mask;
  const uint16_t adjacent_outer_file_blockers = (occ_r[1] >> 35) & file_mask;

  return !(inner_rank_blockers || outer_file_blockers) ||
         !(inner_file_blockers || outer_rank_blockers) ||
         !(short_inner_file_blockers || adjacent_outer_rank_blockers) ||
         !(short_inner_rank_blockers || adjacent_outer_file_blockers);
}

int32_t
all_corner_access_2(layer occ, layer occ_r, const int rank, const int file) {
  return corner_access_2_nw(occ, occ_r, rank, file) +
         corner_access_2_ne(occ, occ_r, rank, file) +
         corner_access_2_sw(occ, occ_r, rank, file) +
         corner_access_2_se(occ, occ_r, rank, file);
}

bool corner_access_3_se(
    const layer occ, const layer occ_r, const int rank, const int file) {
  const uint16_t rank_mask = ((uint16_t)1 << rank) - 1;
  const uint16_t file_mask = 0x7fe << file;

  // south access

  // const int inner_rank

  return true;
}

// -----------------------------------------------------------------------------

std::array<int32_t, 121> quarter_to_pst(std::array<int32_t, 29> quarter) {
  static array<int, 29> indices = {1,  2,  3,  4,  5,  11, 12, 13, 14, 15,
                                   16, 22, 23, 24, 25, 26, 27, 33, 34, 35,
                                   36, 37, 38, 44, 45, 46, 47, 48, 49};

  array<int32_t, 121> pst = {0};

  for (int i = 0; i < 29; i++) {
    int32_t val = quarter[i];
    int index = indices[i];
    pst[index] = val;
    index = rotate_right[index];
    pst[index] = val;
    index = rotate_right[index];
    pst[index] = val;
    index = rotate_right[index];
    pst[index] = val;
  }

  return pst;
}

std::array<int32_t, 29> black_niave_pst_quarter = {
    0, // 1
    0, // 2
    0, // 3
    0, // 4
    0, // 5
    0, // 11
    0, // 12
    0, // 13
    0, // 14
    0, // 15
    0, // 16
    0, // 22
    0, // 23
    0, // 24
    0, // 25
    0, // 26
    0, // 27
    0, // 33
    0, // 34
    0, // 35
    0, // 36
    0, // 37
    0, // 38
    0, // 44
    0, // 45
    0, // 46
    0, // 47
    0, // 48
    0  // 49
};

std::array<int32_t, 29> white_niave_pst_quarter = {
    0, // 1
    0, // 2
    0, // 3
    0, // 4
    0, // 5
    0, // 11
    0, // 12
    0, // 13
    0, // 14
    0, // 15
    0, // 16
    0, // 22
    0, // 23
    0, // 24
    0, // 25
    0, // 26
    0, // 27
    0, // 33
    0, // 34
    0, // 35
    0, // 36
    0, // 37
    0, // 38
    0, // 44
    0, // 45
    0, // 46
    0, // 47
    0, // 48
    0  // 49
};

std::array<int32_t, 29> king_niave_pst_quarter = {
    0,   // 1
    500, // 2
    0,   // 3
    0,   // 4
    0,   // 5
    0,   // 11
    500, // 12
    500, // 13
    0,   // 14
    0,   // 15
    0,   // 16
    500, // 22
    500, // 23
    500, // 24
    0,   // 25
    0,   // 26
    0,   // 27
    0,   // 33
    0,   // 34
    0,   // 35
    0,   // 36
    0,   // 37
    -5,  // 38
    0,   // 44
    0,   // 45
    0,   // 46
    0,   // 47
    -10, // 48
    -15  // 49
};

struct ai_settings {
  std::array<int32_t, 121> black_pst;
  std::array<int32_t, 121> white_pst;
  std::array<int32_t, 121> king_pst;
  int32_t king_throne_position_score;
};

ai_settings init_ai_settings() {
  return {
      quarter_to_pst(black_niave_pst_quarter),
      quarter_to_pst(white_niave_pst_quarter),
      quarter_to_pst(king_niave_pst_quarter),
      -100};
}

score_state init_score_state(const board b) {
  return {
      nw_corner_protection(b),
      ne_corner_protection(b),
      sw_corner_protection(b),
      se_corner_protection(b),
      0,
      static_cast<uint8_t>(black_pawn_count(b)),
      static_cast<uint8_t>(white_pawn_count(b)),
      0,
      0,
      0};
}

int32_t guard_count_bonuses[] = {0, 300, 600, 1000};

void update_guard_score_state_move(score_state &s, move m) {

  switch (m.orig) {
  // nw
  case 118:
  case 108:
  case 98:
    s.guard_score -= guard_count_bonuses[s.nw_guard_count];
    s.nw_guard_count--;
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s.guard_score -= guard_count_bonuses[s.ne_guard_count];
    s.ne_guard_count--;
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s.guard_score -= guard_count_bonuses[s.sw_guard_count];
    s.sw_guard_count--;
    break;
  // se
  case 22:
  case 12:
  case 2:
    s.guard_score -= guard_count_bonuses[s.se_guard_count];
    s.se_guard_count -= 1;
    break;
  }
  switch (m.dest) {
  // nw
  case 118:
  case 108:
  case 98:
    s.nw_guard_count++;
    /*
    if (s.nw_guard_count > 3) {
      printf("guard count nw: %d\n", s.nw_guard_count);
      std::cout << "that's illegal: " << m << "\n";
    }
    */
    s.guard_score += guard_count_bonuses[s.nw_guard_count];
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s.ne_guard_count++;
    if (s.ne_guard_count > 3) {
      printf("guard coun ne: %d", s.ne_guard_count);
      std::cout << "that's illegal: " << m << "\n";
    }
    s.guard_score += guard_count_bonuses[s.ne_guard_count];
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s.sw_guard_count++;
    // if (s.sw_guard_count > 3) {
    //   printf("guard count sw: %d", s.sw_guard_count);
    //   std::cout << "that's illegal: " << m << "\n";
    // }
    s.guard_score += guard_count_bonuses[s.sw_guard_count];
    break;
  // se
  case 22:
  case 12:
  case 2:
    s.se_guard_count++;
    // if (s.se_guard_count > 3) {
    //   printf("guard count se: %d", s.se_guard_count);
    //   std::cout << "that's illegal: " << m << "\n";
    // }
    s.guard_score += guard_count_bonuses[s.se_guard_count];
    break;
  }
};

void update_guard_score_state_capture(score_state &s, int i) {
  //  printf("fire cap up\n");

  switch (i) {
  // nw
  case 118:
  case 108:
  case 98:
    s.guard_score -= guard_count_bonuses[s.nw_guard_count];
    s.nw_guard_count--;
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s.guard_score -= guard_count_bonuses[s.ne_guard_count];
    s.ne_guard_count--;
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s.guard_score -= guard_count_bonuses[s.sw_guard_count];
    s.sw_guard_count--;
    break;
  // se
  case 22:
  case 12:
  case 2:
    s.guard_score -= guard_count_bonuses[s.se_guard_count];
    s.se_guard_count -= 1;
    break;
  }
};

score_state update_score_state(score_state old_s, move m, PieceType t) {
  score_state s = old_s;
  if (t == black_type) {
    update_guard_score_state_move(s, m);
  }
  return s;
}

int white_pawn_move_count(const board b) {
  return get_team_move_count(b.get_occ(), b.white, b.get_occ_r(), b.white_r);
}

int black_pawn_move_count(const board b) {
  return get_team_move_count(b.get_occ(), b.black, b.get_occ_r(), b.black_r);
}

bool king_escaped(const board b) {
  return b.king[0] & corners[0] || b.king[1] & corners[1];
}

bool king_captured(const board b) {
  uint8_t king_index =
      b.king[0] ? _tzcnt_u64(b.king[0]) : _tzcnt_u64(b.king[1]) + 64;
  layer attackers = surround_masks[king_index] & b.black;
  uint8_t attacker_count =
      __builtin_popcountll(attackers[0]) + __builtin_popcountll(attackers[1]);
  return attacker_count > 3;
}

uint32_t CORNER_PROTECTION_BONUS = 250;

int corner_protection(const board b) {
  return (nw_corner_protection(b) * CORNER_PROTECTION_BONUS) +
         (ne_corner_protection(b) * CORNER_PROTECTION_BONUS) +
         (sw_corner_protection(b) * CORNER_PROTECTION_BONUS) +
         (se_corner_protection(b) * CORNER_PROTECTION_BONUS);
}

// typedef int32_t score;
static const int32_t MIN_SCORE = -INT_MAX;
static const int32_t MAX_SCORE = INT_MAX;

constexpr layer corner_adjacent = read_layer(
    ".  X  .  .  .  .  .  .  .  X  ."
    "X  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    "X  .  .  .  .  .  .  .  .  .  X"
    ".  X  .  .  .  .  .  .  .  X  .",
    'X');

bool king_escape_ensured(const board b) {
  return b.king[0] & corner_adjacent[0] || b.king[1] & corner_adjacent[1];
}

int32_t score_board(const board board, const bool is_black_turn) {
  uint king_pos = board.king[0] ? _tzcnt_u64(board.king[0])
                                : _tzcnt_u64(board.king[1]) + 64;
  uint king_rank = king_pos / 11;
  uint king_file = king_pos % 11;

  int32_t white_score = //(white_pawn_count(*board) * 10000) +
      white_pawn_move_count(board) + (get_king_move_count(board) * 100) +
      (all_corner_access_2(
           board.get_occ(), board.get_occ_r(), king_rank, king_file) *
       1000) +
      (king_escape_ensured(board) * 9000000);

  int32_t black_score =             //(black_pawn_count(*board) * 10000) +
      black_pawn_move_count(board); // +
                                    // corner_protection(*board);

  return is_black_turn ? black_score - white_score : white_score - black_score;
}

bool game_over_check(const board &b, bool is_black_turn, int32_t &score) {
  if (king_escaped(b)) {
    score = is_black_turn ? MIN_SCORE : MAX_SCORE;
    return true;
  } else if (king_captured(b)) {
    score = is_black_turn ? MAX_SCORE : MIN_SCORE;
    return true;
  }
  return false;
}

int32_t score_board_for_order(const board *board, const bool is_black_turn) {
  int32_t white_score = white_pawn_count(*board) * 1000;

  int32_t black_score =
      (black_pawn_count(*board) * 1000) + corner_protection(*board) * 100;

  return is_black_turn ? black_score - white_score : white_score - black_score;
}

struct repetitions {
  bool over_limit() { return odd_reps > 2 || even_reps > 2; }
  move odd;
  uint8_t odd_reps;
  move even;
  uint8_t even_reps;
  bool is_odd;
};

struct team_repetitions {
  bool over_limit(bool is_black_turn) {
    if (is_black_turn) {
      return black.over_limit();
    } else {
      return white.over_limit();
    }
  }
  repetitions black;
  repetitions white;
};

repetitions init_repetitions() { return {{0, 0}, 0, {0, 0}, 0, true}; }

team_repetitions init_team_repetitions() {
  return {init_repetitions(), init_repetitions()};
}

repetitions update_repetitions(repetitions r, move m) {
  repetitions output = r;
  if (output.is_odd) {
    if (m != output.odd) {
      output.odd = m;
      output.odd_reps = 0;
    } else {
      output.odd_reps++;
    }
  } else {
    if (m != output.even) {
      output.even = m;
      output.even_reps = 0;
    } else {
      output.even_reps++;
    }
  }
  output.is_odd = !output.is_odd;
  return output;
}

team_repetitions
update_team_repetitions(team_repetitions r, move m, bool is_black_turn) {
  team_repetitions new_r = r;
  if (is_black_turn) {
    new_r.black = update_repetitions(new_r.black, m);
  } else {
    new_r.white = update_repetitions(new_r.white, m);
  }
  return new_r;
}

typedef struct negamax_ab_result {
  move _move;
  board _board;
  int32_t _score;
  score_state ss;
} negamax_ab_result;

// move moves_table[11][240];
// board boards_table[11][240];
// board scores_table[11][240];
const int MAX_DEPTH = 32;
int PV_LENGTH[MAX_DEPTH];
move PV_TABLE[MAX_DEPTH][MAX_DEPTH];
board PV_TABLE_BOARDS[MAX_DEPTH][MAX_DEPTH];
move PREV_PV[MAX_DEPTH];
int PREV_PV_LENGTH;
move KILLER_MOVES[MAX_DEPTH][2];

int32_t negamax_ab_sorted_pv(
    const move m,
    const board b,
    const team_repetitions r,
    const bool is_black_turn,
    const int depth,
    const int ply,
    int32_t alpha,
    const int32_t beta,
    int *tally,
    const bool is_pv,
    const score_state ss,
    const bool allow_null_move,
    const struct ai_settings &ai_settings) {
  PV_LENGTH[ply] = ply;

  /*
  if (ply > 31) {
    printf("max depth exceeded\n");
    exit(1);
  }
  */

  int32_t game_over_score = 0;
  bool game_over = game_over_check(b, is_black_turn, game_over_score);
  if (game_over) {
    return game_over_score;
  }

  int total = 0;
  move moves_table[324];
  board boards_table[324];
  uint8_t cap_counts[324] = {0};

  if (depth > 3 && ply > 0 && allow_null_move && !is_pv) {
    // Null move heuristic
    // TODO: figure out the most suitable depth cutoff (if any)
    // TODO: add check to see if the static evaluation of the board is above
    // beta
    // TODO: ensure king is not in check/on verge of escape, otherwise this
    // will be unsound.
    int null_shortening = 2;
    int32_t null_result = -negamax_ab_sorted_pv(
        m,
        b,
        r,
        !is_black_turn,
        depth - 1 - null_shortening,
        ply + 1 + null_shortening,
        -beta,
        -beta + 1,
        tally,
        false,
        ss,
        false,
        ai_settings);
    if (null_result >= beta) {
      return beta;
    }
  }
  /*
   */

  if (depth <= 0) {
    if (ply < MAX_DEPTH) {
      if (is_black_turn) {
        get_capture_move_boards<true>(
            boards_table, b, &total, moves_table, cap_counts);
      } else {
        get_capture_move_boards<false>(
            boards_table, b, &total, moves_table, cap_counts);
      }
    }
    if (total == 0) {
      if (!b.black[0] && !b.black[1]) {
        printf("ply: %d\n", ply);
        print_board(b);
      }
      return score_board(b, is_black_turn) + ss.get_score(is_black_turn);
    }
  } else {

    if (is_black_turn) {
      get_team_moves<true>(b, &total, moves_table, cap_counts, boards_table);
    } else {
      get_king_moves(b, &total, moves_table, cap_counts, boards_table);
      get_team_moves<false>(b, &total, moves_table, cap_counts, boards_table);
    }
  }

  negamax_ab_result combi[324];
  for (int i = 0; i < total; i++) {
    combi[i] = (negamax_ab_result){moves_table[i], boards_table[i]};

    if (combi[i]._move.orig == 44 && combi[i]._move.dest == 22 &&
        ss.se_guard_count == 3) {
      print_board(combi[i]._board);
    }

    // update score state
    combi[i].ss = update_score_state(
        ss, combi[i]._move, is_black_turn ? black_type : white_type);

    // update score state piece counts
    if (is_black_turn) {
      combi[i].ss.white_pawn_count -= cap_counts[i];
    } else {
      combi[i].ss.black_pawn_count -= cap_counts[i];
    }

    // TODO: move this somewhere closer to the core capture code se we don't
    // have to re-calculate the indexes of captures
    if (cap_counts[i]) {
      if (is_black_turn) {
        layer cap_layer = b.white ^ combi[i]._board.white;
        while (cap_layer[0]) {
          int cap_index = _tzcnt_u64(cap_layer[0]);
          combi[i].ss.pst_white_score -= ai_settings.white_pst[cap_index];
          cap_layer[0] &= cap_layer[0] - 1;
        }
        while (cap_layer[1]) {
          int cap_index = _tzcnt_u64(cap_layer[1]) + 64;
          combi[i].ss.pst_white_score -= ai_settings.white_pst[cap_index];
          cap_layer[1] &= cap_layer[1] - 1;
        }
      } else {
        layer cap_layer = b.black ^ combi[i]._board.black;
        while (cap_layer[0]) {
          int cap_index = _tzcnt_u64(cap_layer[0]);
          update_guard_score_state_capture(combi[i].ss, cap_index);
          combi[i].ss.pst_black_score -= ai_settings.black_pst[cap_index];
          cap_layer[0] &= cap_layer[0] - 1;
        }
        while (cap_layer[1]) {
          int cap_index = _tzcnt_u64(cap_layer[1]) + 64;
          update_guard_score_state_capture(combi[i].ss, cap_index);
          combi[i].ss.pst_black_score -= ai_settings.black_pst[cap_index];
          cap_layer[1] &= cap_layer[1] - 1;
        }
      }
    }

    uint8_t king_pos =
        b.king[0] ? _tzcnt_u64(b.king[0]) : _tzcnt_u64(b.king[1]) + 64;

    // update pst score
    if (is_black_turn) {
      combi[i].ss.pst_black_score -= ai_settings.black_pst[combi[i]._move.orig];
      combi[i].ss.pst_black_score += ai_settings.black_pst[combi[i]._move.dest];
    } else if (combi[i]._move.orig == !king_pos) {
      combi[i].ss.pst_white_score -= ai_settings.white_pst[combi[i]._move.orig];
      combi[i].ss.pst_white_score += ai_settings.white_pst[combi[i]._move.dest];
    } else {
      combi[i].ss.pst_king_score -= ai_settings.king_pst[combi[i]._move.orig];
      combi[i].ss.pst_king_score += ai_settings.king_pst[combi[i]._move.dest];
    }

    combi[i]._score = depth > 1 ? combi[i].ss.get_score(is_black_turn) : 0;
    // Add bonus if killer move
    if (combi[i]._move == KILLER_MOVES[ply][0] ||
        combi[i]._move == KILLER_MOVES[ply][1])
      combi[i]._score += 10000000;
    // Add bonus if we're following the
    if (is_pv && (combi[i]._move == PREV_PV[ply])) {
      combi[i]._score += 100000000;
    }
  }

  // niave sort
  /*
  if (depth > 1) {
    std::sort(combi, combi + total, [](const auto &lhs, const auto &rhs) {
      return lhs._score > rhs._score;
    });
  }
  */

  // start with a bogus best
  int32_t best = MIN_SCORE;
  negamax_ab_result tmp;
  for (int i = 0; i < total; i++) {
    if (depth > 1) {
      int best_index = i;
      for (int j = i + 1; j < total; j++) {
        if (combi[j]._score > combi[best_index]._score) {
          best_index = j;
        }
      }
      if (best_index != i) {
        tmp = combi[i];
        combi[i] = combi[best_index];
        combi[best_index] = tmp;
      }
    }

    // TODO: move this to leaf node
    (*tally)++;

    // update and check repetitions
    team_repetitions new_r =
        update_team_repetitions(r, combi[i]._move, is_black_turn);
    if (new_r.over_limit(is_black_turn)) {
      continue;
    }

    // Late Move Reduction: if we have more than two ply left, and
    // we've already examined the best 25 moves based on the move
    // ordering heuristics, then search this move 2 ply more shallow
    // with a narrow window, and skip if we don't raise alpha (which
    // indicates that this move, at least superficially, conforms to
    // our expectation in being poor)
    if (depth > 2 && i > 25) {
      int32_t lmr_eval = -negamax_ab_sorted_pv(
          combi[i]._move,
          combi[i]._board,
          new_r,
          !is_black_turn,
          depth == 0 ? 0 : depth - 2,
          ply + 2,
          -alpha - 1,
          -alpha,
          tally,
          (is_pv && !i),
          combi[i].ss,
          true,
          ai_settings);
      if (lmr_eval <= alpha) {
        continue;
      }
    }

    // calcualte result and negate score the is_pv parameter is (is_pv
    // && !i) because if this _is_ the pv then the first entry must be
    // the next PV node due to the bonus applied above, unless the.
    // next PV move wasn't found, which would be a real bug.

    int32_t eval = -negamax_ab_sorted_pv(
        combi[i]._move,
        combi[i]._board,
        new_r,
        !is_black_turn,
        depth == 0 ? 0 : depth - 1,
        ply + 1,
        -beta,
        -alpha,
        tally,
        (is_pv && !i),
        combi[i].ss,
        true,
        ai_settings);

    /*
    int32_t eval;
    if (i == 0) {
      int32_t eval = -negamax_ab_sorted_pv(
          combi[i]._move, combi[i]._board, !is_black_turn,
          depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
          (is_pv && !i), combi[i].ss, true);
    } else {
      // search with null window
      int32_t eval = -negamax_ab_sorted_pv(
          combi[i]._move, combi[i]._board, !is_black_turn,
          depth == 0 ? 0 : depth - 1, ply + 1, -alpha-1, -alpha, tally,
          (is_pv && !i), combi[i].ss, true);
      if (alpha < eval && eval < beta) {
        // if failed high, research with full window
        int32_t eval = -negamax_ab_sorted_pv(
            combi[i]._move, combi[i]._board, !is_black_turn,
            depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
            (is_pv && !i), combi[i].ss, true);
      }
    }
    */

    // if (depth == 1 && (m.orig == 115 && m.dest == 114)) printf("score: %d\n",
    // next_result);
    // if (depth == 1) printf("score: %d\n", eval);
    // if (depth == 1) print_board(combi[i]._board);

    if (eval > best) {
      best = eval;
    }
    if (best > alpha) {
      // ALPHA BETA HANDLING
      // ---------------------------------------------------------------
      alpha = best;

      // PV HANDLING
      // --------------------------------------------------------------- assign
      // move discovered here
      PV_TABLE[ply][ply] = combi[i]._move;
      PV_TABLE_BOARDS[ply][ply] = combi[i]._board; // me only
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
        /*
        if (next_ply > 31) {
          printf("next ply over limit\n");
          exit(1);
        }
        */
        PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
        PV_TABLE_BOARDS[ply][next_ply] =
            PV_TABLE_BOARDS[ply + 1][next_ply]; // me only
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];

      // KILLER HANDLING
      // ---------------------------------------------------------------
      // TODO: exclude captures
      // NOTE: this should typically be handled at a beta cutoff, but
      // benchmarking shows better performance if killers are set
      // every time alpha is raised
      KILLER_MOVES[ply][1] = KILLER_MOVES[ply][0];
      KILLER_MOVES[ply][0] = combi[i]._move;
    }
    if (alpha > beta) {
      break;
    }
  }
  // if (depth == 1 && (m.orig == 115 && m.dest == 114))
  // printf("----------------------");
  // if (depth == 1) printf("----------------------");

  return best;
}

enum Flag : uint8_t {
  lower_bound = 1,
  exact = 2,
  upper_bound = 3,
};
struct search_result {
  move m;
  board b;
  int32_t s;
  team_repetitions r;
};

int32_t quiesce(
    const move m,
    const board b,
    const team_repetitions r,
    const bool is_black_turn,
    // const int depth,
    const int ply,
    int32_t alpha,
    const int32_t beta,
    int *tally,
    // const bool is_pv,
    const score_state ss,
    // const bool allow_null_move,
    const struct ai_settings &ai_settings) {
  // base implementation from:
  // https://www.chessprogramming.org/Quiescence_Search

  // TODO: implement delta pruning

  // assert we don't exceed a generous ply limit to guard against infinite loops
  assert(ply < 20);

  // int stand_pat = Evaluate();
  // if( stand_pat >= beta )
  //     return beta;
  // if( alpha < stand_pat )
  //     alpha = stand_pat;
  int stand_pat = score_board(b, is_black_turn) + ss.get_score(is_black_turn);
  if (stand_pat >= beta)
    return beta;
  if (alpha < stand_pat)
    alpha = stand_pat;

  // Gen moves
  // 1. a) if black, moves that block escape paths
  //    b) if white, escape moves
  // 2. capture moves

  // until( every_capture_has_been_examined )  {
  //     MakeCapture();
  //     score = -Quiesce( -beta, -alpha );
  //     TakeBackMove();

  //     if( score >= beta )
  //         return beta;
  //     if( score > alpha )
  //        alpha = score;
  // }
  // return alpha;
}

search_result negamax_ab_sorted_pv_runner(
    board b,
    team_repetitions r,
    bool is_black,
    int depth,
    struct ai_settings ai_settings) {
  int tally = 0;
  score_state s = init_score_state(b);
  /*
  printf("nw: %d\n", s.nw_guard_count);
  printf("ne: %d\n", s.ne_guard_count);
  printf("sw: %d\n", s.sw_guard_count);
  printf("se: %d\n", s.se_guard_count);
  */

  memset(KILLER_MOVES, 0, MAX_DEPTH * sizeof(move) * 2);
  memset(PREV_PV, 0, MAX_DEPTH * sizeof(move));
  for (int i = 0; i < depth; i++) {
    negamax_ab_sorted_pv(
        (move){0, 0},
        b,
        r,
        is_black,
        i,
        0,
        INT_MIN,
        INT_MAX,
        &tally,
        true,
        s,
        false,
        ai_settings);
    for (int j = 0; j < PV_LENGTH[0]; j++) {
      PREV_PV[j] = PV_TABLE[0][j];
    }
    PREV_PV_LENGTH = PV_LENGTH[0];
  }
  auto res = negamax_ab_sorted_pv(
      (move){0, 0},
      b,
      r,
      is_black,
      depth,
      0,
      MIN_SCORE,
      MAX_SCORE,
      &tally,
      true,
      s,
      false,
      ai_settings);

  move result_move = PV_TABLE[0][0];
  board result_board = PV_TABLE_BOARDS[0][0];
  team_repetitions new_r = update_team_repetitions(r, result_move, is_black);
  // printf("tally: %d\n", tally);
  return {result_move, result_board, res, new_r};
}
