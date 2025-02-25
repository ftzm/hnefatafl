#include "move.h"
#include "board.h"
#include "capture.h"
#include "layer.h"
#include "stdio.h"
#include "stdbool.h"
#include "x86intrin.h"

uint16_t row_moves_table[2048][11];
uint16_t center_row_moves_table[2048][11];

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
      0b11111111111};

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
      center_row_moves_table[row][pos] =
          get_row_moves(row, pos) & INVERTED_THRONE_MASK;
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
      row_move_count_table[row][pos] =
          __builtin_popcount(get_row_moves(row, pos));
    }
  }
}

void gen_center_row_move_counts() {
  uint16_t row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      center_row_move_count_table[row][pos] =
          __builtin_popcount(get_row_moves(row, pos) & INVERTED_THRONE_MASK);
    }
  }
}

/*[[[cog
import cog

combos = [
    (0, [0, 11, 22, 33, 44]),
    (1, [2, 13, 24, 35, 46,])
]

cog.outl("""
inline uint8_t get_team_move_count(
  const layer occ,
  const layer team,
  const layer occ_r,
  const layer team_r) {

  uint8_t total = 0;
  uint16_t movers;
  uint16_t blockers;""")

for rot in ("", "_r"):
    for index, offsets in combos:
        for offset in offsets:
            o = f"""
  movers = ((uint64_t) team{rot}._[{index}] >> {offset}) & 0b11111111111;
  blockers = ((uint64_t) occ{rot}._[{index}] >> {offset}) & 0b11111111111;
  while (movers) {{
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }}"""
            cog.outl(o)

cog.outl("""
  uint16_t row;
  uint16_t prog;

  // center horizontal
  row = ((occ._[0] >> 55) | ((occ._[1] & 0x3) << 9));
  prog = (team._[0] >> 55) | ((team._[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  // center vertical
  row = ((occ_r._[0] >> 55) | ((occ_r._[1] & 0x3) << 9));
  prog = (team_r._[0] >> 55) | ((team_r._[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }
  return total;
}""")
]]]*/

inline uint8_t get_team_move_count(
  const layer occ,
  const layer team,
  const layer occ_r,
  const layer team_r) {

  uint8_t total = 0;
  uint16_t movers;
  uint16_t blockers;

  movers = ((uint64_t) team._[0] >> 0) & 0b11111111111;
  blockers = ((uint64_t) occ._[0] >> 0) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[0] >> 11) & 0b11111111111;
  blockers = ((uint64_t) occ._[0] >> 11) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[0] >> 22) & 0b11111111111;
  blockers = ((uint64_t) occ._[0] >> 22) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[0] >> 33) & 0b11111111111;
  blockers = ((uint64_t) occ._[0] >> 33) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[0] >> 44) & 0b11111111111;
  blockers = ((uint64_t) occ._[0] >> 44) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[1] >> 2) & 0b11111111111;
  blockers = ((uint64_t) occ._[1] >> 2) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[1] >> 13) & 0b11111111111;
  blockers = ((uint64_t) occ._[1] >> 13) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[1] >> 24) & 0b11111111111;
  blockers = ((uint64_t) occ._[1] >> 24) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[1] >> 35) & 0b11111111111;
  blockers = ((uint64_t) occ._[1] >> 35) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team._[1] >> 46) & 0b11111111111;
  blockers = ((uint64_t) occ._[1] >> 46) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[0] >> 0) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[0] >> 0) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[0] >> 11) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[0] >> 11) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[0] >> 22) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[0] >> 22) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[0] >> 33) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[0] >> 33) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[0] >> 44) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[0] >> 44) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[1] >> 2) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[1] >> 2) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[1] >> 13) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[1] >> 13) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[1] >> 24) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[1] >> 24) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[1] >> 35) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[1] >> 35) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t) team_r._[1] >> 46) & 0b11111111111;
  blockers = ((uint64_t) occ_r._[1] >> 46) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  uint16_t row;
  uint16_t prog;

  // center horizontal
  row = ((occ._[0] >> 55) | ((occ._[1] & 0x3) << 9));
  prog = (team._[0] >> 55) | ((team._[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  // center vertical
  row = ((occ_r._[0] >> 55) | ((occ_r._[1] & 0x3) << 9));
  prog = (team_r._[0] >> 55) | ((team_r._[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }
  return total;
}
//[[[end]]]

void init_move_globals() {
  gen_foe_masks();
  gen_ally_masks();
  gen_surround_masks();
  gen_row_moves();
  gen_center_row_moves();
  gen_row_move_counts();
  gen_center_row_move_counts();
}

/*[[[cog
import cog
from itertools import product

levels = ["lower", "middle", "upper"]


def build_func(color, rotation, level):
    if level == "lower":
        index = 0
    elif level == "upper":
        index = 1
    else:
        index = None

    rotation_table = "rotate_left" if rotation == "_r" else "rotate_right"
    enemy = "white" if color == "black" else "black"

    op_rotation = "" if rotation else "_r"

    middle_handler = f"""
  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  op_layer_bit(b.{color}{rotation}, orig, -=);
  op_layer_bit(b.{color}{rotation}, dest, |=);

  is_capture = op_layer_bit(cap_dests, dest, &);"""

    main_half_handler = f"""
  b.{color}{rotation}._[{index}] -= (uint64_t)1 << orig;
  b.{color}{rotation}._[{index}] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[{index}] & ((uint64_t)1 << dest);"""


    upper_index_adjustment = """
  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;"""




    return f"""
void process_move_{color}{rotation}_{level}(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {{
  board b = base_board;
  bool is_capture;
  {middle_handler if level == "middle" else main_half_handler}
  {upper_index_adjustment if level == "upper" else ""}
  uint8_t orig_r = {rotation_table}[orig];
  uint8_t dest_r = {rotation_table}[dest];

  op_layer_bit(b.{color}{op_rotation}, orig_r, -=);
  op_layer_bit(b.{color}{op_rotation}, dest_r, |=);

  if (is_capture) {{
    layer friends = layer_or(b.{color}{rotation}, corners);
    {"friends = layer_or(friends, b.king);" if is_black else ""}

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.{enemy}{rotation},
        &b.{enemy}{"_r" if not rotation else ""},
        {"true" if rotation else "false"});
  }}

  shield_wall_{color}(&b, {"true" if rotation else "false"});

  moves[*total] = (struct move){{orig{rotation}, dest{rotation}}};
  boards[*total] = b;
  (*total)++;
}}"""

for (is_black, is_rotated, level) in product(["black", "white"], ["", "_r"],
levels): cog.outl(build_func(is_black, is_rotated, level))



cog.outl()

]]]*/

void process_move_black_lower(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.black._[0] -= (uint64_t)1 << orig;
  b.black._[0] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((uint64_t)1 << dest);
  
  uint8_t orig_r = rotate_right[orig];
  uint8_t dest_r = rotate_right[dest];

  op_layer_bit(b.black_r, orig_r, -=);
  op_layer_bit(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.white,
        &b.white_r,
        false);
  }

  shield_wall_black(&b, false);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_middle(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  op_layer_bit(b.black, orig, -=);
  op_layer_bit(b.black, dest, |=);

  is_capture = op_layer_bit(cap_dests, dest, &);
  
  uint8_t orig_r = rotate_right[orig];
  uint8_t dest_r = rotate_right[dest];

  op_layer_bit(b.black_r, orig_r, -=);
  op_layer_bit(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.white,
        &b.white_r,
        false);
  }

  shield_wall_black(&b, false);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_upper(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.black._[1] -= (uint64_t)1 << orig;
  b.black._[1] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((uint64_t)1 << dest);
  
  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  uint8_t orig_r = rotate_right[orig];
  uint8_t dest_r = rotate_right[dest];

  op_layer_bit(b.black_r, orig_r, -=);
  op_layer_bit(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.white,
        &b.white_r,
        false);
  }

  shield_wall_black(&b, false);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_lower(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.black_r._[0] -= (uint64_t)1 << orig;
  b.black_r._[0] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((uint64_t)1 << dest);
  
  uint8_t orig_r = rotate_left[orig];
  uint8_t dest_r = rotate_left[dest];

  op_layer_bit(b.black, orig_r, -=);
  op_layer_bit(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black_r, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.white_r,
        &b.white,
        true);
  }

  shield_wall_black(&b, true);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_middle(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  op_layer_bit(b.black_r, orig, -=);
  op_layer_bit(b.black_r, dest, |=);

  is_capture = op_layer_bit(cap_dests, dest, &);
  
  uint8_t orig_r = rotate_left[orig];
  uint8_t dest_r = rotate_left[dest];

  op_layer_bit(b.black, orig_r, -=);
  op_layer_bit(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black_r, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.white_r,
        &b.white,
        true);
  }

  shield_wall_black(&b, true);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_upper(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.black_r._[1] -= (uint64_t)1 << orig;
  b.black_r._[1] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((uint64_t)1 << dest);
  
  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  uint8_t orig_r = rotate_left[orig];
  uint8_t dest_r = rotate_left[dest];

  op_layer_bit(b.black, orig_r, -=);
  op_layer_bit(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black_r, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.white_r,
        &b.white,
        true);
  }

  shield_wall_black(&b, true);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_lower(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.white._[0] -= (uint64_t)1 << orig;
  b.white._[0] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((uint64_t)1 << dest);
  
  uint8_t orig_r = rotate_right[orig];
  uint8_t dest_r = rotate_right[dest];

  op_layer_bit(b.white_r, orig_r, -=);
  op_layer_bit(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.black,
        &b.black_r,
        false);
  }

  shield_wall_white(&b, false);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_middle(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  op_layer_bit(b.white, orig, -=);
  op_layer_bit(b.white, dest, |=);

  is_capture = op_layer_bit(cap_dests, dest, &);
  
  uint8_t orig_r = rotate_right[orig];
  uint8_t dest_r = rotate_right[dest];

  op_layer_bit(b.white_r, orig_r, -=);
  op_layer_bit(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.black,
        &b.black_r,
        false);
  }

  shield_wall_white(&b, false);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_upper(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.white._[1] -= (uint64_t)1 << orig;
  b.white._[1] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((uint64_t)1 << dest);
  
  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  uint8_t orig_r = rotate_right[orig];
  uint8_t dest_r = rotate_right[dest];

  op_layer_bit(b.white_r, orig_r, -=);
  op_layer_bit(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.black,
        &b.black_r,
        false);
  }

  shield_wall_white(&b, false);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_lower(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.white_r._[0] -= (uint64_t)1 << orig;
  b.white_r._[0] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((uint64_t)1 << dest);
  
  uint8_t orig_r = rotate_left[orig];
  uint8_t dest_r = rotate_left[dest];

  op_layer_bit(b.white, orig_r, -=);
  op_layer_bit(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white_r, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.black_r,
        &b.black,
        true);
  }

  shield_wall_white(&b, true);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_middle(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  op_layer_bit(b.white_r, orig, -=);
  op_layer_bit(b.white_r, dest, |=);

  is_capture = op_layer_bit(cap_dests, dest, &);
  
  uint8_t orig_r = rotate_left[orig];
  uint8_t dest_r = rotate_left[dest];

  op_layer_bit(b.white, orig_r, -=);
  op_layer_bit(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white_r, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.black_r,
        &b.black,
        true);
  }

  shield_wall_white(&b, true);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_upper(
    const board base_board,
    board *boards,
    move *moves,
    uint8_t *cap_counts,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests
) {
  board b = base_board;
  bool is_capture;
  
  b.white_r._[1] -= (uint64_t)1 << orig;
  b.white_r._[1] |= (uint64_t)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((uint64_t)1 << dest);
  
  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  uint8_t orig_r = rotate_left[orig];
  uint8_t dest_r = rotate_left[dest];

  op_layer_bit(b.white, orig_r, -=);
  op_layer_bit(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white_r, corners);
    friends = layer_or(friends, b.king);

    cap_counts[*total] = apply_captures_niave(
        friends,
        &b.black_r,
        &b.black,
        true);
  }

  shield_wall_white(&b, true);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

//[[[end]]]

/*[[[cog
import cog
from itertools import product

def build(color, rotation, index):
    process_move_name = "process_move_" + color + rotation + "_" + \
    ("upper" if index == "1" else "lower")
    return f"""
void get_next_row_boards_{color}{rotation}_{index}(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {{
  unsigned short movers =
   (board->{color}{rotation}._[{index}] >> row_offset) & 0b11111111111;
  while (movers) {{
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {{
          uint8_t dest = _tzcnt_u64(row_moves);
          {process_move_name}(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }}
    movers &= movers - 1;
  }}
}}
    """

for (color, rotation, index) in product(["black", "white"], ["", "_r"], ["0",
"1"]): cog.outl(build(color, rotation, index))

def build_c(color, rotation):
    return f"""
void get_next_row_boards_center_{color}{rotation}(
    board *boards,
    const layer occ,
    const board *board,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    const layer cap_dests
) {{
  unsigned short movers = get_center_row(board->{color}{rotation});
  while (movers) {{
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers = get_center_row(occ);
    uint16_t row_moves = row_moves_table[blockers][orig];
    while (row_moves) {{
      uint8_t dest = _tzcnt_u16(row_moves);
      if (dest == 5) {{
        row_moves &= row_moves - 1;
        continue;
      }}
      process_move_{color}{rotation}_middle(
        *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }}
    movers &= movers - 1;
  }}
}}
"""

for (color, rotation) in product(["black", "white"], ["", "_r"]):
    cog.outl(build_c(color, rotation))

]]]*/

void get_next_row_boards_black_0(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->black._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_black_lower(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_black_1(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->black._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_black_upper(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_black_r_0(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->black_r._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_black_r_lower(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_black_r_1(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->black_r._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_black_r_upper(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_white_0(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->white._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_white_lower(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_white_1(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->white._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_white_upper(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_white_r_0(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->white_r._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_white_r_lower(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_white_r_1(
  board *boards,
  const uint64_t occ,
  const board *board,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  const layer cap_dests,
  int row_offset
) {
  unsigned short movers =
   (board->white_r._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {
          uint8_t dest = _tzcnt_u64(row_moves);
          process_move_white_r_upper(
            *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
          row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}
    

void get_next_row_boards_center_black(
    board *boards,
    const layer occ,
    const board *board,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    const layer cap_dests
) {
  unsigned short movers = get_center_row(board->black);
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
      process_move_black_middle(
        *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}


void get_next_row_boards_center_black_r(
    board *boards,
    const layer occ,
    const board *board,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    const layer cap_dests
) {
  unsigned short movers = get_center_row(board->black_r);
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
      process_move_black_r_middle(
        *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}


void get_next_row_boards_center_white(
    board *boards,
    const layer occ,
    const board *board,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    const layer cap_dests
) {
  unsigned short movers = get_center_row(board->white);
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
      process_move_white_middle(
        *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}


void get_next_row_boards_center_white_r(
    board *boards,
    const layer occ,
    const board *board,
    int *total,
    move *moves,
    uint8_t *cap_counts,
    const layer cap_dests
) {
  unsigned short movers = get_center_row(board->white_r);
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
      process_move_white_r_middle(
        *board, boards, moves, cap_counts, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

//[[[end]]]

/*
void attempt_black(board) {
  int total = 0;
  move moves[240];

  static const uint64_t right_barriers = 36046397799139328;
  uint64_t occ;
  occ |= right_barriers;

  uint64_t leftward_movers;
  uint8_t pos = 0;

  while (leftward_movers) {
    // get to next position
    uint8_t to_next = _tzcnt_u64(leftward_movers) + 1;
    leftward_movers >>= to_next;
    occ >>= to_next;
    pos += to_next;

    // TODO 1: unroll the loops by writing 10 moves forward and then only moving
the total forward by the correct amount
    // TODO 2: have a simd vector of 8 integers of increasing values, and append
the position to each of those in parallel. uint8_t to_blocker = _tzcnt_u64(occ);
    while (to_blocker) {
      moves[total] = (move){pos, pos + to_blocker};
      to_blocker--;
    }
  }
}
*/

  /*
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
*/

const layer drop_2_east = {18338604880312133628ULL, 143903978713751539ULL};
const layer drop_2_west = {18419709275360197119ULL, 35975994678437884ULL};

layer find_capture_destinations(
    const layer allies, const layer foes, const layer occ) {
  layer north = layer_shiftl(layer_and(layer_shiftl(allies, 11), foes), 11);
  layer south = layer_shiftr(layer_and(layer_shiftr(allies, 11), foes), 11);
  layer east = layer_shiftr(layer_and(layer_shiftr(layer_and(allies, drop_2_east), 1), foes), 1);
  layer west = layer_shiftl(layer_and(layer_shiftl(layer_and(allies, drop_2_west), 1), foes), 1);
  return (layer){
      (north._[0] | south._[0] | east._[0] | west._[0]) & (~occ._[0]),
      (north._[1] | south._[1] | east._[1] | west._[1]) & (~occ._[1])};
}

/*[[[cog
import cog
from itertools import product

def build(color):
    out = f"""
void get_team_moves_{color}(
  const board current,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  board *boards
) {{
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = layer_or(current.{color}, corners);
  {"allies = layer_or(allies, current.king);" if color == "white" else ""}
  layer allies_r = layer_or(current.{color}_r, corners);
  layer foes = current.{"white" if color == "black" else "black"};
  layer foes_r = current.{"white" if color == "black" else "black"}_r;

  const layer capture_dests =
    find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
    find_capture_destinations(allies_r, foes_r, occ_r);
"""
    for r in ["", "_r"]:
        for (sub, offset) in [(0, 0), (0, 11), (0, 22), (0, 33), (0, 44),
                              (1, 2), (1, 13), (1, 24), (1, 35), (1, 46)]:
            out += f"""
  get_next_row_boards_{color}{r}_{sub}(
    boards,
    occ._[{sub}],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests{r},
    {offset}
  );
"""

    out += f"""
  get_next_row_boards_center_{color}(
    boards,
    occ,
    &current,
    total,
    moves,
    cap_counts,
    capture_dests
  );

  get_next_row_boards_center_{color}_r(
    boards,
    occ_r,
    &current,
    total,
    moves,
    cap_counts,
    capture_dests
  );"""

    out += "\n}"
    return out
 

for color in ["white", "black"]:
    cog.outl(build(color))

]]]*/

void get_team_moves_white(
  const board current,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  board *boards
) {
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = layer_or(current.white, corners);
  allies = layer_or(allies, current.king);
  layer allies_r = layer_or(current.white_r, corners);
  layer foes = current.black;
  layer foes_r = current.black_r;

  const layer capture_dests =
    find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
    find_capture_destinations(allies_r, foes_r, occ_r);

  get_next_row_boards_white_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    0
  );

  get_next_row_boards_white_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    11
  );

  get_next_row_boards_white_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    22
  );

  get_next_row_boards_white_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    33
  );

  get_next_row_boards_white_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    44
  );

  get_next_row_boards_white_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    2
  );

  get_next_row_boards_white_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    13
  );

  get_next_row_boards_white_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    24
  );

  get_next_row_boards_white_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    35
  );

  get_next_row_boards_white_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    46
  );

  get_next_row_boards_white_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    0
  );

  get_next_row_boards_white_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    11
  );

  get_next_row_boards_white_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    22
  );

  get_next_row_boards_white_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    33
  );

  get_next_row_boards_white_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    44
  );

  get_next_row_boards_white_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    2
  );

  get_next_row_boards_white_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    13
  );

  get_next_row_boards_white_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    24
  );

  get_next_row_boards_white_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    35
  );

  get_next_row_boards_white_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    46
  );

  get_next_row_boards_center_white(
    boards,
    occ,
    &current,
    total,
    moves,
    cap_counts,
    capture_dests
  );

  get_next_row_boards_center_white_r(
    boards,
    occ_r,
    &current,
    total,
    moves,
    cap_counts,
    capture_dests
  );
}

void get_team_moves_black(
  const board current,
  int *total,
  move *moves,
  uint8_t *cap_counts,
  board *boards
) {
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = layer_or(current.black, corners);
  
  layer allies_r = layer_or(current.black_r, corners);
  layer foes = current.white;
  layer foes_r = current.white_r;

  const layer capture_dests =
    find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
    find_capture_destinations(allies_r, foes_r, occ_r);

  get_next_row_boards_black_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    0
  );

  get_next_row_boards_black_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    11
  );

  get_next_row_boards_black_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    22
  );

  get_next_row_boards_black_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    33
  );

  get_next_row_boards_black_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    44
  );

  get_next_row_boards_black_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    2
  );

  get_next_row_boards_black_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    13
  );

  get_next_row_boards_black_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    24
  );

  get_next_row_boards_black_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    35
  );

  get_next_row_boards_black_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests,
    46
  );

  get_next_row_boards_black_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    0
  );

  get_next_row_boards_black_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    11
  );

  get_next_row_boards_black_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    22
  );

  get_next_row_boards_black_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    33
  );

  get_next_row_boards_black_r_0(
    boards,
    occ._[0],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    44
  );

  get_next_row_boards_black_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    2
  );

  get_next_row_boards_black_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    13
  );

  get_next_row_boards_black_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    24
  );

  get_next_row_boards_black_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    35
  );

  get_next_row_boards_black_r_1(
    boards,
    occ._[1],
    &current,
    total,
    moves,
    cap_counts,
    capture_dests_r,
    46
  );

  get_next_row_boards_center_black(
    boards,
    occ,
    &current,
    total,
    moves,
    cap_counts,
    capture_dests
  );

  get_next_row_boards_center_black_r(
    boards,
    occ_r,
    &current,
    total,
    moves,
    cap_counts,
    capture_dests
  );
}
//[[[end]]]
