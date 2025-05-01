#include "move.h"
#include "board.h"
#include "capture.h"
#include "io.h"
#include "layer.h"
#include "stdbool.h"
#include "stdio.h"
#include "string.h"
#include "x86intrin.h"

int cmp_moves(const move *a, const move *b) {
  int orig_cmp = a->orig - b->orig;
  if (orig_cmp == 0) {
    return a->dest - b->dest;
  } else {
    return orig_cmp;
  }
}

uint16_t row_moves_table[2048][11];
uint16_t center_row_moves_table[2048][11];

/**
 * find positions that can be moved to from a position given the
 * occupancy of a row.
 * @param occ occupancy of the row, including the bit at `pos`.
 * @param pos the index of the starting position of the moves.
 * @return a uint16_t wo.
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
    const layer occ, const layer team, const layer occ_r, const layer team_r) {

  uint8_t total = 0;
  uint16_t movers;
  uint16_t blockers;

  movers = ((uint64_t)team._[0] >> 0) & 0b11111111111;
  blockers = ((uint64_t)occ._[0] >> 0) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[0] >> 11) & 0b11111111111;
  blockers = ((uint64_t)occ._[0] >> 11) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[0] >> 22) & 0b11111111111;
  blockers = ((uint64_t)occ._[0] >> 22) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[0] >> 33) & 0b11111111111;
  blockers = ((uint64_t)occ._[0] >> 33) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[0] >> 44) & 0b11111111111;
  blockers = ((uint64_t)occ._[0] >> 44) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[1] >> 2) & 0b11111111111;
  blockers = ((uint64_t)occ._[1] >> 2) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[1] >> 13) & 0b11111111111;
  blockers = ((uint64_t)occ._[1] >> 13) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[1] >> 24) & 0b11111111111;
  blockers = ((uint64_t)occ._[1] >> 24) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[1] >> 35) & 0b11111111111;
  blockers = ((uint64_t)occ._[1] >> 35) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team._[1] >> 46) & 0b11111111111;
  blockers = ((uint64_t)occ._[1] >> 46) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[0] >> 0) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[0] >> 0) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[0] >> 11) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[0] >> 11) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[0] >> 22) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[0] >> 22) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[0] >> 33) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[0] >> 33) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[0] >> 44) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[0] >> 44) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[1] >> 2) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[1] >> 2) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[1] >> 13) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[1] >> 13) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[1] >> 24) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[1] >> 24) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[1] >> 35) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[1] >> 35) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((uint64_t)team_r._[1] >> 46) & 0b11111111111;
  blockers = ((uint64_t)occ_r._[1] >> 46) & 0b11111111111;
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

    apply_captures_niave(
        friends,
        &b.{enemy}{rotation},
        &b.{enemy}{"_r" if not rotation else ""},
        {"true" if rotation else "false"});
  }}

  shield_wall_{color}(&b, dest);

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
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.white, &b.white_r, false);
  }

  shield_wall_black(&b, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.white, &b.white_r, false);
  }

  shield_wall_black(&b, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.white, &b.white_r, false);
  }

  shield_wall_black(&b, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_lower(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.white_r, &b.white, true);
  }

  shield_wall_black(&b, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.white_r, &b.white, true);
  }

  shield_wall_black(&b, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.white_r, &b.white, true);
  }

  shield_wall_black(&b, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_lower(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.black, &b.black_r, false);
  }

  shield_wall_white(&b, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.black, &b.black_r, false);
  }

  shield_wall_white(&b, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.black, &b.black_r, false);
  }

  shield_wall_white(&b, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_lower(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.black_r, &b.black, true);
  }

  shield_wall_white(&b, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.black_r, &b.black, true);
  }

  shield_wall_white(&b, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    uint8_t orig,
    uint8_t dest,
    const layer cap_dests) {
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

    apply_captures_niave(friends, &b.black_r, &b.black, true);
  }

  shield_wall_white(&b, dest);

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
static inline __attribute__((always_inline)) void
get_next_row_boards_{color}{rotation}_{index}( board *boards, const uint64_t
occ, const board *board, int *total, move *moves, const layer cap_dests, int
row_offset ) {{ unsigned short movers = (board->{color}{rotation}._[{index}] >>
row_offset) & 0b11111111111; while (movers) {{ uint8_t orig =
_tzcnt_u16(movers); const unsigned short blockers =
      ((uint64_t)occ >> row_offset) & 0b11111111111;
      uint64_t row_moves =
        (uint64_t)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {{
          uint8_t dest = _tzcnt_u64(row_moves);
          {process_move_name}(
            *board, boards, moves, total, orig, dest, cap_dests);
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
        *board, boards, moves, total, orig, dest, cap_dests);
      row_moves &= row_moves - 1;
    }}
    movers &= movers - 1;
  }}
}}
"""

for (color, rotation) in product(["black", "white"], ["", "_r"]):
    cog.outl(build_c(color, rotation))

]]]*/

static inline __attribute__((always_inline)) void get_next_row_boards_black_0(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_black_lower(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_black_1(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_black_upper(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_black_r_0(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black_r._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_black_r_lower(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_black_r_1(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black_r._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_black_r_upper(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_0(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_white_lower(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_1(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_white_upper(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_r_0(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white_r._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_white_r_lower(
          *board, boards, moves, total, orig, dest, cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_r_1(
    board *boards,
    const uint64_t occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white_r._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    uint8_t orig = _tzcnt_u16(movers);
    const unsigned short blockers =
        ((uint64_t)occ >> row_offset) & 0b11111111111;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][orig]
                         << row_offset;
    orig += row_offset;
    while (row_moves) {
      uint8_t dest = _tzcnt_u64(row_moves);
      process_move_white_r_upper(
          *board, boards, moves, total, orig, dest, cap_dests);
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
    const layer cap_dests) {
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
          *board, boards, moves, total, orig, dest, cap_dests);
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
    const layer cap_dests) {
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
          *board, boards, moves, total, orig, dest, cap_dests);
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
    const layer cap_dests) {
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
          *board, boards, moves, total, orig, dest, cap_dests);
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
    const layer cap_dests) {
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
          *board, boards, moves, total, orig, dest, cap_dests);
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

const layer drop_2_east = {18338604880312133628ULL, 143903978713751539ULL};
const layer drop_2_west = {18419709275360197119ULL, 35975994678437884ULL};

layer find_capture_destinations(
    const layer allies, const layer foes, const layer occ) {
  layer north = layer_shiftl(layer_and(layer_shiftl(allies, 11), foes), 11);
  layer south = layer_shiftr(layer_and(layer_shiftr(allies, 11), foes), 11);
  layer east = layer_shiftr(
      layer_and(layer_shiftr(layer_and(allies, drop_2_east), 1), foes), 1);
  layer west = layer_shiftl(
      layer_and(layer_shiftl(layer_and(allies, drop_2_west), 1), foes), 1);
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
    occ{r}._[{sub}],
    &current,
    total,
    moves,
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
    capture_dests
  );

  get_next_row_boards_center_{color}_r(
    boards,
    occ_r,
    &current,
    total,
    moves,
    capture_dests
  );"""

    out += "\n}"
    return out


for color in ["white", "black"]:
    cog.outl(build(color))

]]]*/

void get_team_moves_white(
    const board current, int *total, move *moves, board *boards) {
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = layer_or(current.white, corners);
  allies = layer_or(allies, current.king);
  layer allies_r = layer_or(current.white_r, corners);
  layer foes = current.black;
  layer foes_r = current.black_r;

  const layer capture_dests = find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
      find_capture_destinations(allies_r, foes_r, occ_r);

  get_next_row_boards_white_0(
      boards, occ._[0], &current, total, moves, capture_dests, 0);

  get_next_row_boards_white_0(
      boards, occ._[0], &current, total, moves, capture_dests, 11);

  get_next_row_boards_white_0(
      boards, occ._[0], &current, total, moves, capture_dests, 22);

  get_next_row_boards_white_0(
      boards, occ._[0], &current, total, moves, capture_dests, 33);

  get_next_row_boards_white_0(
      boards, occ._[0], &current, total, moves, capture_dests, 44);

  get_next_row_boards_white_1(
      boards, occ._[1], &current, total, moves, capture_dests, 2);

  get_next_row_boards_white_1(
      boards, occ._[1], &current, total, moves, capture_dests, 13);

  get_next_row_boards_white_1(
      boards, occ._[1], &current, total, moves, capture_dests, 24);

  get_next_row_boards_white_1(
      boards, occ._[1], &current, total, moves, capture_dests, 35);

  get_next_row_boards_white_1(
      boards, occ._[1], &current, total, moves, capture_dests, 46);

  get_next_row_boards_white_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 0);

  get_next_row_boards_white_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 11);

  get_next_row_boards_white_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 22);

  get_next_row_boards_white_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 33);

  get_next_row_boards_white_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 44);

  get_next_row_boards_white_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 2);

  get_next_row_boards_white_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 13);

  get_next_row_boards_white_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 24);

  get_next_row_boards_white_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 35);

  get_next_row_boards_white_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 46);

  get_next_row_boards_center_white(
      boards, occ, &current, total, moves, capture_dests);

  get_next_row_boards_center_white_r(
      boards, occ_r, &current, total, moves, capture_dests);
}

void get_team_moves_black(
    const board current, int *total, move *moves, board *boards) {
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = layer_or(current.black, corners);

  layer allies_r = layer_or(current.black_r, corners);
  layer foes = current.white;
  layer foes_r = current.white_r;

  const layer capture_dests = find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
      find_capture_destinations(allies_r, foes_r, occ_r);

  get_next_row_boards_black_0(
      boards, occ._[0], &current, total, moves, capture_dests, 0);

  get_next_row_boards_black_0(
      boards, occ._[0], &current, total, moves, capture_dests, 11);

  get_next_row_boards_black_0(
      boards, occ._[0], &current, total, moves, capture_dests, 22);

  get_next_row_boards_black_0(
      boards, occ._[0], &current, total, moves, capture_dests, 33);

  get_next_row_boards_black_0(
      boards, occ._[0], &current, total, moves, capture_dests, 44);

  get_next_row_boards_black_1(
      boards, occ._[1], &current, total, moves, capture_dests, 2);

  get_next_row_boards_black_1(
      boards, occ._[1], &current, total, moves, capture_dests, 13);

  get_next_row_boards_black_1(
      boards, occ._[1], &current, total, moves, capture_dests, 24);

  get_next_row_boards_black_1(
      boards, occ._[1], &current, total, moves, capture_dests, 35);

  get_next_row_boards_black_1(
      boards, occ._[1], &current, total, moves, capture_dests, 46);

  get_next_row_boards_black_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 0);

  get_next_row_boards_black_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 11);

  get_next_row_boards_black_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 22);

  get_next_row_boards_black_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 33);

  get_next_row_boards_black_r_0(
      boards, occ_r._[0], &current, total, moves, capture_dests_r, 44);

  get_next_row_boards_black_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 2);

  get_next_row_boards_black_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 13);

  get_next_row_boards_black_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 24);

  get_next_row_boards_black_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 35);

  get_next_row_boards_black_r_1(
      boards, occ_r._[1], &current, total, moves, capture_dests_r, 46);

  get_next_row_boards_center_black(
      boards, occ, &current, total, moves, capture_dests);

  get_next_row_boards_center_black_r(
      boards, occ_r, &current, total, moves, capture_dests);
}
//[[[end]]]

void get_king_moves(
    const board current, int *total, move *moves, board *boards) {
  const layer occ = {
      current.white._[0] | current.black._[0] | current.king._[0],
      current.white._[1] | current.black._[1] | current.king._[1]};

  // const layer capture_dests = find_capture_destinations_op(current.white,
  // current.black);

  uint8_t orig = current.king._[0] ? _tzcnt_u64(current.king._[0])
                                   : _tzcnt_u64(current.king._[1]) + 64;

  if (orig < 55) {
    const uint row_offset = sub_layer_row_offset[orig];
    const uint8_t row_orig = orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ._[0] >> row_offset) & 0x7FF;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][row_orig]
                         << row_offset;
    while (row_moves) {
      const uint8_t dest = _tzcnt_u64(row_moves);
      const uint8_t dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[0] = (uint64_t)1 << dest;
      new_board.king._[1] = 0;
      new_board.king_r._[!sub_layer(dest_r)] = 0;
      new_board.king_r._[sub_layer(dest_r)] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);
      // handle captures
      // if (capture_dests[0] & (1 << dest)) {
      apply_captures_niave(
          layer_or(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      // }
      shield_wall_white(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else if (orig > 65) {
    const uint8_t sub_orig = orig - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const uint8_t row_orig = sub_orig - row_offset;
    const uint16_t blockers = ((uint64_t)occ._[1] >> row_offset) & 0x7FF;
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
      new_board.king._[0] = 0;
      new_board.king._[1] = (uint64_t)1 << sub_dest;
      new_board.king_r._[!sub_layer(dest_r)] = 0;
      new_board.king_r._[sub_layer(dest_r)] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);
      apply_captures_niave(
          layer_or(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      shield_wall_white(&new_board, dest);
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
      new_board.king._[!sub_layer(dest)] = 0;
      new_board.king._[sub_layer(dest)] = (uint64_t)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king_r._[!sub_layer(dest_r)] = 0;
      new_board.king_r._[sub_layer(dest_r)] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);
      // handle captures
      // if (capture_dests[1] & (1 << sub_dest)) {
      apply_captures_niave(
          layer_or(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      // }
      shield_wall_white(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  }

  const layer occ_r = {
      current.white_r._[0] | current.black_r._[0] | current.king_r._[0],
      current.white_r._[1] | current.black_r._[1] | current.king_r._[1]};

  uint8_t orig_r = rotate_right[orig];

  if (orig_r < 55) {
    const uint row_offset = sub_layer_row_offset[orig_r];
    const uint8_t row_orig = orig_r - row_offset;
    const uint16_t blockers = ((uint64_t)occ_r._[0] >> row_offset) & 0x7FF;
    uint64_t row_moves = (uint64_t)row_moves_table[blockers][row_orig]
                         << row_offset;
    while (row_moves) {
      const uint8_t dest_r = _tzcnt_u64(row_moves);
      const uint8_t dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[sub_layer(dest)] = (uint64_t)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king._[!sub_layer(dest)] = 0;
      new_board.king_r._[0] = (uint64_t)1 << dest_r;
      new_board.king_r._[1] = 0;
      // handle captures
      // if (capture_dests[0] & (1 << dest)) {
      apply_captures_niave(
          layer_or(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      // }
      shield_wall_white(&new_board, dest);
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
    const uint16_t blockers = ((uint64_t)occ_r._[1] >> row_offset) & 0x7FF;
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
      new_board.king._[!sub_layer(dest)] = 0;
      new_board.king._[sub_layer(dest)] = (uint64_t)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king_r._[0] = 0;
      new_board.king_r._[1] = (uint64_t)1 << sub_dest;
      apply_captures_niave(
          layer_or(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      shield_wall_white(&new_board, dest);
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
      new_board.king._[!sub_layer(dest)] = 0;
      new_board.king._[sub_layer(dest)] = (uint64_t)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king_r._[!sub_layer(dest_r)] = 0;
      new_board.king_r._[sub_layer(dest_r)] =
          (uint64_t)1 << (sub_layer_offset_direct[dest_r]);

      apply_captures_niave(
          layer_or(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      shield_wall_white(&new_board, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  }
}

/* Generate a layer of locations which, when landed upon, _may_
trigger a shield wall capture.

The idea is not to be perfectly accurate, but rather to rule out a
majority of ineligible edge positions via a relatively cheap "bulk"
computation, so that not every move to an edge position need do a full
shield wall check. As such, the resulting layer may contain false
positives, but may not contain false negatives, as the latter would
never be checked and thus go undiscovered.
*/
layer gen_shield_wall_triggers(
    const layer allies, const layer foes, const layer occ) {
  layer triggers = EMPTY_LAYER;

  // north
  {
    uint64_t edges = foes._[1] & (allies._[1] << 11);
    uint64_t left = occ._[1] & (edges << 1);
    uint64_t right = occ._[1] & (edges >> 1);
    triggers._[1] |= left | right;
  }

  // south
  {
    uint64_t edges = foes._[0] & (allies._[0] >> 11);
    uint64_t left = occ._[0] & (edges << 1);
    uint64_t right = occ._[0] & (edges >> 1);
    triggers._[0] |= left | right;
  }

  // east
  {
    layer edges = layer_and(foes, layer_shiftr(allies, 1));
    layer up = layer_and(occ, layer_shiftl(edges, 11));
    layer down = layer_and(occ, layer_shiftr(edges, 11));
    triggers = layer_or(triggers, layer_or(up, down));
  }

  // west
  {
    layer edges = layer_and(foes, layer_shiftl(allies, 1));
    layer up = layer_and(occ, layer_shiftl(edges, 11));
    layer down = layer_and(occ, layer_shiftr(edges, 11));
    triggers = layer_or(triggers, layer_or(up, down));
  }

  // only at the end do we apply the edge mask.
  triggers = layer_and(triggers, EDGES);

  return triggers;
}

void gen_reference_moves_black3(
    const board b, int *total, move *ms, board *bs) {
  *total = 0;
  layer occ = board_occ(b);

  const layer capture_dests = find_capture_destinations(b.black, b.white, occ);
  const layer shield_dests = gen_shield_wall_triggers(b.black, b.white, occ);

  int dest;

  int orig = 0;
  uint64_t pieces = b.black._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = rank(orig);
    int remaining_north = 10 - rank;
    while (remaining_north--) {
      dest += 11;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_black(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // south
    dest = orig;
    // rank = rank(orig);
    while (rank--) {
      dest -= 11;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_black(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // west
    dest = orig;
    int file = file(orig);
    int remaining_south = 10 - file;
    while (remaining_south--) {
      dest += 1;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_black(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // east
    dest = orig;
    // file = file(orig);
    while (file--) {
      dest -= 1;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.black, orig, |=);
      op_layer_bit(b2.black_r, rotate_right[orig], |=);
      op_layer_bit(b2.black, dest, |=);
      op_layer_bit(b2.black_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_black(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    pieces -= 1;
    pieces >>= to_next;
  }
  if (lower) {
    orig = 64;
    pieces = b.black._[1];
    lower = false;
    goto process;
  }
}

void gen_reference_moves_white3(
    const board b, int *total, move *ms, board *bs) {
  *total = 0;
  layer occ = board_occ(b);

  const layer capture_dests = find_capture_destinations(b.white, b.black, occ);
  const layer shield_dests = gen_shield_wall_triggers(b.white, b.black, occ);

  int dest;

  int orig = 0;
  uint64_t pieces = b.white._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = rank(orig);
    int remaining_north = 10 - rank;
    while (remaining_north--) {
      dest += 11;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.white, orig, |=);
      op_layer_bit(b2.white_r, rotate_right[orig], |=);
      op_layer_bit(b2.white, dest, |=);
      op_layer_bit(b2.white_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_white(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // south
    dest = orig;
    // rank = rank(orig);
    while (rank--) {
      dest -= 11;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.white, orig, |=);
      op_layer_bit(b2.white_r, rotate_right[orig], |=);
      op_layer_bit(b2.white, dest, |=);
      op_layer_bit(b2.white_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_white(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // west
    dest = orig;
    int file = file(orig);
    int remaining_south = 10 - file;
    while (remaining_south--) {
      dest += 1;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.white, orig, |=);
      op_layer_bit(b2.white_r, rotate_right[orig], |=);
      op_layer_bit(b2.white, dest, |=);
      op_layer_bit(b2.white_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_white(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // east
    dest = orig;
    // file = file(orig);
    while (file--) {
      dest -= 1;
      if (check_index(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      op_layer_bit(b2.white, orig, |=);
      op_layer_bit(b2.white_r, rotate_right[orig], |=);
      op_layer_bit(b2.white, dest, |=);
      op_layer_bit(b2.white_r, rotate_right[dest], |=);

      if (check_index(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (check_index(shield_dests, dest))
        shield_wall_white(&b2, dest);

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    pieces -= 1;
    pieces >>= to_next;
  }
  if (lower) {
    orig = 64;
    pieces = b.white._[1];
    lower = false;
    goto process;
  }
}

// -----------------------------------------------------------------------------
// Move map methods

const uint8_t rank_mod[121] = {
    0,   0,   0,   0,   0,   0,   0,   0,   0,  0,  0,  11, 11, 11, 11,  11,
    11,  11,  11,  11,  11,  11,  22,  22,  22, 22, 22, 22, 22, 22, 22,  22,
    22,  33,  33,  33,  33,  33,  33,  33,  33, 33, 33, 33, 44, 44, 44,  44,
    44,  44,  44,  44,  44,  44,  44,  55,  55, 55, 55, 55, 55, 55, 55,  55,
    55,  55,  66,  66,  66,  66,  66,  66,  66, 66, 66, 66, 66, 77, 77,  77,
    77,  77,  77,  77,  77,  77,  77,  77,  88, 88, 88, 88, 88, 88, 88,  88,
    88,  88,  88,  99,  99,  99,  99,  99,  99, 99, 99, 99, 99, 99, 110, 110,
    110, 110, 110, 110, 110, 110, 110, 110, 110};

union sources_u {
  struct sources dirs;
  uint64_t all;
};

void gen_king_mm(board b, layer occ, int orig, move_map mm) {
  int dest;

  // north
  dest = orig;
  int rank = rank(orig);
  int remaining_north = 10 - rank;
  while (remaining_north--) {
    dest += 11;

    mm[dest].south = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }

  // south
  dest = orig;
  while (rank--) {
    dest -= 11;

    mm[dest].north = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }

  // west
  dest = orig;
  int file = file(orig);
  int remaining_south = 10 - file;
  while (remaining_south--) {
    dest += 1;

    mm[dest].east = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }

  // east
  dest = orig;
  // file = file(orig);
  while (file--) {
    dest -= 1;

    mm[dest].west = orig;

    // if this position is occupied we stop here
    if (check_index(occ, dest))
      break;
  }
}

#define FALLBACK(_x, _y) (_x ? _x : _y)

/*
Macros to get the position closest to the edge in a given direction
relative to the position provided.
*/

#define DIR_EDGE_north(_pos) (file(_pos) + 110)
#define DIR_EDGE_south(_pos) (file(_pos))
#define DIR_EDGE_east(_pos) (rank_mod[_pos])
#define DIR_EDGE_west(_pos) (rank_mod[_pos] + 10)
#define DIR_EDGE(_pos, _dir) DIR_EDGE_##_dir(_pos)

/*
Macros to get the index of the closest piece in a given direction, or
if no such piece exists, the index at the edge of the board in that
direction.
*/

#define ALL_DIR(_x, _dir) (allies[_x]._dir | them1[_x]._dir | them2[_x]._dir)
#define DIRMOST(_pos, _dir)                                                    \
  (FALLBACK(ALL_DIR(_pos, _dir), DIR_EDGE(_pos, _dir)))
#define NORTHMOST(_pos) DIRMOST(_pos, north)
#define SOUTHMOST(_pos) DIRMOST(_pos, south)
#define EASTMOST(_pos) DIRMOST(_pos, east)
#define WESTMOST(_pos) DIRMOST(_pos, west)

inline void departure_rank_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the east
  for (int i = EASTMOST(pos); i < pos; i++) {
    allies[i].west = allies[pos].west;
    them1[i].west = them1[pos].west;
    them2[i].west = them2[pos].west;
  }

  // correct the rank to the west
  for (int i = WESTMOST(pos); i > pos; i--) {
    allies[i].east = allies[pos].east;
    them1[i].east = them1[pos].east;
    them2[i].east = them2[pos].east;
  }
}

inline void arrival_rank_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the east
  for (int i = EASTMOST(pos); i < pos; i++) {
    allies[i].west = pos;
    them1[i].west = 0;
    them2[i].west = 0;
  }

  // correct the rank to the west
  for (int i = WESTMOST(pos); i > pos; i--) {
    allies[i].east = pos;
    them1[i].east = 0;
    them2[i].east = 0;
  }
}

inline void departure_file_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the south
  for (int i = SOUTHMOST(pos); i < pos; i += 11) {
    allies[i].north = allies[pos].north;
    them1[i].north = them1[pos].north;
    them2[i].north = them2[pos].north;
  }

  // correct the rank to the north
  for (int i = NORTHMOST(pos); i > pos; i -= 11) {
    allies[i].south = allies[pos].south;
    them1[i].south = them1[pos].south;
    them2[i].south = them2[pos].south;
  }
}

inline void arrival_file_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  // correct the rank to the south
  for (int i = SOUTHMOST(pos); i < pos; i += 11) {
    allies[i].north = pos;
    them1[i].north = 0;
    them2[i].north = 0;
  }

  // correct the rank to the north
  for (int i = NORTHMOST(pos); i > pos; i -= 11) {
    allies[i].south = pos;
    them1[i].south = 0;
    them2[i].south = 0;
  }
}

inline void capture_correction(
    const uint8_t pos, move_map allies, move_map them1, move_map them2) {
  departure_file_correction(pos, allies, them1, them2);
  departure_rank_correction(pos, allies, them1, them2);
}

void apply_southward_move(
    const uint8_t orig,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {
  // correct file positions north of the src
  const uint8_t north_stop = FALLBACK(ALL_DIR(orig, north), 120);
  for (int i = orig + 11; i <= north_stop; i += 11) {
    allies[i].south = dest;
  }

  // correct file positions south of the destination
  const uint8_t south_occ =
      allies[orig].south | them1[orig].south | them2[orig].south;
  for (int i = dest - 11; i >= (south_occ ? south_occ : 0); i -= 11) {
    allies[i].north = dest;
  }

  // correct file positions between src and dest
  for (int i = orig - 11; i > dest; i -= 11) {
    allies[i].north = allies[orig].north;
    allies[i].south = dest;
    them1[i].north = them1[orig].north;
    them1[i].south = 0;
    them2[i].north = them2[orig].north;
    them2[i].south = 0;
  }

  // correct dest north
  allies[dest].north = allies[orig].north;
  them1[dest].north = them1[orig].north;
  them2[dest].north = them2[orig].north;

  // correct src south
  allies[orig].south = dest;
  them1[orig].south = 0;
  them2[orig].south = 0;

  departure_rank_correction(orig, allies, them1, them2);
  arrival_rank_correction(dest, allies, them1, them2);
}

void apply_northward_move(
    const uint8_t src,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {
  // correct file positions north of the dest
  const uint8_t north_occ =
      allies[src].north | them1[src].north | them2[src].north;
  for (int i = dest + 11; i <= (north_occ ? north_occ : 120); i += 11) {
    allies[i].south = dest;
  }

  // correct file positions south of the src
  const uint8_t south_occ =
      allies[src].south | them1[src].south | them2[src].south;
  for (int i = src - 11; i >= (south_occ ? south_occ : 0); i -= 11) {
    allies[i].north = dest;
  }

  // correct file positions between src and dest
  for (int i = src + 11; i < dest; i += 11) {
    allies[i].north = dest;
    allies[i].south = allies[src].south;
    them1[i].north = 0;
    them1[i].south = them1[src].south;
    them2[i].north = 0;
    them2[i].south = them2[src].south;
  }

  // correct dest south
  allies[dest].south = allies[src].south;
  them1[dest].south = them1[src].south;
  them2[dest].south = them2[src].south;

  // correct src north
  allies[src].north = dest;
  them1[src].north = 0;
  them2[src].north = 0;

  departure_rank_correction(src, allies, them1, them2);
  arrival_rank_correction(dest, allies, them1, them2);
}

void apply_eastward_move(
    const uint8_t orig,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {
  // correct rank positions west of the orig
  for (int i = WESTMOST(orig); i > orig; i--) {
    allies[i].east = dest;
  }

  // correct rank positions east of the dest
  for (int i = EASTMOST(dest); i < dest; i++) {
    allies[i].west = dest;
  }

  // correct rank positions between src and dest
  for (int i = dest + 1; i < orig; i++) {
    allies[i].west = allies[orig].west;
    allies[i].east = dest;
    them1[i].west = them1[orig].west;
    them1[i].east = 0;
    them2[i].west = them2[orig].west;
    them2[i].east = 0;
  }

  // correct dest west
  allies[dest].west = allies[orig].west;
  them1[dest].west = them1[orig].west;
  them2[dest].west = them2[orig].west;

  // correct orig east
  allies[orig].east = dest;
  them1[orig].east = 0;
  them2[orig].east = 0;

  departure_file_correction(orig, allies, them1, them2);
  arrival_file_correction(dest, allies, them1, them2);
}

void apply_westward_move(
    const uint8_t orig,
    const uint8_t dest,
    move_map allies,
    move_map them1,
    move_map them2) {

  // correct rank positions west of the dest
  for (int i = WESTMOST(dest); i > dest; i--) {
    allies[i].east = dest;
  }

  // correct rank positions east of the orig
  for (int i = EASTMOST(orig); i < orig; i++) {
    allies[i].west = dest;
  }

  // correct rank positions between src and dest
  for (int i = orig + 1; i < dest; i++) {
    allies[i].west = dest;
    allies[i].east = allies[orig].east;
    them1[i].west = 0;
    them1[i].east = them1[orig].east;
    them2[i].west = 0;
    them2[i].east = them2[orig].east;
  }

  // correct dest east
  allies[dest].east = allies[orig].east;
  them1[dest].east = them1[orig].east;
  them2[dest].east = them2[orig].east;

  // correct orig west
  allies[orig].west = dest;
  them1[orig].west = 0;
  them2[orig].west = 0;

  departure_file_correction(orig, allies, them1, them2);
  arrival_file_correction(dest, allies, them1, them2);
}

void build_mm(layer movers, const layer occ, move_map mm) {

  int dest;

  int orig = 0;
  uint64_t pieces = movers._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = rank(orig);
    int remaining_north = 10 - rank;
    while (remaining_north--) {
      dest += 11;
      mm[dest].south = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    // south
    dest = orig;
    while (rank--) {
      dest -= 11;
      mm[dest].north = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    // west
    dest = orig;
    int file = file(orig);
    int remaining_south = 10 - file;
    while (remaining_south--) {
      dest += 1;
      mm[dest].east = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    // east
    dest = orig;
    // file = file(orig);
    while (file--) {
      dest -= 1;
      mm[dest].west = orig;
      // if this position is occupied we stop here
      if (check_index(occ, dest))
        break;
    }

    pieces -= 1;
    pieces >>= to_next;
  }
  if (lower) {
    orig = 64;
    pieces = movers._[1];
    lower = false;
    goto process;
  }
}

struct move_maps build_mms(board b) {
  struct move_maps mms;
  memset(&mms, 0, sizeof(mms));

  layer occ = board_occ(b);
  build_mm(b.white, occ, mms.white);
  build_mm(b.black, occ, mms.black);
  build_mm(b.king, occ, mms.king);

  return mms;
}

void gen_moves_from_mm_white(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  uint64_t remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.white, dest, |=);
    op_layer_bit(departed.white_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].north, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].north], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].south, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].south], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].east, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].east], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].west, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].west], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_white_capture(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  uint64_t remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.white, dest, |=);
    op_layer_bit(departed.white_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].north, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].north], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].south, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].south], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].east, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].east], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.white, mm[dest].west, -=);
      op_layer_bit(b2.white_r, rotate_right[mm[dest].west], -=);
      apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_black(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  uint64_t remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.black, dest, |=);
    op_layer_bit(departed.black_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].north, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].north], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].south, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].south], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].east, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].east], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].west, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].west], -=);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_black_capture(
    board b,
    layer dests,
    move_map mm,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;
  layer occ = board_occ(b);

  uint64_t remaining = dests._[0];
  int dest = 0;
  bool lower = true;

process:
  while (remaining) {
    int to_next = _tzcnt_u64(remaining);
    dest += to_next;

    // type pun the source to a uint32_t and check it, breaking if false

    board departed = b;
    op_layer_bit(departed.black, dest, |=);
    op_layer_bit(departed.black_r, rotate_right[dest], |=);

    if (mm[dest].north) {
      ms[(*total)] = (move){mm[dest].north, dest};
      ds[(*total)] = south;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].north, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].north], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].south) {
      ms[(*total)] = (move){mm[dest].south, dest};
      ds[(*total)] = north;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].south, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].south], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].east) {
      ms[(*total)] = (move){mm[dest].east, dest};
      ds[(*total)] = west;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].east, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].east], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    if (mm[dest].west) {
      ms[(*total)] = (move){mm[dest].west, dest};
      ds[(*total)] = east;
      board b2 = departed;
      op_layer_bit(b2.black, mm[dest].west, -=);
      op_layer_bit(b2.black_r, rotate_right[mm[dest].west], -=);
      apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);
      bs[(*total)] = b2;
      (*total)++;
    }

    remaining -= 1;
    remaining >>= to_next;
  }
  if (lower) {
    dest = 64;
    remaining = dests._[1];
    lower = false;
    goto process;
  }
}

void gen_moves_from_mm_king(
    const board b,
    const int orig,
    const move_map allies,
    const move_map them1,
    const move_map them2,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;

  // north moves
  int north_occ = FALLBACK(them1[orig].north | them2[orig].north, 121);
  for (int dest = orig + 11; dest < north_occ; dest += 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = north;
    (*total)++;
  }

  // south moves
  int south_occ = FALLBACK(them1[orig].south | them2[orig].south, -1);
  for (int dest = orig - 11; dest > south_occ; dest -= 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = south;
    (*total)++;
  }

  // east moves
  int east_occ =
      FALLBACK(them1[orig].east | them2[orig].east, rank_mod[orig] - 1);
  for (int dest = orig - 1; dest > east_occ; dest--) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = east;
    (*total)++;
  }

  // west moves
  int west_occ =
      FALLBACK(them1[orig].west | them2[orig].west, rank_mod[orig] + 11);
  for (int dest = orig + 1; dest < west_occ; dest++) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = west;
    (*total)++;
  }
}

void gen_moves_from_mm_king_capture(
    const board b,
    const int orig,
    const move_map allies,
    const move_map them1,
    const move_map them2,
    move *ms,
    dir *ds,
    board *bs,
    int *total) {
  *total = 0;

  // north moves
  int north_occ = FALLBACK(them1[orig].north | them2[orig].north, 121);
  for (int dest = orig + 11; dest < north_occ; dest += 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = north;
    (*total)++;
  }

  // south moves
  int south_occ = FALLBACK(them1[orig].south | them2[orig].south, -1);
  for (int dest = orig - 11; dest > south_occ; dest -= 11) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = south;
    (*total)++;
  }

  // east moves
  int east_occ =
      FALLBACK(them1[orig].east | them2[orig].east, rank_mod[orig] - 1);
  for (int dest = orig - 1; dest > east_occ; dest--) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = east;
    (*total)++;
  }

  // west moves
  int west_occ =
      FALLBACK(them1[orig].west | them2[orig].west, rank_mod[orig] + 11);
  for (int dest = orig + 1; dest < west_occ; dest++) {
    board b2 = b;
    b2.king = EMPTY_LAYER;
    b2.king_r = EMPTY_LAYER;
    op_layer_bit(b2.king, dest, |=);
    op_layer_bit(b2.king_r, rotate_right[dest], |=);
    apply_captures_niave(b2.king, &b2.black, &b2.black_r, dest);
    bs[(*total)] = b2;
    ms[*total] = (struct move){orig, dest};
    ds[(*total)] = west;
    (*total)++;
  }
}

// -----------------------------------------------------------------------------
// king corner access

#define dirty_get_row_0(l) (uint64_t) l._[0]
#define dirty_get_row_1(l) ((uint64_t)l._[0] >> 11)
#define dirty_get_row_2(l) ((uint64_t)l._[0] >> 22)
#define dirty_get_row_3(l) ((uint64_t)l._[0] >> 33)
#define dirty_get_row_4(l) ((uint64_t)l._[0] >> 44)
#define dirty_get_row_5(l)                                                     \
  ((uint64_t)l._[0] >> 55) | ((((uint64_t)l._[1] & 0x3) << 9))
#define dirty_get_row_6(l) ((uint64_t)l._[1] >> 2)
#define dirty_get_row_7(l) ((uint64_t)l._[1] >> 13)
#define dirty_get_row_8(l) ((uint64_t)l._[1] >> 24)
#define dirty_get_row_9(l) ((uint64_t)l._[1] >> 35)
#define dirty_get_row_10(l) ((uint64_t)l._[1] >> 46)

#define mask_rightward(x) (((uint16_t)1 << x) - 1)
#define mask_leftward(x) ((0x7fe << x) & 0x7fe)

const layer below_0 = {0ULL, 0ULL};
const layer below_1 = {2047ULL, 0ULL};
const layer below_2 = {4194303ULL, 0ULL};
const layer below_3 = {8589934591ULL, 0ULL};
const layer below_4 = {17592186044415ULL, 0ULL};
const layer below_5 = {36028797018963967ULL, 0ULL};
const layer below_6 = {18446744073709551615ULL, 3ULL};
const layer below_7 = {18446744073709551615ULL, 8191ULL};
const layer below_8 = {18446744073709551615ULL, 16777215ULL};
const layer below_9 = {18446744073709551615ULL, 34359738367ULL};
const layer below_10 = {18446744073709551615ULL, 70368744177663ULL};

layer below_n[11] = {
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
    below_10,
};

const layer above_0 = {18446744073709549568ULL, 18446744073709551615ULL};
const layer above_1 = {18446744073705357312ULL, 18446744073709551615ULL};
const layer above_2 = {18446744065119617024ULL, 18446744073709551615ULL};
const layer above_3 = {18446726481523507200ULL, 18446744073709551615ULL};
const layer above_4 = {18410715276690587648ULL, 18446744073709551615ULL};
const layer above_5 = {0ULL, 18446744073709551612ULL};
const layer above_6 = {0ULL, 18446744073709543424ULL};
const layer above_7 = {0ULL, 18446744073692774400ULL};
const layer above_8 = {0ULL, 18446744039349813248ULL};
const layer above_9 = {0ULL, 18446673704965373952ULL};
const layer above_10 = {0ULL, 18302628885633695744ULL};

layer above_n[11] = {
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
    above_10,
};

const layer file_mask_0 = {36046397799139329ULL, 70403120701444ULL};
const layer file_mask_1 = {72092795598278658ULL, 140806241402888ULL};
const layer file_mask_9 = {9011599449784832ULL, 36046397799139329ULL};
const layer file_mask_10 = {18023198899569664ULL, 72092795598278658ULL};

const layer not_corners = {18446744073709550590ULL, 71987225293750271ULL};

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

#define not_rank file
#define not_file rank

#define APPLY_LEGAL_EDGE_MASK_0 path._[0] &= legal_edge_mask;
#define APPLY_LEGAL_EDGE_MASK_1
#define APPLY_LEGAL_EDGE_MASK_9
#define APPLY_LEGAL_EDGE_MASK_10 path._[1] &= legal_edge_mask;
#define APPLY_LEGAL_EDGE_MASK(_n) APPLY_LEGAL_EDGE_MASK_##_n

#define OFFSET_ROW_0
#define OFFSET_ROW_1 path._[0] <<= 11;
#define OFFSET_ROW_9 path._[1] <<= 35;
#define OFFSET_ROW_10 path._[1] <<= 35;
#define OFFSET_ROW()

#define CORNER_PATH_1_LANE_SIDE(_i, _side)                                     \
  const uint16_t mask_ = mask_##_side##ward(file);                             \
  if (!(row & mask)) {                                                         \
    layer path = {(uint64_t)mask, _i};                                         \
    APPLY_LEGAL_EDGE_MASK(_i);                                                 \
    OFFSET_ROW(_i);                                                            \
    LAYER_OR_ASSG(paths, path);                                                \
  }

#define CORNER_PATH_1_LANE(_i, _orientation)                                   \
  case _i: {                                                                   \
    uint16_t row = dirty_get_row_##_i(occ);                                    \
    CORNER_PATH_1_LANE_SIDE(left);                                             \
    CORNER_PATH_1_LANE_SIDE(right);                                            \
  }; break;

/**
 * Get layer mask for all open 1-move routes to a corner.
 */
layer corner_paths_1(
    const layer occ, const layer occ_r, const int rank, const int file) {
  layer paths = EMPTY_LAYER;

  const uint16_t legal_edge_mask = 0b01111111110;

  switch (rank) {
  case 0: {
    uint16_t row = dirty_get_row_0(occ);
    const uint16_t mask_left = mask_leftward(file);
    if (!(row & mask_left)) {
      // layer path = {(uint64_t)mask_left & legal_edge_mask, 0};
      layer path = {(uint64_t)mask_left, 0};
      path._[0] &= legal_edge_mask;
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t mask_right = mask_rightward(file);
    if (!(row & mask_right)) {
      layer path = {(uint64_t)mask_right & legal_edge_mask, 0};
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  case 1: {
    uint16_t row = dirty_get_row_1(occ);
    uint16_t mask_left = mask_leftward(file);
    if (!(row & mask_left)) {
      layer path = {(uint64_t)mask_left, 0};
      path._[0] <<= 11;
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t mask_right = mask_rightward(file);
    if (!(row & mask_right)) {
      layer path = {(uint64_t)mask_right << 11, 0};
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  case 9: {
    uint16_t row = dirty_get_row_9(occ);
    uint16_t rank_mask_right = mask_rightward(file);
    if (!(row & rank_mask_right)) {
      layer path = {0, (uint64_t)rank_mask_right << 35};
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t rank_mask_left = mask_leftward(file);
    if (!(row & rank_mask_left)) {
      layer path = {0, (uint64_t)rank_mask_left << 35};
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  case 10: {
    uint16_t row = dirty_get_row_10(occ);
    uint16_t rank_mask_right = mask_rightward(file);
    if (!(row & rank_mask_right)) {
      layer path = {0, (uint64_t)(rank_mask_right & legal_edge_mask) << 46};
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t rank_mask_left = mask_leftward(file);
    if (!(row & rank_mask_left)) {
      layer path = {0, (uint64_t)(rank_mask_left & legal_edge_mask) << 46};
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  }

  switch (file) {
  case 0: {
    uint16_t row = dirty_get_row_0(occ_r);
    uint16_t mask_left = mask_leftward(rank);
    if (!(row & mask_left)) {
      layer path =
          layer_and(not_corners, layer_and(file_mask_0, below_n[rank]));
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t mask_right = mask_rightward(rank);
    if (!(row & mask_right)) {
      layer path =
          layer_and(not_corners, layer_and(file_mask_0, above_n[rank]));
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  case 1: {
    uint16_t row = dirty_get_row_1(occ_r);
    uint16_t mask_left = mask_leftward(rank);
    if (!(row & mask_left)) {
      layer path =
          layer_and(not_corners, layer_and(file_mask_1, below_n[rank]));
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t mask_right = mask_rightward(rank);
    if (!(row & mask_right)) {
      layer path =
          layer_and(not_corners, layer_and(file_mask_1, above_n[rank]));
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  case 9: {
    uint16_t row = dirty_get_row_9(occ_r);
    uint16_t rank_mask_right = mask_rightward(rank);
    if (!(row & rank_mask_right)) {
      layer path = layer_and(file_mask_9, above_n[rank]);
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t rank_mask_left = mask_leftward(rank);
    if (!(row & rank_mask_left)) {
      layer path = layer_and(file_mask_9, below_n[rank]);
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  case 10: {
    uint16_t row = dirty_get_row_10(occ_r);
    uint16_t rank_mask_right = mask_rightward(rank);
    if (!(row & rank_mask_right)) {
      layer path =
          layer_and(not_corners, layer_and(file_mask_10, above_n[rank]));
      LAYER_OR_ASSG(paths, path);
    }
    uint16_t rank_mask_left = mask_leftward(rank);
    if (!(row & rank_mask_left)) {
      layer path =
          layer_and(not_corners, layer_and(file_mask_10, below_n[rank]));
      LAYER_OR_ASSG(paths, path);
    }
  }; break;
  }
  return paths;
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

#define EXTRACT_LEFTWARD(_i, _r)                                               \
  uint64_t dest_bit = _blsi_u64(dests);                                        \
  uint8_t dest = _tzcnt_u64(dest_bit);                                         \
  OFFSET(dest, _i);                                                            \
  uint8_t orig =                                                               \
      63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & leftward_occ##_r._[_i]);         \
  uint64_t orig_bit = (uint64_t)1 << orig;                                     \
  OFFSET(orig, _i);

#define EXTRACT_CENTER_LEFTWARD(_l)                                            \
  uint16_t dest_bit = dests & -dests;                                          \
  uint8_t dest = _tzcnt_u16(dest_bit);                                         \
  dest += 55;                                                                  \
  uint8_t orig = 15 - __lzcnt16((dest_bit - 1) & _l);                          \
  orig += 55;

#define DROP_1_EAST_0 18410697675910412286ULL
#define DROP_1_EAST_1 144044784955154427ULL

#define LEFTWARD(_i, _r)                                                       \
  {                                                                            \
    uint64_t dests = targets##_r._[_i] &                                       \
                     (leftward_occ##_r._[_i] - (movers##_r._[_i] << 1)) &      \
                     DROP_1_EAST_##_i;                                         \
    while (dests) {                                                            \
      EXTRACT_LEFTWARD(_i, _r);                                                \
      uint8_t orig_r = ROTATE_DIR(_r)[orig];                                   \
      uint8_t dest_r = ROTATE_DIR(_r)[dest];                                   \
      BOOKKEEP##_r(_i);                                                        \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }

#define LEFTWARD_CENTER(_r)                                                    \
  {                                                                            \
    uint16_t dests = get_center_row(targets##_r) &                             \
                     (center_occ##_r - (center_movers##_r << 1));              \
    while (dests) {                                                            \
      EXTRACT_CENTER_LEFTWARD(center_occ##_r);                                 \
      uint8_t orig_r = ROTATE_DIR(_r)[orig];                                   \
      uint8_t dest_r = ROTATE_DIR(_r)[dest];                                   \
      BOOKKEEP_CENTER##_r();                                                   \
      dests -= dest_bit;                                                       \
    }                                                                          \
  }
/*
leftward
*/

#define RIGHTWARD_DESTS(_i, _r)                                                \
  uint64_t below = orig_bit - 1;                                               \
  uint64_t above_highest_occ_mask =                                            \
      ~((uint64_t)-1 >> _lzcnt_u64(rightward_occ##_r._[_i] & below));          \
  uint64_t dests = targets##_r._[_i] & below & above_highest_occ_mask;

#define RIGHTWARD_DESTS_CENTER(_r)                                             \
  uint16_t below = orig_bit - 1;                                               \
  uint16_t above_highest_occ_mask =                                            \
      (center_occ##_r & below)                                                 \
          ? ((uint16_t)-1 << (16 - __lzcnt16(center_occ##_r & below)))         \
          : (uint16_t)-1;                                                      \
  uint16_t dests = get_center_row(targets##_r) & below & above_highest_occ_mask;

#define RIGHTWARD(_i, _r)                                                      \
  {                                                                            \
    uint64_t origs =                                                           \
        movers##_r._[_i] & ~(rightward_occ##_r._[_i] - targets##_r._[_i]);     \
    while (origs) {                                                            \
                                                                               \
      uint64_t orig_bit = _blsi_u64(origs);                                    \
      uint8_t orig = _tzcnt_u64(orig_bit);                                     \
      OFFSET(orig, _i);                                                        \
      uint8_t orig_r = ROTATE_DIR(_r)[orig];                                   \
      origs -= orig_bit;                                                       \
                                                                               \
      RIGHTWARD_DESTS(_i, _r);                                                 \
                                                                               \
      while (dests) {                                                          \
        uint8_t dest = _tzcnt_u64(dests);                                      \
        uint64_t dest_bit = (uint64_t)1 << dest;                               \
        OFFSET(dest, _i);                                                      \
        uint8_t dest_r = ROTATE_DIR(_r)[dest];                                 \
                                                                               \
        BOOKKEEP##_r(_i);                                                      \
                                                                               \
        dests -= dest_bit;                                                     \
      }                                                                        \
    }                                                                          \
  }

#define RIGHTWARD_CENTER(_r)                                                   \
  {                                                                            \
    uint16_t origs =                                                           \
        center_movers##_r & ~(center_occ##_r - (get_center_row(targets##_r))); \
    while (origs) {                                                            \
                                                                               \
      uint16_t orig_bit = origs & -origs;                                      \
      uint8_t orig = _tzcnt_u16(orig_bit);                                     \
      origs -= orig_bit;                                                       \
      orig += 55;                                                              \
      uint8_t orig_r = ROTATE_DIR(_r)[orig];                                   \
                                                                               \
      RIGHTWARD_DESTS_CENTER(_r);                                              \
                                                                               \
      while (dests) {                                                          \
        uint8_t dest = _tzcnt_u16(dests);                                      \
        uint16_t dest_bit = (uint16_t)1 << dest;                               \
        dest += 55;                                                            \
                                                                               \
        uint8_t dest_r = ROTATE_DIR(_r)[dest];                                 \
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
  op_layer_bit(ls_r[(*total)], orig_r, |=);                                    \
  op_layer_bit(ls_r[(*total)], dest_r, |=);                                    \
  (*total)++;

#define BOOKKEEP_R(_i)                                                         \
  ms[(*total)] = (move){orig_r, dest_r};                                       \
  op_layer_bit(ls_r[(*total)], orig, |=);                                      \
  op_layer_bit(ls_r[(*total)], dest, |=);                                      \
  op_layer_bit(ls[(*total)], orig_r, |=);                                      \
  op_layer_bit(ls[(*total)], dest_r, |=);                                      \
  (*total)++;
#define BOOKKEEP_r BOOKKEEP_R
#define BOOKKEEP_CENTER_r BOOKKEEP_R

#define BOOKKEEP_CENTER()                                                      \
  ms[(*total)] = (move){orig, dest};                                           \
  op_layer_bit(ls[(*total)], orig, |=);                                        \
  op_layer_bit(ls[(*total)], dest, |=);                                        \
  op_layer_bit(ls_r[(*total)], orig_r, |=);                                    \
  op_layer_bit(ls_r[(*total)], dest_r, |=);                                    \
  (*total)++;

#define BIT_AT(_i) ((uint64_t)1 << _i)

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

  uint16_t center_occ = get_center_row(occ);
  uint16_t center_occ_r = get_center_row(occ_r);
  uint16_t center_movers = get_center_row(movers);
  uint16_t center_movers_r = get_center_row(movers_r);
  layer leftward_occ = layer_or(occ, file_mask_0);
  layer leftward_occ_r = layer_or(occ_r, file_mask_0);
  layer rightward_occ = layer_or(occ, file_mask_10);
  layer rightward_occ_r = layer_or(occ_r, file_mask_10);
  // I thought these might be necessary but maybe not...
  // rightward_occ._[1] |= 2;
  // rightward_occ_r._[1] |= 2;
  movers._[0] &= LOWER_HALF_MASK;
  movers._[1] &= UPPER_HALF_MASK;
  movers_r._[0] &= LOWER_HALF_MASK;
  movers_r._[1] &= UPPER_HALF_MASK;

  LEFTWARD(0, );     // lower westward
  LEFTWARD(1, );     // upper westward
  LEFTWARD_CENTER(); // center westward

  LEFTWARD(0, _r);     // lower southward
  LEFTWARD(1, _r);     // upper southward
  LEFTWARD_CENTER(_r); // center southward

  RIGHTWARD(0, );     // lower eastward
  RIGHTWARD(1, );     // upper eastward
  RIGHTWARD_CENTER(); // center eastward

  RIGHTWARD(0, _r);     // lower northward
  RIGHTWARD(1, _r);     // upper northward
  RIGHTWARD_CENTER(_r); // center northward
}

/*
new plan for rightward moves:

I can still do this king of thing:
    uint64_t lefts = movers_r._[1] & ~(occ_r._[1] - targets_r._[1]);
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
