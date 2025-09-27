#include "board.h"
#include "capture.h"
#include "constants.h"
#include "move.h"
#include "stdbool.h"
#include "x86intrin.h" // IWYU pragma: export

u16 row_moves_table[2048][11];
u16 center_row_moves_table[2048][11];

/**
 * find positions that can be moved to from a position given the
 * occupancy of a row.
 * @param occ occupancy of the row, including the bit at `pos`.
 * @param pos the index of the starting position of the moves.
 * @return a u16 wo.
 */
inline u16 get_row_moves(const u16 occ, const u16 pos) {
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
  u16 lower = occ & lowers[pos];
  // set bits to the left of the position
  u16 upper = occ & (0b11111111110 << pos);
  // a mask which begins to the right of the lowest set bit in `upper`
  // (or at the very leftmost position of the row if there are no bits
  // set in `upper`) and extends to the right of the row
  u16 rightward = lowers[__tzcnt_u16(upper | 0x800)];
  // a mask which begins at the highest set bit of `lower` and extends
  // to the right of the row. "blocked" because it represents the
  // positions that can't be reached when moving rightwards
  u16 blocked = 0xFFFF >> __lzcnt16(lower);
  // subtract the blocked positions from the rightward set of
  // positions, leaving only those that can be reached. Also remove
  // the current position.
  return (rightward - blocked) - (1 << pos);
}

/**
 * Populate the global lookup table of row moves
 */
void gen_row_moves() {
  u16 row;
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
  u16 row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      center_row_moves_table[row][pos] =
          get_row_moves(row, pos) & INVERTED_THRONE_MASK;
    }
  }
}

u8 row_move_count_table[2048][11];
u8 center_row_move_count_table[2048][11];

void gen_row_move_counts() {
  u16 row;
  unsigned char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      row_move_count_table[row][pos] =
          __builtin_popcount(get_row_moves(row, pos));
    }
  }
}

void gen_center_row_move_counts() {
  u16 row;
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
inline u8 get_team_move_count(
  const layer occ,
  const layer team,
  const layer occ_r,
  const layer team_r) {

  u8 total = 0;
  u16 movers;
  u16 blockers;""")

for rot in ("", "_r"):
    for index, offse         cog.outl(o)

cog.outl("""
  u16 row;
  u16 prog;

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

u16 get_team_move_count(
    const layer occ,
    const layer team,
    const layer occ_r,
    const layer team_r) {

  u16 total = 0;
  u16 movers;
  u16 blockers;

  movers = ((u64)team._[0] >> 0) & 0b11111111111;
  blockers = ((u64)occ._[0] >> 0) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[0] >> 11) & 0b11111111111;
  blockers = ((u64)occ._[0] >> 11) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[0] >> 22) & 0b11111111111;
  blockers = ((u64)occ._[0] >> 22) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[0] >> 33) & 0b11111111111;
  blockers = ((u64)occ._[0] >> 33) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[0] >> 44) & 0b11111111111;
  blockers = ((u64)occ._[0] >> 44) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[1] >> 2) & 0b11111111111;
  blockers = ((u64)occ._[1] >> 2) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[1] >> 13) & 0b11111111111;
  blockers = ((u64)occ._[1] >> 13) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[1] >> 24) & 0b11111111111;
  blockers = ((u64)occ._[1] >> 24) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[1] >> 35) & 0b11111111111;
  blockers = ((u64)occ._[1] >> 35) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team._[1] >> 46) & 0b11111111111;
  blockers = ((u64)occ._[1] >> 46) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[0] >> 0) & 0b11111111111;
  blockers = ((u64)occ_r._[0] >> 0) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[0] >> 11) & 0b11111111111;
  blockers = ((u64)occ_r._[0] >> 11) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[0] >> 22) & 0b11111111111;
  blockers = ((u64)occ_r._[0] >> 22) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[0] >> 33) & 0b11111111111;
  blockers = ((u64)occ_r._[0] >> 33) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[0] >> 44) & 0b11111111111;
  blockers = ((u64)occ_r._[0] >> 44) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[1] >> 2) & 0b11111111111;
  blockers = ((u64)occ_r._[1] >> 2) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[1] >> 13) & 0b11111111111;
  blockers = ((u64)occ_r._[1] >> 13) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[1] >> 24) & 0b11111111111;
  blockers = ((u64)occ_r._[1] >> 24) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[1] >> 35) & 0b11111111111;
  blockers = ((u64)occ_r._[1] >> 35) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  movers = ((u64)team_r._[1] >> 46) & 0b11111111111;
  blockers = ((u64)occ_r._[1] >> 46) & 0b11111111111;
  while (movers) {
    total += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }

  u16 row;
  u16 prog;

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
  OP_LAYER_BIT(b.{color}{rotation}, orig, -=);
  OP_LAYER_BIT(b.{color}{rotation}, dest, |=);

  is_capture = OP_LAYER_BIT(cap_dests, dest, &);"""

    main_half_handler = f"""
  b.{color}{rotation}._[{index}] -= (u64)1 << orig;
  b.{color}{rotation}._[{index}] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[{index}] & ((u64)1 << dest);"""


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
    u8 orig,
    u8 dest,
    const layer cap_dests
) {{
  board b = base_board;
  bool is_capture;
  {middle_handler if level == "middle" else main_half_handler}
  {upper_index_adjustment if level == "upper" else ""}
  u8 orig_r = {rotation_table}[orig];
  u8 dest_r = {rotation_table}[dest];

  OP_LAYER_BIT(b.{color}{op_rotation}, orig_r, -=);
  OP_LAYER_BIT(b.{color}{op_rotation}, dest_r, |=);

  if (is_capture) {{
    layer friends = LAYER_OR(b.{color}{rotation}, corners);
    {"friends = LAYER_OR(friends, b.king);" if is_black else ""}

    apply_captures_niave(
        friends,
        &b.{enemy}{rotation},
        &b.{enemy}{"_r" if not rotation else ""},
        {"true" if rotation else "false"});
  }}

  u64 z = 0;

  shield_wall_{color}(&b, &z, dest);

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
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.black._[0] -= (u64)1 << orig;
  b.black._[0] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((u64)1 << dest);

  u8 orig_r = rotate_right[orig];
  u8 dest_r = rotate_right[dest];

  OP_LAYER_BIT(b.black_r, orig_r, -=);
  OP_LAYER_BIT(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.black, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.white, &b.white_r, false);
  }

  u64 z = 0;

  shield_wall_black(&b, &z, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  OP_LAYER_BIT(b.black, orig, -=);
  OP_LAYER_BIT(b.black, dest, |=);

  is_capture = OP_LAYER_BIT(cap_dests, dest, &);

  u8 orig_r = rotate_right[orig];
  u8 dest_r = rotate_right[dest];

  OP_LAYER_BIT(b.black_r, orig_r, -=);
  OP_LAYER_BIT(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.black, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.white, &b.white_r, false);
  }

  u64 z = 0;

  shield_wall_black(&b, &z, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.black._[1] -= (u64)1 << orig;
  b.black._[1] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((u64)1 << dest);

  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  u8 orig_r = rotate_right[orig];
  u8 dest_r = rotate_right[dest];

  OP_LAYER_BIT(b.black_r, orig_r, -=);
  OP_LAYER_BIT(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.black, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.white, &b.white_r, false);
  }

  u64 z = 0;

  shield_wall_black(&b, &z, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_lower(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.black_r._[0] -= (u64)1 << orig;
  b.black_r._[0] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((u64)1 << dest);

  u8 orig_r = rotate_left[orig];
  u8 dest_r = rotate_left[dest];

  OP_LAYER_BIT(b.black, orig_r, -=);
  OP_LAYER_BIT(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.black_r, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.white_r, &b.white, true);
  }

  u64 z = 0;

  shield_wall_black(&b, &z, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  OP_LAYER_BIT(b.black_r, orig, -=);
  OP_LAYER_BIT(b.black_r, dest, |=);

  is_capture = OP_LAYER_BIT(cap_dests, dest, &);

  u8 orig_r = rotate_left[orig];
  u8 dest_r = rotate_left[dest];

  OP_LAYER_BIT(b.black, orig_r, -=);
  OP_LAYER_BIT(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.black_r, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.white_r, &b.white, true);
  }

  u64 z = 0;

  shield_wall_black(&b, &z, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_black_r_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.black_r._[1] -= (u64)1 << orig;
  b.black_r._[1] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((u64)1 << dest);

  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  u8 orig_r = rotate_left[orig];
  u8 dest_r = rotate_left[dest];

  OP_LAYER_BIT(b.black, orig_r, -=);
  OP_LAYER_BIT(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.black_r, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.white_r, &b.white, true);
  }

  u64 z = 0;

  shield_wall_black(&b, &z, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_lower(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.white._[0] -= (u64)1 << orig;
  b.white._[0] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((u64)1 << dest);

  u8 orig_r = rotate_right[orig];
  u8 dest_r = rotate_right[dest];

  OP_LAYER_BIT(b.white_r, orig_r, -=);
  OP_LAYER_BIT(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.white, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.black, &b.black_r, false);
  }

  u64 z = 0;

  shield_wall_white(&b, &z, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  OP_LAYER_BIT(b.white, orig, -=);
  OP_LAYER_BIT(b.white, dest, |=);

  is_capture = OP_LAYER_BIT(cap_dests, dest, &);

  u8 orig_r = rotate_right[orig];
  u8 dest_r = rotate_right[dest];

  OP_LAYER_BIT(b.white_r, orig_r, -=);
  OP_LAYER_BIT(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.white, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.black, &b.black_r, false);
  }

  u64 z = 0;

  shield_wall_white(&b, &z, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.white._[1] -= (u64)1 << orig;
  b.white._[1] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((u64)1 << dest);

  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  u8 orig_r = rotate_right[orig];
  u8 dest_r = rotate_right[dest];

  OP_LAYER_BIT(b.white_r, orig_r, -=);
  OP_LAYER_BIT(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.white, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.black, &b.black_r, false);
  }

  u64 z = 0;

  shield_wall_white(&b, &z, dest);

  moves[*total] = (struct move){orig, dest};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_lower(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.white_r._[0] -= (u64)1 << orig;
  b.white_r._[0] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[0] & ((u64)1 << dest);

  u8 orig_r = rotate_left[orig];
  u8 dest_r = rotate_left[dest];

  OP_LAYER_BIT(b.white, orig_r, -=);
  OP_LAYER_BIT(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.white_r, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.black_r, &b.black, true);
  }

  u64 z = 0;

  shield_wall_white(&b, &z, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_middle(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  // for a center row we should offset by 55 from the start
  orig += 55;
  dest += 55;
  OP_LAYER_BIT(b.white_r, orig, -=);
  OP_LAYER_BIT(b.white_r, dest, |=);

  is_capture = OP_LAYER_BIT(cap_dests, dest, &);

  u8 orig_r = rotate_left[orig];
  u8 dest_r = rotate_left[dest];

  OP_LAYER_BIT(b.white, orig_r, -=);
  OP_LAYER_BIT(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.white_r, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.black_r, &b.black, true);
  }

  u64 z = 0;

  shield_wall_white(&b, &z, dest);

  moves[*total] = (struct move){orig_r, dest_r};
  boards[*total] = b;
  (*total)++;
}

void process_move_white_r_upper(
    const board base_board,
    board *boards,
    move *moves,
    int *total,
    u8 orig,
    u8 dest,
    const layer cap_dests) {
  board b = base_board;
  bool is_capture;

  b.white_r._[1] -= (u64)1 << orig;
  b.white_r._[1] |= (u64)1 << dest;

  // use the pre-offset value to check the capture destinations
  is_capture = cap_dests._[1] & ((u64)1 << dest);

  // we should offset by 64 to get the layer index from the sub-layer
  // index before recording the move and getting the rotated coords.
  // offset_coords(is_upper, 64);
  orig += 64;
  dest += 64;
  u8 orig_r = rotate_left[orig];
  u8 dest_r = rotate_left[dest];

  OP_LAYER_BIT(b.white, orig_r, -=);
  OP_LAYER_BIT(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = LAYER_OR(b.white_r, corners);
    friends = LAYER_OR(friends, b.king);

    apply_captures_niave(friends, &b.black_r, &b.black, true);
  }

  u64 z = 0;

  shield_wall_white(&b, &z, dest);

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
get_next_row_boards_{color}{rotation}_{index}( board *boards, const u64
occ, const board *board, int *total, move *moves, const layer cap_dests, int
row_offset ) {{ unsigned short movers = (board->{color}{rotation}._[{index}] >>
row_offset) & 0b11111111111; while (movers) {{ u8 orig =
_tzcnt_u16(movers); const unsigned short blockers =
      ((u64)occ >> row_offset) & 0b11111111111;
      u64 row_moves =
        (u64)row_moves_table[blockers][orig] << row_offset;
        orig += row_offset;
        while (row_moves) {{
          u8 dest = _tzcnt_u64(row_moves);
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
  unsigned short movers = GET_CENTER_ROW(board->{color}{rotation});
  while (movers) {{
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = GET_CENTER_ROW(occ);
    u16 row_moves = row_moves_table[blockers][orig];
    while (row_moves) {{
      u8 dest = _tzcnt_u16(row_moves);
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
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_black_lower(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_black_1(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_black_upper(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_black_r_0(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black_r._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_black_r_lower(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_black_r_1(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->black_r._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_black_r_upper(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_0(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_white_lower(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_1(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_white_upper(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_r_0(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white_r._[0] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_white_r_lower(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
      row_moves = _blsr_u64(row_moves);
    }
    movers &= movers - 1;
  }
}

static inline __attribute__((always_inline)) void get_next_row_boards_white_r_1(
    board *boards,
    const u64 occ,
    const board *board,
    int *total,
    move *moves,
    const layer cap_dests,
    int row_offset) {
  unsigned short movers = (board->white_r._[1] >> row_offset) & 0b11111111111;
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = ((u64)occ >> row_offset) & 0b11111111111;
    u64 row_moves = (u64)row_moves_table[blockers][orig] << row_offset;
    orig += row_offset;
    while (row_moves) {
      u8 dest = _tzcnt_u64(row_moves);
      process_move_white_r_upper(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
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
  unsigned short movers = GET_CENTER_ROW(board->black);
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = GET_CENTER_ROW(occ);
    u16 row_moves = row_moves_table[blockers][orig];
    while (row_moves) {
      u8 dest = _tzcnt_u16(row_moves);
      if (dest == 5) {
        row_moves &= row_moves - 1;
        continue;
      }
      process_move_black_middle(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
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
  unsigned short movers = GET_CENTER_ROW(board->black_r);
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = GET_CENTER_ROW(occ);
    u16 row_moves = row_moves_table[blockers][orig];
    while (row_moves) {
      u8 dest = _tzcnt_u16(row_moves);
      if (dest == 5) {
        row_moves &= row_moves - 1;
        continue;
      }
      process_move_black_r_middle(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
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
  unsigned short movers = GET_CENTER_ROW(board->white);
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = GET_CENTER_ROW(occ);
    u16 row_moves = row_moves_table[blockers][orig];
    while (row_moves) {
      u8 dest = _tzcnt_u16(row_moves);
      if (dest == 5) {
        row_moves &= row_moves - 1;
        continue;
      }
      process_move_white_middle(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
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
  unsigned short movers = GET_CENTER_ROW(board->white_r);
  while (movers) {
    u8 orig = _tzcnt_u16(movers);
    const unsigned short blockers = GET_CENTER_ROW(occ);
    u16 row_moves = row_moves_table[blockers][orig];
    while (row_moves) {
      u8 dest = _tzcnt_u16(row_moves);
      if (dest == 5) {
        row_moves &= row_moves - 1;
        continue;
      }
      process_move_white_r_middle(
          *board,
          boards,
          moves,
          total,
          orig,
          dest,
          cap_dests);
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

  static const u64 right_barriers = 36046397799139328;
  u64 occ;
  occ |= right_barriers;

  u64 leftward_movers;
  u8 pos = 0;

  while (leftward_movers) {
    // get to next position
    u8 to_next = _tzcnt_u64(leftward_movers) + 1;
    leftward_movers >>= to_next;
    occ >>= to_next;
    pos += to_next;

    // TODO 1: unroll the loops by writing 10 moves forward and then only moving
the total forward by the correct amount
    // TODO 2: have a simd vector of 8 integers of increasing values, and append
the position to each of those in parallel. u8 to_blocker = _tzcnt_u64(occ);
    while (to_blocker) {
      moves[total] = (move){pos, pos + to_blocker};
      to_blocker--;
    }
  }
}
*/

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
  layer allies = LAYER_OR(current.{color}, corners);
  {"allies = LAYER_OR(allies, current.king);" if color == "white" else ""}
  layer allies_r = LAYER_OR(current.{color}_r, corners);
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
    const board current,
    int *total,
    move *moves,
    board *boards) {
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = LAYER_OR(current.white, corners);
  allies = LAYER_OR(allies, current.king);
  layer allies_r = LAYER_OR(current.white_r, corners);
  layer foes = current.black;
  layer foes_r = current.black_r;

  const layer capture_dests = find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
      find_capture_destinations(allies_r, foes_r, occ_r);

  get_next_row_boards_white_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      0);

  get_next_row_boards_white_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      11);

  get_next_row_boards_white_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      22);

  get_next_row_boards_white_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      33);

  get_next_row_boards_white_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      44);

  get_next_row_boards_white_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      2);

  get_next_row_boards_white_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      13);

  get_next_row_boards_white_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      24);

  get_next_row_boards_white_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      35);

  get_next_row_boards_white_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      46);

  get_next_row_boards_white_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      0);

  get_next_row_boards_white_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      11);

  get_next_row_boards_white_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      22);

  get_next_row_boards_white_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      33);

  get_next_row_boards_white_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      44);

  get_next_row_boards_white_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      2);

  get_next_row_boards_white_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      13);

  get_next_row_boards_white_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      24);

  get_next_row_boards_white_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      35);

  get_next_row_boards_white_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      46);

  get_next_row_boards_center_white(
      boards,
      occ,
      &current,
      total,
      moves,
      capture_dests);

  get_next_row_boards_center_white_r(
      boards,
      occ_r,
      &current,
      total,
      moves,
      capture_dests);
}

void get_team_moves_black(
    const board current,
    int *total,
    move *moves,
    board *boards) {
  const layer occ = board_occ(current);
  const layer occ_r = board_occ_r(current);
  layer allies = LAYER_OR(current.black, corners);

  layer allies_r = LAYER_OR(current.black_r, corners);
  layer foes = current.white;
  layer foes_r = current.white_r;

  const layer capture_dests = find_capture_destinations(allies, foes, occ);

  const layer capture_dests_r =
      find_capture_destinations(allies_r, foes_r, occ_r);

  get_next_row_boards_black_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      0);

  get_next_row_boards_black_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      11);

  get_next_row_boards_black_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      22);

  get_next_row_boards_black_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      33);

  get_next_row_boards_black_0(
      boards,
      occ._[0],
      &current,
      total,
      moves,
      capture_dests,
      44);

  get_next_row_boards_black_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      2);

  get_next_row_boards_black_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      13);

  get_next_row_boards_black_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      24);

  get_next_row_boards_black_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      35);

  get_next_row_boards_black_1(
      boards,
      occ._[1],
      &current,
      total,
      moves,
      capture_dests,
      46);

  get_next_row_boards_black_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      0);

  get_next_row_boards_black_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      11);

  get_next_row_boards_black_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      22);

  get_next_row_boards_black_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      33);

  get_next_row_boards_black_r_0(
      boards,
      occ_r._[0],
      &current,
      total,
      moves,
      capture_dests_r,
      44);

  get_next_row_boards_black_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      2);

  get_next_row_boards_black_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      13);

  get_next_row_boards_black_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      24);

  get_next_row_boards_black_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      35);

  get_next_row_boards_black_r_1(
      boards,
      occ_r._[1],
      &current,
      total,
      moves,
      capture_dests_r,
      46);

  get_next_row_boards_center_black(
      boards,
      occ,
      &current,
      total,
      moves,
      capture_dests);

  get_next_row_boards_center_black_r(
      boards,
      occ_r,
      &current,
      total,
      moves,
      capture_dests);
}
//[[[end]]]

void get_king_moves(
    const board current,
    int *total,
    move *moves,
    board *boards) {
  const layer occ = {
      {current.white._[0] | current.black._[0] | current.king._[0],
       current.white._[1] | current.black._[1] | current.king._[1]}};

  // const layer capture_dests = find_capture_destinations_op(current.white,
  // current.black);

  u8 orig = current.king._[0] ? _tzcnt_u64(current.king._[0])
                              : _tzcnt_u64(current.king._[1]) + 64;

  if (orig < 55) {
    const uint row_offset = sub_layer_row_offset[orig];
    const u8 row_orig = orig - row_offset;
    const u16 blockers = ((u64)occ._[0] >> row_offset) & 0x7FF;
    u64 row_moves = (u64)row_moves_table[blockers][row_orig] << row_offset;
    while (row_moves) {
      const u8 dest = _tzcnt_u64(row_moves);
      const u8 dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[0] = (u64)1 << dest;
      new_board.king._[1] = 0;
      new_board.king_r._[!SUB_LAYER(dest_r)] = 0;
      new_board.king_r._[SUB_LAYER(dest_r)] =
          (u64)1 << (sub_layer_offset_direct[dest_r]);
      // handle captures
      // if (capture_dests[0] & (1 << dest)) {
      apply_captures_niave(
          LAYER_OR(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      // }
      u64 z = 0;
      shield_wall_white(&new_board, &z, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else if (orig > 65) {
    const u8 sub_orig = orig - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const u8 row_orig = sub_orig - row_offset;
    const u16 blockers = ((u64)occ._[1] >> row_offset) & 0x7FF;
    u64 row_moves = (u64)row_moves_table[blockers][row_orig] << row_offset;
    while (row_moves) {
      const u8 sub_dest = _tzcnt_u64(row_moves);
      const u8 dest = sub_dest + 64;
      const u8 dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[0] = 0;
      new_board.king._[1] = (u64)1 << sub_dest;
      new_board.king_r._[!SUB_LAYER(dest_r)] = 0;
      new_board.king_r._[SUB_LAYER(dest_r)] =
          (u64)1 << (sub_layer_offset_direct[dest_r]);
      apply_captures_niave(
          LAYER_OR(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      u64 z = 0;
      shield_wall_white(&new_board, &z, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else {
    // center

    u8 local_orig = orig - 55;
    u16 blockers = GET_CENTER_ROW(occ);
    // we can use the normal one because we're the king
    u16 row_moves = row_moves_table[blockers][local_orig];
    while (row_moves) {
      const u8 dest = _tzcnt_u16(row_moves) + 55;
      const u8 dest_r = rotate_right[dest];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[!SUB_LAYER(dest)] = 0;
      new_board.king._[SUB_LAYER(dest)] = (u64)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king_r._[!SUB_LAYER(dest_r)] = 0;
      new_board.king_r._[SUB_LAYER(dest_r)] =
          (u64)1 << (sub_layer_offset_direct[dest_r]);
      // handle captures
      // if (capture_dests[1] & (1 << sub_dest)) {
      apply_captures_niave(
          LAYER_OR(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      // }
      u64 z = 0;
      shield_wall_white(&new_board, &z, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  }

  const layer occ_r = {
      {current.white_r._[0] | current.black_r._[0] | current.king_r._[0],
       current.white_r._[1] | current.black_r._[1] | current.king_r._[1]}};

  u8 orig_r = rotate_right[orig];

  if (orig_r < 55) {
    const uint row_offset = sub_layer_row_offset[orig_r];
    const u8 row_orig = orig_r - row_offset;
    const u16 blockers = ((u64)occ_r._[0] >> row_offset) & 0x7FF;
    u64 row_moves = (u64)row_moves_table[blockers][row_orig] << row_offset;
    while (row_moves) {
      const u8 dest_r = _tzcnt_u64(row_moves);
      const u8 dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[SUB_LAYER(dest)] = (u64)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king._[!SUB_LAYER(dest)] = 0;
      new_board.king_r._[0] = (u64)1 << dest_r;
      new_board.king_r._[1] = 0;
      // handle captures
      // if (capture_dests[0] & (1 << dest)) {
      apply_captures_niave(
          LAYER_OR(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      // }
      u64 z = 0;
      shield_wall_white(&new_board, &z, dest);
      // APPLY_CAPTURES_king;
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else if (orig_r > 65) {
    const u8 sub_orig = orig_r - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const u8 row_orig = sub_orig - row_offset;
    const u16 blockers = ((u64)occ_r._[1] >> row_offset) & 0x7FF;
    u64 row_moves = (u64)row_moves_table[blockers][row_orig] << row_offset;
    while (row_moves) {
      const u8 sub_dest = _tzcnt_u64(row_moves);
      const u8 dest_r = sub_dest + 64;
      const u8 dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[!SUB_LAYER(dest)] = 0;
      new_board.king._[SUB_LAYER(dest)] = (u64)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king_r._[0] = 0;
      new_board.king_r._[1] = (u64)1 << sub_dest;
      apply_captures_niave(
          LAYER_OR(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      u64 z = 0;
      shield_wall_white(&new_board, &z, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  } else {
    // center

    u8 local_orig = orig_r - 55;
    u16 blockers = GET_CENTER_ROW(occ_r);
    // we can use the normal one because we're the king
    u16 row_moves = row_moves_table[blockers][local_orig];
    while (row_moves) {
      const u8 dest_r = _tzcnt_u16(row_moves) + 55;
      const u8 dest = rotate_left[dest_r];
      // register move
      moves[*total] = (struct move){orig, dest};
      // generate board
      board new_board = current;
      new_board.king._[!SUB_LAYER(dest)] = 0;
      new_board.king._[SUB_LAYER(dest)] = (u64)1
                                          << (sub_layer_offset_direct[dest]);
      new_board.king_r._[!SUB_LAYER(dest_r)] = 0;
      new_board.king_r._[SUB_LAYER(dest_r)] =
          (u64)1 << (sub_layer_offset_direct[dest_r]);

      apply_captures_niave(
          LAYER_OR(new_board.white, corners),
          &new_board.black,
          &new_board.black_r,
          dest);
      u64 z = 0;
      shield_wall_white(&new_board, &z, dest);
      // bookkeep
      boards[*total] = new_board;
      (*total)++;
      row_moves = _blsr_u64(row_moves);
    }
  }
}

/* Generate a layer of locations which, when landed upon, _may_
trigger a shield wall capture.

Specifically, this generates a layer of open squares at the edge which
are adjacent to foe pieces , which themselves are adjacent to an
opposing pieces in the inner row.

The idea is not to be perfectly accurate, but rather to rule out a
majority of ineligible edge positions via a relatively cheap "bulk"
computation, so that not every move to an edge position need do a full
shield wall check. As such, the resulting layer may contain false
positives, but may not contain false negatives, as the latter would
never be checked and thus go undiscovered.
*/
layer gen_shield_wall_triggers(
    const layer allies,
    const layer foes,
    const layer occ) {
  layer triggers = EMPTY_LAYER;

  // north
  {
    u64 edges = foes._[1] & (allies._[1] << 11);
    u64 left = occ._[1] & (edges << 1);
    u64 right = occ._[1] & (edges >> 1);
    triggers._[1] |= left | right;
  }

  // south
  {
    u64 edges = foes._[0] & (allies._[0] >> 11);
    u64 left = occ._[0] & (edges << 1);
    u64 right = occ._[0] & (edges >> 1);
    triggers._[0] |= left | right;
  }

  // east
  {
    layer edges = LAYER_AND(foes, LAYER_SHIFTR(allies, 1));
    layer up = LAYER_AND(occ, LAYER_SHIFTL_SHORT(edges, 11));
    layer down = LAYER_AND(occ, LAYER_SHIFTR(edges, 11));
    triggers = LAYER_OR(triggers, LAYER_OR(up, down));
  }

  // west
  {
    layer edges = LAYER_AND(foes, LAYER_SHIFTL_SHORT(allies, 1));
    layer up = LAYER_AND(occ, LAYER_SHIFTL_SHORT(edges, 11));
    layer down = LAYER_AND(occ, LAYER_SHIFTR(edges, 11));
    triggers = LAYER_OR(triggers, LAYER_OR(up, down));
  }

  // only at the end do we apply the edge mask.
  triggers = LAYER_AND(triggers, EDGES);

  return triggers;
}

void gen_reference_moves_black3(
    const board b,
    int *total,
    move *ms,
    board *bs) {
  *total = 0;
  layer occ = board_occ(b);

  const layer capture_dests = find_capture_destinations(b.black, b.white, occ);
  const layer shield_dests = gen_shield_wall_triggers(b.black, b.white, occ);

  int dest;

  int orig = 0;
  u64 pieces = b.black._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = RANK(orig);
    int remaining_north = 10 - rank;
    while (remaining_north--) {
      dest += 11;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.black, orig, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.black, dest, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_black(&b2, &z, dest);
      }

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // south
    dest = orig;
    // rank = RANK(orig);
    while (rank--) {
      dest -= 11;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.black, orig, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.black, dest, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_black(&b2, &z, dest);
      }

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // west
    dest = orig;
    int file = FILE(orig);
    int remaining_south = 10 - file;
    while (remaining_south--) {
      dest += 1;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.black, orig, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.black, dest, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_black(&b2, &z, dest);
      }

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // east
    dest = orig;
    // file = FILE(orig);
    while (file--) {
      dest -= 1;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.black, orig, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.black, dest, |=);
      OP_LAYER_BIT(b2.black_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.black, &b2.white, &b2.white_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_black(&b2, &z, dest);
      }

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
    const board b,
    int *total,
    move *ms,
    board *bs) {
  *total = 0;
  layer occ = board_occ(b);

  const layer capture_dests = find_capture_destinations(b.white, b.black, occ);
  const layer shield_dests = gen_shield_wall_triggers(b.white, b.black, occ);

  int dest;

  int orig = 0;
  u64 pieces = b.white._[0];
  bool lower = true;

process:
  while (pieces) {
    int to_next = _tzcnt_u64(pieces);
    orig += to_next;

    // north
    dest = orig;
    int rank = RANK(orig);
    int remaining_north = 10 - rank;
    while (remaining_north--) {
      dest += 11;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.white, orig, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.white, dest, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_white(&b2, &z, dest);
      }

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // south
    dest = orig;
    // rank = RANK(orig);
    while (rank--) {
      dest -= 11;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.white, orig, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.white, dest, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_white(&b2, &z, dest);
      }

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // west
    dest = orig;
    int file = FILE(orig);
    int remaining_south = 10 - file;
    while (remaining_south--) {
      dest += 1;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.white, orig, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.white, dest, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_white(&b2, &z, dest);
      }

      bs[(*total)] = b2;
      ms[(*total)] = (move){orig, dest};
      (*total)++;
    }

    // east
    dest = orig;
    // file = FILE(orig);
    while (file--) {
      dest -= 1;
      if (CHECK_INDEX(occ, dest))
        break;
      if (dest == 60)
        continue;

      board b2 = b;
      OP_LAYER_BIT(b2.white, orig, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[orig], |=);
      OP_LAYER_BIT(b2.white, dest, |=);
      OP_LAYER_BIT(b2.white_r, rotate_right[dest], |=);

      if (CHECK_INDEX(capture_dests, dest))
        apply_captures_niave(b2.white, &b2.black, &b2.black_r, dest);

      if (CHECK_INDEX(shield_dests, dest)) {
        u64 z = 0;
        shield_wall_white(&b2, &z, dest);
      }

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

// inline __attribute__((always_inline)) int get_king_move_count(const board
// b)
// {
int get_king_move_count(const board b) {
  int total = 0;

  const layer occ = king_board_occ(b);

  if (b.king._[0] & LOWER_HALF_MASK) {
    int orig = _tzcnt_u64(b.king._[0]);
    const uint row_offset = sub_layer_row_offset[orig];
    const u8 row_orig = orig - row_offset;
    const u16 blockers = ((u64)occ._[0] >> row_offset) & 0x7FF;
    // total += row_move_count_table[blockers][row_orig];
    total += __builtin_popcount(get_row_moves(blockers, row_orig));
  } else if (b.king._[1] & UPPER_HALF_MASK) {
    int orig = _tzcnt_u64(b.king._[1]);
    const uint row_offset = sub_layer_row_offset_upper[orig];
    const u8 row_orig = orig - row_offset;
    const u16 blockers = ((u64)occ._[1] >> row_offset) & 0x7FF;
    // total += row_move_count_table[blockers][row_orig];
    total += __builtin_popcount(get_row_moves(blockers, row_orig));
  } else {
    u8 orig = _tzcnt_u16(GET_CENTER_ROW(b.king));
    u16 blockers = GET_CENTER_ROW(occ);
    // total += row_move_count_table[blockers][local_orig];
    total += __builtin_popcount(get_row_moves(blockers, orig));
  }

  const layer occ_r = king_board_occ_r(b);

  u8 orig_r = 67;

  if (orig_r < 55) {
    const uint row_offset = sub_layer_row_offset[orig_r];
    const u8 row_orig = orig_r - row_offset;
    const u16 blockers = ((u64)occ_r._[0] >> row_offset) & 0x7FF;
    // total += row_move_count_table[blockers][row_orig];
    total += __builtin_popcount(get_row_moves(blockers, row_orig));
  } else if (orig_r > 65) {
    const u8 sub_orig = orig_r - 64;
    const uint row_offset = sub_layer_row_offset_upper[sub_orig];
    const u8 row_orig = sub_orig - row_offset;
    const u16 blockers = ((u64)occ_r._[1] >> row_offset) & 0x7FF;
    // total += row_move_count_table[blockers][row_orig];
    total += __builtin_popcount(get_row_moves(blockers, row_orig));
  } else {
    // center
    const u8 local_orig = orig_r - 55;
    const u16 blockers = GET_CENTER_ROW(occ_r);
    // total += row_move_count_table[blockers][local_orig];
    total += __builtin_popcount(get_row_moves(blockers, local_orig));
  }
  return total;
}

/* I feel like this should be faster than the newer paradigm one but it's
 * somehow the same to slightly slower */
int king_moves_count2(const board *b) {
  int total = 0;
  layer occ = king_board_occ(*b);
  layer occ_r = king_board_occ_r(*b);
  int king_pos = LOWEST_INDEX(b->king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);
  total +=
      __builtin_popcount(get_row_moves(get_row(occ, king_rank), king_file));
  total += __builtin_popcount(
      get_row_moves(get_row(occ_r, king_file), 10 - king_rank));
  return total;
}
