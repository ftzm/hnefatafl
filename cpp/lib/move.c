#include "board.c"
#include "layer.h"
#include "capture.c"

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

const uint64_t inverted_throne_mask = 0b11111011111;

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

/*

template <bool is_black, bool is_rotated, bool is_lower, bool is_upper>
inline __attribute__((always_inline)) void
process_move(const board base_board, board *boards, move *moves, uint8_t *cap_counts,
                  int *total, uint8_t orig, uint8_t dest, const layer cap_dests) {

  bool is_capture;

  // if neither upper nor lower then this is a center row, and we should offset
  // by 55 from the start
  // offset_coords(!is_lower && !is_upper, 55);
  if constexpr (!is_lower && !is_upper) {
    orig += 55; dest += 55;
    board_layer(is_black, is_rotated)[sub_layer[orig]] -=
        (uint64_t)1 << sub_layer_offset_direct[orig];
    board_layer(is_black, is_rotated)[sub_layer[dest]] |=
        (uint64_t)1 << sub_layer_offset_direct[dest];

    is_capture = op_layer_bit(cap_dests, dest, &);

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
    orig += 64; dest += 64;
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

  shield_wall<is_black>(b, (is_rotated ? dest_r : dest));

  moves[*total] =
      (struct move){(is_rotated ? orig_r : orig), (is_rotated ? dest_r : dest)};
  boards[*total] = b;
  (*total)++;
}
*/

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

  op_layer_bit(b.{color}{rotation}, orig_r, -=);
  op_layer_bit(b.{color}{rotation}, dest_r, |=);

  if (is_capture) {{
    layer friends = layer_or(b.{color}{rotation}, corners);
    {"friends = layer_or(friends, b.king);" if is_black else ""} 

    cap_counts[*total] = apply_captures_niave_count(
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

for (is_black, is_rotated, level) in product(["black", "white"], ["", "_r"], levels):
    cog.outl(build_func(is_black, is_rotated, level))



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

  op_layer_bit(b.black, orig_r, -=);
  op_layer_bit(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.black, orig_r, -=);
  op_layer_bit(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.black, orig_r, -=);
  op_layer_bit(b.black, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.black_r, orig_r, -=);
  op_layer_bit(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black_r, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.black_r, orig_r, -=);
  op_layer_bit(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black_r, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.black_r, orig_r, -=);
  op_layer_bit(b.black_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.black_r, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.white, orig_r, -=);
  op_layer_bit(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.white, orig_r, -=);
  op_layer_bit(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.white, orig_r, -=);
  op_layer_bit(b.white, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.white_r, orig_r, -=);
  op_layer_bit(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white_r, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.white_r, orig_r, -=);
  op_layer_bit(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white_r, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

  op_layer_bit(b.white_r, orig_r, -=);
  op_layer_bit(b.white_r, dest_r, |=);

  if (is_capture) {
    layer friends = layer_or(b.white_r, corners);
    friends = layer_or(friends, b.king); 

    cap_counts[*total] = apply_captures_niave_count(
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

    // TODO 1: unroll the loops by writing 10 moves forward and then only moving the total forward by the correct amount
    // TODO 2: have a simd vector of 8 integers of increasing values, and append the position to each of those in parallel.
    uint8_t to_blocker = _tzcnt_u64(occ);
    while (to_blocker) {
      moves[total] = (move){pos, pos + to_blocker};
      to_blocker--;
    }
  }
}
