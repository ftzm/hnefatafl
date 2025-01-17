#pragma once

#include "layer.cpp"
#include "board.cpp"

inline layer foe_masks[120];
inline layer foe_masks_r[120];
inline layer ally_masks[120];
inline layer ally_masks_r[120];
inline layer surround_masks[120];
inline layer surround_masks_r[120];

//******************************************************************************

inline void gen_foe_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 11;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 21) {
      target = i - 11;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 1;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 1;
      foe_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      foe_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
  }
}


inline void gen_surround_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 110) {
      target = i + 11;
      surround_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      surround_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 10) {
      target = i - 11;
      surround_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      surround_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 10) {
      target = i + 1;
      surround_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      surround_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 0) {
      target = i - 1;
      surround_masks[i][sub_layer[target]] |=
          ((uint64_t)1 << (target - sub_layer_offset[target]));
      target_r = rotate_right[target];
      surround_masks_r[i][sub_layer[target_r]] |=
          ((uint64_t)1 << sub_layer_offset_direct[target_r]);
    }
  }
}


inline void gen_ally_masks() {
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

inline __attribute__((always_inline)) uint8_t
apply_captures_niave_count(const layer friends, layer &foes, layer &foes_r, int dest) {
  uint8_t count = 0;

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
      foes[sub_layer[target]] -= ((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      count++;
  }

  //southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 21 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      count++;
  }

  //westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 9 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      count++;
  }
   
  //eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 1 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      foes[sub_layer[target]] -= ((uint64_t) 1 << sub_layer_offset_direct[target]);
      int target_r = rotate_right[target];
      foes_r[sub_layer[target_r]] -= ((uint64_t) 1 << sub_layer_offset_direct[target_r]);
      count++;
  }

  return count;
}
//******************************************************************************
// Capture functions

#define foes_dir(is_rotated) (is_rotated ? foes_r : foes)
#define allies_dir(is_rotated) (is_rotated ? allies_r : allies)
#define rotate_table_dir(is_rotated) (is_rotated ? rotate_left : rotate_right)

//******************************************************************************
// Components

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
  // TODO: check that this is maximally efficient
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
  // TODO: check that this is maximally efficient
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
  // TODO: check that this is maximally efficient
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
  // TODO: check that this is maximally efficient
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

template <bool is_black, bool left, bool right, int sub_index, bool is_rotated>
void capture_s_edge(board *b, unsigned char pos) {

  layer &allies = is_black ? b->black : b->white;
  layer &allies_r = is_black ? b->black_r : b->white_r;
  layer &foes = is_black ? b->white : b->black;
  layer &foes_r = is_black ? b->white_r : b->black_r;

  if constexpr (is_rotated) {
    pos = rotate_right[pos];
  }

  const uint64_t sub_allies = (is_rotated ? allies_r : allies)[sub_index] | corners[sub_index];
  uint64_t sub_foes = (is_rotated ? foes_r : foes)[sub_index];

  // add king to foes if foes are white
  if constexpr (is_black) {
    sub_foes |= (is_rotated ? b->king_r : b->king)[sub_index];
  }
  uint64_t captures = 0;

  if constexpr (left && !sub_index) {
    lower_left_shield_captures(sub_allies, sub_allies >> 11, sub_foes, pos,
                               &captures);
  }
  if constexpr (right && !sub_index) {
    lower_right_shield_captures(sub_allies, sub_allies >> 11, sub_foes, pos,
                                &captures);
  }
  if constexpr (left && sub_index) {
    upper_left_shield_captures(sub_allies, sub_foes, pos, &captures);
  }
  if constexpr (left && sub_index) {
    upper_right_shield_captures(sub_allies, sub_foes, pos, &captures);
  }

  // remove king from captures if it's there
  if constexpr (is_black) {
    captures &= ~(is_rotated ? b->king_r : b->king)[sub_index];
  }

  foes_dir(is_rotated)[sub_index] -= captures;
  if constexpr (sub_index) {
    distribute_rotated<is_rotated, 64>(captures, foes_dir(!is_rotated));
  } else {
    distribute_rotated<is_rotated, 0>(captures, foes_dir(!is_rotated));
  }
}

template <int sub_index, int sub_offset, bool is_rotated>
void capture_edge(const layer &allies, const layer &allies_r, layer &foes,
                  layer &foes_r, unsigned char pos) {
  if constexpr (is_rotated) {
    pos = rotate_right[pos];
  }
  uint64_t captures = half_captures<sub_index>(allies_dir(is_rotated),
					       foes_dir(is_rotated), pos);

  foes_dir(is_rotated)[sub_index] -= captures;
  distribute_rotated<is_rotated, sub_offset>(captures, foes_dir(!is_rotated));
}


#define capture_s_s (capture_s_edge<true, true, 0, 0, false>)
#define capture_s_se (capture_s_edge<true, false, 0, 0, false>)
#define capture_s_sw (capture_s_edge<false, true, 0, 0, false>)
#define capture_s_e (capture_s_edge<true, true, 0, 0, true>)
#define capture_s_en (capture_s_edge<false, true, 0, 0, true>)
#define capture_s_es (capture_s_edge<true, false, 0, 0, true>)
#define capture_s_n (capture_s_edge<true, true, 1, 64, false>)
#define capture_s_ne (capture_s_edge<true, true, 1, 64, false>)
#define capture_s_nw (capture_s_edge<true, true, 1, 64, false>)
#define capture_s_w (capture_s_edge<true, true, 1, 64, true>)
#define capture_s_wn (capture_s_edge<false, true, 1, 64, true>)
#define capture_s_ws (capture_s_edge<true, false, 1, 64, true>)

template <bool is_black>
void shield_wall(board *b, uint pos) {
  switch (pos) {
  // south
  case 1:
  case 2:
    capture_s_edge<is_black, true, false, 0, false>(b, pos);
    break;
  case 3:
  case 4:
  case 5:
  case 6:
  case 7:
    capture_s_edge<is_black, true, true, 0, false>(b, pos);
    break;
  case 8:
  case 9:
    capture_s_edge<is_black, false, true, 0, false>(b, pos);
    break;

  // north
  case 111:
  case 112:
    capture_s_edge<is_black, true, false, 1, false>(b, pos);
    break;
  case 113:
  case 114:
  case 115:
  case 116:
  case 117:
    capture_s_edge<is_black, true, true, 1, false>(b, pos);
    break;
  case 118:
  case 119:
    capture_s_edge<is_black, true, true, 1, false>(b, pos);
    break;

  // east
  case 11:
  case 22:
    capture_s_edge<is_black, true, false, 0, true>(b, pos);
    break;
  case 33:
  case 44:
  case 55:
  case 66:
  case 77:
    capture_s_edge<is_black, true, true, 0, true>(b, pos);
    break;
  case 88:
  case 99:
    capture_s_edge<is_black, false, true, 0, true>(b, pos);
    break;

  // west
  case 21:
  case 32:
    capture_s_edge<is_black, true, false, 1, true>(b, pos);
    break;
  case 43:
  case 54:
  case 65:
  case 76:
  case 87:
    capture_s_edge<is_black, true, true, 1, true>(b, pos);
    break;
  case 98:
  case 109:
    capture_s_edge<is_black, false, true, 1, true>(b, pos);
    break;

  // not an edge square
  default:
    break;
  }
  // 
}
