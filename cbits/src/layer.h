#pragma once

#include "util.h"

typedef struct layer {
  u64 _[2];
} layer;

#define EMPTY_LAYER                                                            \
  (layer) {                                                                    \
    ._ = { 0, 0 }                                                              \
  }

// extern const u8 sub_layer[121];

extern const u8 sub_layer_offset_direct[121];

/**
 * This lookup table only contains 55 elements as positions above 55
 * should be handled separately, being split between two layers.
 */
static const u8 sub_layer_row_offset[55] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 33, 33, 33, 33, 33,
    33, 33, 33, 33, 33, 33, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44};

/**
 * the first two indices should not be used, as these represent
 * squares of a row split between both halves which need to be handled
 * separately. they're only here so that the upper element index
 * numbers are correct.
 */
static const u8 sub_layer_row_offset_upper[57] = {
    0,  0,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 35, 35, 35,
    35, 35, 35, 35, 35, 35, 35, 35, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46};

layer rotate_layer_right(const layer input);
layer rotate_layer_left(const layer input);

extern const u8 sub_layer_table[121];

extern const u8 rotate_right[121];

extern const u8 rotate_left[121];

// #define sub_layer(i) (i > 63)
// benchmarks suggest the lookup table is actually faster
#define SUB_LAYER(i) (sub_layer_table[i])

#define OP_LAYER_BIT(l, b, op)                                                 \
  (l._[SUB_LAYER(b)] op((u64)1 << sub_layer_offset_direct[b]))
#define OP_LAYER_BIT_PTR(l, b, op)                                             \
  (l->_[SUB_LAYER(b)] op((u64)1 << sub_layer_offset_direct[b]))

#define LAYER_BIN_OP(a, b, op)                                                 \
  ((layer){._ = {a._[0] op b._[0], a._[1] op b._[1]}})

// #define layer_or(a, b) ((layer) {._ = {a._[0] | b._[0], a._[1] | b._[1]}})
#define LAYER_OR(_a, _b) LAYER_BIN_OP(_a, _b, |)

// #define layer_and(a, b) ((layer) {._ = {a._[0] & b._[0], a._[1] & b._[1]}})
#define LAYER_AND(_a, _b) LAYER_BIN_OP(_a, _b, &)

// #define layer_xor(a, b) ((layer) {._ = {a._[0] ^ b._[0], a._[1] ^ b._[1]}})
#define LAYER_XOR(_a, _b) LAYER_BIN_OP(_a, _b, ^)

#define LAYER_NEG(_a) ((layer){._ = {~_a._[0], ~_a._[1]}})

#define LAYER_NOT(_a) LAYER_NEG(_a)

// can only be called with n > 0 && n < 65
#define LAYER_SHIFTL_SHORT(_l, _n)                                             \
  ((layer){_l._[0] << _n, (_l._[1] << _n) | (_l._[0] >> (64 - _n))})

// can only be called with n > 0 && n < 65
#define LAYER_SHIFTR(_l, _n)                                                   \
  ((layer){(_l._[0] >> _n) | (_l._[1] << (64 - _n)), _l._[1] >> _n})

// can be called with any value, though we assume it doesn't exceed (-)128
inline layer layer_shift(layer _l, int _n) {
  if (_n > 64) {
    return (layer){0, _l._[0] << (_n - 64)};
  } else if (_n > 0) {
    return LAYER_SHIFTL_SHORT(_l, _n);
  } else if (_n < -64) {
    return (layer){_l._[1] >> -(_n + 64), 0};
  } else if (_n < 0) {
    return LAYER_SHIFTR(_l, _n);
  } else {
    return _l;
  }
}

#define CHECK_INDEX(_l, _i)                                                    \
  (_l._[SUB_LAYER(_i)] & ((u64)1 << sub_layer_offset_direct[_i]))

#define SET_INDEX(_l, _i)                                                      \
  _l._[SUB_LAYER(_i)] |= ((u64)1 << sub_layer_offset_direct[_i])

#define CLEAR_INDEX(_l, _i)                                                    \
  _l._[SUB_LAYER(_i)] &= ~((u64)1 << sub_layer_offset_direct[_i])

#define TOGGLE_INDEX(_l, _i)                                                   \
  _l._[SUB_LAYER(_i)] ^= ((u64)1 << sub_layer_offset_direct[_i])

#define CHECK_INDEX_PTR(_l, _i)                                                \
  (_l->_[SUB_LAYER(_i)] & ((u64)1 << sub_layer_offset_direct[_i]))

#define SET_INDEX_PTR(_l, _i)                                                  \
  _l->_[SUB_LAYER(_i)] |= ((u64)1 << sub_layer_offset_direct[_i])

#define CLEAR_INDEX_PTR(_l, _i)                                                \
  _l->_[SUB_LAYER(_i)] &= ~((u64)1 << sub_layer_offset_direct[_i])

#define TOGGLE_INDEX_PTR(_l, _i)                                               \
  _l->_[SUB_LAYER(_i)] ^= ((u64)1 << sub_layer_offset_direct[_i])

#define GET_CENTER_ROW(_l)                                                     \
  (((u64)_l._[0] >> 55) | ((((u64)_l._[1] & 0x3) << 9) & 0b11111111111))
#define SET_CENTER_ROW(_layer, _row)                                           \
  (_layer._[0] |= ((u64)_row << 55), _layer._[1] |= (_row >> 9))

#define INVERTED_THRONE_MASK ((u16)0b11111011111)

#define LAYERS_EQUAL(_a, _b) (_a._[0] == _b._[0] && _a._[1] == _b._[1])

#define NOT_EMPTY(_l) (_l._[0] | _l._[1])
#define IS_EMPTY(_l) (!(NOT_EMPTY(_l)))

extern const u8 rank_table[121];

#define RANK(_a) (rank_table[_a])

extern const u8 file_table[121];

#define FILE(_a) (file_table[_a])

static const layer EDGES = {54069596698710015ULL, 144080055268552710ULL};

#define LAYER_OR_ASSG(_x, _y)                                                  \
  _x._[0] |= _y._[0];                                                          \
  _x._[1] |= _y._[1]
#define LAYER_OR_ASSG_PTR(_x, _y)                                              \
  _x->_[0] |= _y._[0];                                                         \
  _x->_[1] |= _y._[1]

#define LAYER_AND_ASSG(_x, _y)                                                 \
  _x._[0] &= _y._[0];                                                          \
  _x._[1] &= _y._[1]
#define LAYER_AND_ASSG_PTR(_x, _y)                                             \
  _x->_[0] &= _y._[0];                                                         \
  _x->_[1] &= _y._[1]

#define LAYER_XOR_ASSG(_x, _y)                                                 \
  _x._[0] ^= _y._[0];                                                          \
  _x._[1] ^= _y._[1]
#define LAYER_XOR_ASSG_PTR(_x, _y)                                             \
  _x->_[0] ^= _y._[0];                                                         \
  _x->_[1] ^= _y._[1]

#define LOWEST_INDEX(_l)                                                       \
  (_l._[0] ? _tzcnt_u64(_l._[0]) : _tzcnt_u64(_l._[1]) + 64)

#define LAYER_POPCOUNT(_l)                                                     \
  (__builtin_popcountll(_l._[0]) + __builtin_popcountll(_l._[1]))

#define LAYER_NAND(_a, _b) LAYER_NEG(LAYER_AND(_a, _b))

#define LAYER_NOR(_a, _b) LAYER_NEG(LAYER_OR(_a, _b))

#define LAYER_NEG_ASSG(_x)                                                     \
  _x._[0] = ~_x._[0];                                                          \
  _x._[1] = ~_x._[1]

#define LOWER_HALF_MASK ((u64)36028797018963967ULL)
#define UPPER_HALF_MASK ((u64)144115188075855868ULL)

u16 dirty_get_row(layer l, int n);
u16 get_row(layer l, int n);

#define MAP_INDICES(_l, _f)                                                    \
  while (_l._[0]) {                                                            \
    int i = _tzcnt_u64(_l._[0]);                                               \
    _f;                                                                        \
    _l._[0] = _blsr_u64(_l._[0]);                                              \
  }                                                                            \
  while (_l._[1]) {                                                            \
    int i = 64 + _tzcnt_u64(_l._[1]);                                          \
    _f;                                                                        \
    _l._[1] = _blsr_u64(_l._[1]);                                              \
  }
