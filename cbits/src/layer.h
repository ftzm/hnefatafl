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
#define LAYER_OR(a, b) LAYER_BIN_OP(a, b, |)

// #define layer_and(a, b) ((layer) {._ = {a._[0] & b._[0], a._[1] & b._[1]}})
#define LAYER_AND(a, b) LAYER_BIN_OP(a, b, &)

// #define layer_xor(a, b) ((layer) {._ = {a._[0] ^ b._[0], a._[1] ^ b._[1]}})
#define LAYER_XOR(a, b) LAYER_BIN_OP(a, b, ^)

#define LAYER_NEG(a) ((layer){._ = {~a._[0], ~a._[1]}})

// can only be called with n > 0 && n < 65
#define LAYER_SHIFTL_SHORT(l, n)                                               \
  ((layer){l._[0] << n, (l._[1] << n) | (l._[0] >> (64 - n))})

// can only be called with n > 0 && n < 65
#define LAYER_SHIFTR(l, n)                                                     \
  ((layer){(l._[0] >> n) | (l._[1] << (64 - n)), l._[1] >> n})

// can be called with any value, though we assume it doesn't exceed (-)128
inline layer layer_shift(layer l, int n) {
  if (n > 64) {
    return (layer) { 0, l._[0] << (n - 64) };
  } else if (n > 0) {
    return LAYER_SHIFTL_SHORT(l, n);
  } else if (n < -64) {
    return (layer) {l._[1] >> -(n + 64), 0};
  } else if (n < 0) {
    return LAYER_SHIFTR(l, n);
  } else {
    return l;
  }
}

#define CHECK_INDEX(layer, i)                                                  \
  (layer._[SUB_LAYER(i)] & ((u64)1 << sub_layer_offset_direct[i]))

#define SET_INDEX(layer, i)                                                    \
  layer._[SUB_LAYER(i)] |= ((u64)1 << sub_layer_offset_direct[i])

#define GET_CENTER_ROW(layer)                                                  \
  (((u64)layer._[0] >> 55) |                                              \
   ((((u64)layer._[1] & 0x3) << 9) & 0b11111111111))
#define SET_CENTER_ROW(_layer, _row)                                           \
  (_layer._[0] |= ((u64)_row << 55), _layer._[1] |= (_row >> 9))

#define INVERTED_THRONE_MASK ((u16)0b11111011111)

#define LAYERS_EQUAL(a, b) (a._[0] == b._[0] && a._[1] == b._[1])

#define NOT_EMPTY(l) (l._[0] | l._[1])
#define IS_EMPTY(l) (!(NOT_EMPTY(l)))

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

#define LOWEST_INDEX(layer)                                                    \
  (layer._[0] ? _tzcnt_u64(layer._[0]) : _tzcnt_u64(layer._[1]) + 64)

#define LOWER_HALF_MASK ((u64)36028797018963967ULL)
#define UPPER_HALF_MASK ((u64)144115188075855868ULL)

u16 dirty_get_row(layer l, int n);
