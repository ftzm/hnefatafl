#pragma once

#include "stdint.h"



#define u8 uint8_t
#define u16 uint16_t
#define u32 uint32_t
#define u64 uint64_t

typedef struct layer {
  uint64_t _[2];
} layer;

#define EMPTY_LAYER (layer){._={0,0}}

// extern const uint8_t sub_layer[121];

extern const uint8_t sub_layer_offset_direct[121];


/**
 * This lookup table only contains 55 elements as positions above 55
 * should be handled separately, being split between two layers.
 */
static const uint8_t sub_layer_row_offset[55] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
  22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
  33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
  44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44
};

/**
 * the first two indices should not be used, as these represent
 * squares of a row split between both halves which need to be handled
 * separately. they're only here so that the upper element index
 * numbers are correct.
 */
static const uint8_t sub_layer_row_offset_upper[57] = {
  0, 0,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
  46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46
};

layer rotate_layer_right(const layer input);
layer rotate_layer_left(const layer input);

extern const uint8_t sub_layer_table[121];

extern const uint8_t rotate_right[121];

extern const uint8_t rotate_left[121];

// #define sub_layer(i) (i > 63)
// benchmarks suggest the lookup table is actually faster
#define sub_layer(i) (sub_layer_table[i])

#define op_layer_bit(l, b, op) (l._[sub_layer(b)] op ((uint64_t) 1 << sub_layer_offset_direct[b]))
#define op_layer_bit_ptr(l, b, op) (l->_[sub_layer(b)] op ((uint64_t) 1 << sub_layer_offset_direct[b]))

#define layer_bin_op(a, b, op) ((layer) {._ = {a._[0] op b._[0], a._[1] op b._[1]}})

//#define layer_or(a, b) ((layer) {._ = {a._[0] | b._[0], a._[1] | b._[1]}})
#define layer_or(a, b) layer_bin_op(a, b, |)

//#define layer_and(a, b) ((layer) {._ = {a._[0] & b._[0], a._[1] & b._[1]}})
#define layer_and(a, b) layer_bin_op(a, b, &)

//#define layer_xor(a, b) ((layer) {._ = {a._[0] ^ b._[0], a._[1] ^ b._[1]}})
#define layer_xor(a, b) layer_bin_op(a, b, ^)

#define layer_neg(a) ((layer) {._ = {~a._[0], ~a._[1]}})

// can only be called with n > 0 && n < 65
#define layer_shiftl(l, n) \
((layer) {l._[0] << n, (l._[1] << n) | (l._[0] >> (64 - n))})

// can only be called with n > 0 && n < 65
#define layer_shiftr(l, n) \
((layer) {(l._[0] >> n) | (l._[1] << (64 - n)), l._[1] >> n})

#define check_index(layer, i) \
  (layer._[sub_layer(i)] & ((uint64_t)1 << sub_layer_offset_direct[i]))

#define toggle_index(layer, i) \
  layer._[sub_layer[i]] |= ((uint64_t)1 << sub_layer_offset_direct[i])

extern layer corners;

#define get_center_row(layer) (((uint64_t)layer._[0] >> 55) | ((((uint64_t)layer._[1] & 0x3) << 9) & 0b11111111111))
#define SET_CENTER_ROW(_layer, _row) (_layer._[0] |= ((uint64_t)_row << 55), _layer._[1] |= (_row >> 9))

#define INVERTED_THRONE_MASK ((uint16_t) 0b11111011111)

#define LAYERS_EQUAL(a, b) (a._[0] == b._[0] && a._[1] == b._[1])

#define NOT_EMPTY(l) (l._[0] | l._[1])
#define IS_EMPTY(l) (!(NOT_EMPTY(l)))


extern const uint8_t rank_table[121];

#define rank(_a) (rank_table[_a])

extern const uint8_t file_table[121];

#define file(_a) (file_table[_a])

static const layer EDGES = {54069596698710015ULL, 144080055268552710ULL};

#define LAYER_OR_ASSG(_x, _y) _x._[0] |= _y._[0]; _x._[1] |= _y._[1]

#define lowest_index(layer)                                                    \
  (layer._[0] ? _tzcnt_u64(layer._[0]) : _tzcnt_u64(layer._[1]) + 64)

#define LOWER_HALF_MASK ((uint64_t) 36028797018963967ULL)
#define UPPER_HALF_MASK ((uint64_t) 144115188075855868ULL)
