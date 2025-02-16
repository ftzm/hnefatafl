#pragma once

#include "stdint.h"

typedef struct layer {
  uint64_t _[2];
} layer;

#define EMPTY_LAYER (layer){._={0,0}}

// extern const uint8_t sub_layer[121];

extern const uint8_t sub_layer_offset_direct[121];

layer rotate_layer_right(const layer input);

extern const uint8_t sub_layer_table[121];

extern const uint8_t rotate_right[121];

extern const uint8_t rotate_left[121];

// #define sub_layer(i) (i > 63)
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

#define layer_neg(a, b) ((layer) {._ = {~a._[0], ~a._[1]}})

// can only be called with n > 0 && n < 65
#define layer_shiftl(l, n) \
((layer) {l._[0] << n, (l._[1] << n) | (l._[0] >> (64 - n))})

// can only be called with n > 0 && n < 65
#define layer_shiftr(l, n) \
((layer) {(l._[0] >> n) | (l._[1] << (64 - n)), l._[1] >> n})

#define check_index(layer, i) \
  layer._[sub_layer[i]] & ((uint64_t)1 << sub_layer_offset_direct[i])

#define toggle_index(layer, i) \
  layer._[sub_layer[i]] |= ((uint64_t)1 << sub_layer_offset_direct[i])

extern layer corners;
