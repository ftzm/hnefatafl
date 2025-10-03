#include "layer.h"
#include "x86intrin.h" // IWYU pragma: export

/* A handy index guide:
120 119 118 117 116 115 114 113 112 111 110
109 108 107 106 105 104 103 102 101 100 99
98  97  96  95  94  93  92  91  90  89  88
87  86  85  84  83  82  81  80  79  78  77
76  75  74  73  72  71  70  69  68  67  66
65  64  63  62  61  60  59  58  57  56  55
54  53  52  51  50  49  48  47  46  45  44
43  42  41  40  39  38  37  36  35  34  33
32  31  30  29  28  27  26  25  24  23  22
21  20  19  18  17  16  15  14  13  12  11
10  9   8   7   6   5   4   3   2   1   0
*/

const u8 sub_layer_table[121] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
};

// const u8 sub_layer[121] = {
//   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//   0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
//   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
// };
#define GET_LOWER_ROW(layer, index)                                            \
  (0x7ff & ((layer._[0] >> sub_layer_row_offset[index])))
#define GET_UPPER_ROW(layer, index)                                            \
  (0x7ff & ((layer._[1] >> sub_layer_row_offset_upper[index])))

#define DIRTY_GET_ROW_0(l) (u64) l._[0]
#define DIRTY_GET_ROW_1(l) ((u64)l._[0] >> 11)
#define DIRTY_GET_ROW_2(l) ((u64)l._[0] >> 22)
#define DIRTY_GET_ROW_3(l) ((u64)l._[0] >> 33)
#define DIRTY_GET_ROW_4(l) ((u64)l._[0] >> 44)
#define DIRTY_GET_ROW_5(l) ((u64)l._[0] >> 55) | ((((u64)l._[1] & 0x3) << 9))
#define DIRTY_GET_ROW_6(l) ((u64)l._[1] >> 2)
#define DIRTY_GET_ROW_7(l) ((u64)l._[1] >> 13)
#define DIRTY_GET_ROW_8(l) ((u64)l._[1] >> 24)
#define DIRTY_GET_ROW_9(l) ((u64)l._[1] >> 35)
#define DIRTY_GET_ROW_10(l) ((u64)l._[1] >> 46)

u16 dirty_get_row(layer l, int n) {
  switch (n) {
  case 0:
    return DIRTY_GET_ROW_0(l);
  case 1:
    return DIRTY_GET_ROW_1(l);
  case 2:
    return DIRTY_GET_ROW_2(l);
  case 3:
    return DIRTY_GET_ROW_3(l);
  case 4:
    return DIRTY_GET_ROW_4(l);
  case 5:
    return DIRTY_GET_ROW_5(l);
  case 6:
    return DIRTY_GET_ROW_6(l);
  case 7:
    return DIRTY_GET_ROW_7(l);
  case 8:
    return DIRTY_GET_ROW_8(l);
  case 9:
    return DIRTY_GET_ROW_9(l);
  case 10:
    return DIRTY_GET_ROW_10(l);
  default:
    __builtin_unreachable();
  }
}

#define MASK_ROW(r) (r & 0x7ff)

#define GET_ROW_0(l) MASK_ROW(DIRTY_GET_ROW_0(l))
#define GET_ROW_1(l) MASK_ROW(DIRTY_GET_ROW_1(l))
#define GET_ROW_2(l) MASK_ROW(DIRTY_GET_ROW_2(l))
#define GET_ROW_3(l) MASK_ROW(DIRTY_GET_ROW_3(l))
#define GET_ROW_4(l) MASK_ROW(DIRTY_GET_ROW_4(l))
#define GET_ROW_5(l) MASK_ROW(DIRTY_GET_ROW_5(l))
#define GET_ROW_6(l) MASK_ROW(DIRTY_GET_ROW_6(l))
#define GET_ROW_7(l) MASK_ROW(DIRTY_GET_ROW_7(l))
#define GET_ROW_8(l) MASK_ROW(DIRTY_GET_ROW_8(l))
#define GET_ROW_9(l) MASK_ROW(DIRTY_GET_ROW_9(l))
#define GET_ROW_10(l) MASK_ROW(DIRTY_GET_ROW_10(l))

/**
 *
 */
u16 get_row(layer l, int n) { return MASK_ROW(dirty_get_row(l, n)); }

u16 get_index_row(layer l, int i) {
  if (i < 55) {
    return GET_LOWER_ROW(l, i);
  } else if (i > 65) {
    return GET_UPPER_ROW(l, i - 64);
  } else {
    return GET_CENTER_ROW(l);
  }
}

/**
 * mask which retains only the complete rows of the lower layer,
 * e.g. the first 55 bits.
 */
const u64 lower_rows_mask = 0x7FFFFFFFFFFFFF;
#define LOWER_ROWS_MASK = (u64)0x7FFFFFFFFFFFFF;

/**
 * mask which retains only the complete rows of the upper layer,
 * e.g. 55 bits after skipping the first two.
 */
const u64 upper_rows_mask = 0x1FFFFFFFFFFFFFC;
#define UPPER_ROWS_MASK = (u64)0x1FFFFFFFFFFFFFC;

const u8 sub_layer_offset_direct[121] = {
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,
    54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 0,  1,  2,  3,  4,  5,  6,  7,
    8,  9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
    44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
};

const u8 rotate_right[121] = {
    10,  21,  32,  43, 54,  65,  76,  87,  98, 109, 120, 9,   20,  31,  42, 53,
    64,  75,  86,  97, 108, 119, 8,   19,  30, 41,  52,  63,  74,  85,  96, 107,
    118, 7,   18,  29, 40,  51,  62,  73,  84, 95,  106, 117, 6,   17,  28, 39,
    50,  61,  72,  83, 94,  105, 116, 5,   16, 27,  38,  49,  60,  71,  82, 93,
    104, 115, 4,   15, 26,  37,  48,  59,  70, 81,  92,  103, 114, 3,   14, 25,
    36,  47,  58,  69, 80,  91,  102, 113, 2,  13,  24,  35,  46,  57,  68, 79,
    90,  101, 112, 1,  12,  23,  34,  45,  56, 67,  78,  89,  100, 111, 0,  11,
    22,  33,  44,  55, 66,  77,  88,  99,  110};

const u8 rotate_left[121] = {
    110, 99,  88,  77,  66,  55,  44,  33,  22,  11,  0,   111, 100, 89,
    78,  67,  56,  45,  34,  23,  12,  1,   112, 101, 90,  79,  68,  57,
    46,  35,  24,  13,  2,   113, 102, 91,  80,  69,  58,  47,  36,  25,
    14,  3,   114, 103, 92,  81,  70,  59,  48,  37,  26,  15,  4,   115,
    104, 93,  82,  71,  60,  49,  38,  27,  16,  5,   116, 105, 94,  83,
    72,  61,  50,  39,  28,  17,  6,   117, 106, 95,  84,  73,  62,  51,
    40,  29,  18,  7,   118, 107, 96,  85,  74,  63,  52,  41,  30,  19,
    8,   119, 108, 97,  86,  75,  64,  53,  42,  31,  20,  9,   120, 109,
    98,  87,  76,  65,  54,  43,  32,  21,  10};

layer rotate_layer_right(const layer input) {
  layer output = EMPTY_LAYER;

  u64 lower = input._[0];
  while (lower) {
    int r = rotate_right[_tzcnt_u64(lower)];
    SET_INDEX(output, r);
    lower &= (lower - 1);
  }
  u64 upper = input._[1];
  while (upper) {
    int r = rotate_right[64 + _tzcnt_u64(upper)];
    SET_INDEX(output, r);
    upper &= (upper - 1);
  }

  return output;
}

layer rotate_layer_left(const layer input) {
  layer output = EMPTY_LAYER;

  u64 lower = input._[0];
  while (lower) {
    int r = rotate_left[_tzcnt_u64(lower)];
    SET_INDEX(output, r);
    lower &= (lower - 1);
  }
  u64 upper = input._[1];
  while (upper) {
    int r = rotate_left[64 + _tzcnt_u64(upper)];
    SET_INDEX(output, r);
    upper &= (upper - 1);
  }

  return output;
}

const u8 rank_table[121] = {
    0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1, 1, 1, 1, 1,
    1, 2, 2, 2, 2, 2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3, 3, 3, 3, 3,
    3, 3, 4, 4, 4, 4,  4,  4,  4,  4,  4,  4,  4,  5,  5,  5,  5, 5, 5, 5, 5,
    5, 5, 5, 6, 6, 6,  6,  6,  6,  6,  6,  6,  6,  6,  7,  7,  7, 7, 7, 7, 7,
    7, 7, 7, 7, 8, 8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  9,  9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
};

const u8 file_table[121] = {
    0,  1,  2,  3,  4,  5, 6, 7, 8, 9, 10, 0,  1,  2,  3,  4,  5, 6, 7, 8, 9,
    10, 0,  1,  2,  3,  4, 5, 6, 7, 8, 9,  10, 0,  1,  2,  3,  4, 5, 6, 7, 8,
    9,  10, 0,  1,  2,  3, 4, 5, 6, 7, 8,  9,  10, 0,  1,  2,  3, 4, 5, 6, 7,
    8,  9,  10, 0,  1,  2, 3, 4, 5, 6, 7,  8,  9,  10, 0,  1,  2, 3, 4, 5, 6,
    7,  8,  9,  10, 0,  1, 2, 3, 4, 5, 6,  7,  8,  9,  10, 0,  1, 2, 3, 4, 5,
    6,  7,  8,  9,  10, 0, 1, 2, 3, 4, 5,  6,  7,  8,  9,  10,
};
