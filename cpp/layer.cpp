/**
 * layer.cpp
 *
 * A layer is a representation of a set of board positions. Only
 * presence or absence is indicated. The two 64-bit integers making up
 * a layer are termed "sub layers", with the one at index 0 being
 * "lower" and the one at index 1 being "upper".
 */

#pragma once

#include <iostream>
#include <cstring>
#include <stdint.h>
#include <stdlib.h>
#include <string>
#include <array>
#include <libbase64.h>
using std::array;

// typedef uint64_t layer[2];

typedef array<uint64_t, 2> layer;

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

constexpr unsigned char sub_layer[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
};

/**
 * This lookup table only contains 55 elements as positions above 55
 * should be handled separately, being split between two layers.
 */
constexpr uint sub_layer_row_offset[55] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
  22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
  33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
  44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44
};

/**
 * The first two indices should not be used, as these represent
 * squares of a row split between both halves which need to be handled
 * separately. They're only here so that the upper element index
 * numbers are correct.
 */
constexpr uint sub_layer_row_offset_upper[57] = {
  0, 0,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
  46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46
};

/**
 * mask which retains only the complete rows of the lower layer,
 * e.g. the first 55 bits.
 */
const uint64_t lower_rows_mask = 0x7FFFFFFFFFFFFF;

/**
 * mask which retains only the complete rows of the upper layer,
 * e.g. 55 bits after skipping the first two.
 */
const uint64_t upper_rows_mask = 0x1FFFFFFFFFFFFFC;


constexpr unsigned int sub_layer_offset[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
};

const unsigned int sub_layer_offset_direct[121] = {
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
  22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
  33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
  44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
  55, 56, 57, 58, 59, 60, 61, 62, 63, 0,  1,
  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12,
  13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
  24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
  35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
};


// #define remove(layer, i) layer[sub_layer[i]] -= ((uint64_t)1 << sub_layer_offset_direct[i]);
// #define remove_d(layer, sub, i) layer[sub] -= ((uint64_t)1 << i);

constexpr unsigned char rotate_right[121] = {
  10, 21, 32, 43, 54, 65, 76, 87, 98, 109, 120,
  9,  20, 31, 42, 53, 64, 75, 86, 97, 108, 119,
  8,  19, 30, 41, 52, 63, 74, 85, 96, 107, 118,
  7,  18, 29, 40, 51, 62, 73, 84, 95, 106, 117,
  6,  17, 28, 39, 50, 61, 72, 83, 94, 105, 116,
  5,  16, 27, 38, 49, 60, 71, 82, 93, 104, 115,
  4,  15, 26, 37, 48, 59, 70, 81, 92, 103, 114,
  3,  14, 25, 36, 47, 58, 69, 80, 91, 102, 113,
  2,  13, 24, 35, 46, 57, 68, 79, 90, 101, 112,
  1,  12, 23, 34, 45, 56, 67, 78, 89, 100, 111,
  0,  11, 22, 33, 44, 55, 66, 77, 88, 99,  110
};

const unsigned char rotate_left[121] = {
  110, 99,  88, 77, 66, 55, 44, 33, 22, 11, 0,
  111, 100, 89, 78, 67, 56, 45, 34, 23, 12, 1,
  112, 101, 90, 79, 68, 57, 46, 35, 24, 13, 2,
  113, 102, 91, 80, 69, 58, 47, 36, 25, 14, 3,
  114, 103, 92, 81, 70, 59, 48, 37, 26, 15, 4,
  115, 104, 93, 82, 71, 60, 49, 38, 27, 16, 5,
  116, 105, 94, 83, 72, 61, 50, 39, 28, 17, 6,
  117, 106, 95, 84, 73, 62, 51, 40, 29, 18, 7,
  118, 107, 96, 85, 74, 63, 52, 41, 30, 19, 8,
  119, 108, 97, 86, 75, 64, 53, 42, 31, 20, 9,
  120, 109, 98, 87, 76, 65, 54, 43, 32, 21, 10
};

const uint64_t inverted_throne_mask = 0b11111011111;

constexpr layer rotate_layer(const layer input) {
  layer output = {0};
  for (int i = 0; i < 121; i++) {
    if (input[sub_layer[i]] & ((uint64_t)1 << (i - sub_layer_offset[i]))) {
      int r = rotate_right[i];
      output[sub_layer[r]] |= ((uint64_t)1 << (r - sub_layer_offset[r]));
    }
  }
  return output;
}

/** This implementation reads layer strings from top left to bottom right, but starts at index 120 and works down, 
 */
constexpr layer read_layer(const char *string, unsigned char symbol) {
  layer output = {0};
  int len = std::char_traits<char>::length(string);
  int index = 120;
  for (int i = 0; i < len; i++) {
    char c = string[i];
    if (c == symbol) {
      output[sub_layer[index]] |= ((uint64_t) 1 << (index - sub_layer_offset[index]));
      index--;
    } else if (c == ' ') {
      // skip space
    } else {
      index--; // skip other chars but increment
    }
  }
  return output;
}

std::string stringify(layer layer) {
  std::string string (373, ' ');

  // insert newlines
  for (int i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate unsigned char
  for (int i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = 373 - (((i * 3) + 1) + newline_offset);
    if (layer[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }
  return string;
}

inline void print_layer(layer layer) {
  char string[374];

  // initialize empty string
  memset(string, ' ', 373);
  string[373] = '\0';


  // insert newlines
  for (int i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate unsigned char
  for (int i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = 373 - (((i * 3) + 1) + newline_offset);
    if (layer[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}


inline __attribute__((always_inline)) layer layer_or(const layer a,
                                                     const layer b) {
  return {a[0] | b[0], a[1] | b[1]};
}

inline __attribute__((always_inline)) layer operator|(const layer a,
                                                     const layer b) {
  return {a[0] | b[0], a[1] | b[1]};
}

inline __attribute__((always_inline)) layer layer_and(const layer a,
                                                      const layer b) {
  return {a[0] & b[0], a[1] & b[1]};
}

inline __attribute__((always_inline)) layer operator&(const layer a,
                                                      const layer b) {
  return {a[0] & b[0], a[1] & b[1]};
}

inline __attribute__((always_inline)) layer layer_xor(const layer a,
                                                      const layer b) {
  return {a[0] ^ b[0], a[1] ^ b[1]};
}

inline __attribute__((always_inline)) layer operator^(const layer a,
                                                      const layer b) {
  return {a[0] ^ b[0], a[1] ^ b[1]};
}

inline __attribute__((always_inline)) layer layer_negate(const layer a) {
  return {~a[0], ~a[1]};
}

inline __attribute__((always_inline)) layer operator~(const layer a) {
  return {~a[0], ~a[1]};
}

template <int n>
inline __attribute__((always_inline)) layer layer_shiftl(const layer input) {
  constexpr int upper_offset = 64 - n;
  return {input[0] << n, (input[1] << n) | (input[0] >> upper_offset)};
}

inline __attribute__((always_inline)) constexpr layer operator<<(const layer input, const int n) {
  int upper_offset = 64 - n;
  return {input[0] << n, (input[1] << n) | (input[0] >> upper_offset)};
}

template <int n>
inline __attribute__((always_inline)) layer layer_shiftr(const layer input) {
  constexpr int lower_offset = 64 - n;
  return {(input[0] >> n) | (input[1] << lower_offset), input[1] >> n};
}

inline __attribute__((always_inline)) constexpr layer operator>>(const layer input, const int n) {
  int lower_offset = 64 - n;
  return {(input[0] >> n) | (input[1] << lower_offset), input[1] >> n};
}

//******************************************************************************
// base64

std::string encode_layer(layer input) {
  // char src[] = "hello world";
  // uint8_t (&array_of_bytes)[sizeof(layer)] =
  // char(&src)[sizeof(uint64_t)*2] = *reinterpret_cast<char(*)[sizeof(uint64_t)*2]>(input.data());
  // char(&src)[sizeof(uint64_t)] = *reinterpret_cast<char(*)[sizeof(uint64_t)]>(input[0]);


  char src[16];
  memcpy(&src, input.data(), sizeof(src));

  layer test_layer;
  memcpy(&test_layer, src, sizeof(test_layer));
  
  print_layer(test_layer);
  /*
  */

  size_t srclen = 16;
  char out[40];
  size_t outlen;
  base64_encode(src, srclen, out, &outlen, 0);

  // without conversion to std::string

  char test_out[40];
  uint64_t test_outlen;
  base64_decode(out, outlen, test_out, &test_outlen, 0);

  uint64_t test_lower;
  memcpy(&test_lower, test_out, sizeof(test_lower));
  print_layer({test_lower, 0});

  // with conversion to std::string

  std::string std_string(out, outlen);

  char test_2_out[40];
  uint64_t test_2_outlen;
  base64_decode(&std_string[0], outlen, test_2_out, &test_2_outlen, 0);

  uint64_t test_2_lower;
  memcpy(&test_2_lower, test_2_out, sizeof(test_2_lower));
  print_layer({test_2_lower, 0});





  std::string output(out, outlen);
  std::cout << output << "\n";
  std::cout << outlen << "\n";
  return output;
};

layer decode_layer(std::string input) {
  // char src[] = "hello world";
  // uint8_t (&array_of_bytes)[sizeof(layer)] =

  std::cout << input << "\n";
  std::cout << input.length() << "\n";

  size_t srclen = 24;
  char out[40];
  size_t outlen;
  base64_decode(&input[0], srclen, out, &outlen, 0);

  std::cout << outlen << "\n";
 
  layer l;
  memcpy(&l, out, sizeof(l));
  return l;
};

