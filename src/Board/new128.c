#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef unsigned __int128 u128;

const char rotate_right[121] = {
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

const char rotate_left[121] = {
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

typedef struct board {
  u128 white_pawns;
  u128 king;
  u128 black_pawns;
} board;

u128 read_layer(const char *string) {
  u128 output = 0;
  int len = strlen(string);
  int i;
  char c;
  int index = 0;
  for (i = 0; i < len; i++) {
    char c = string[i];
    if (c == '.') {
      index++;
    } else if (c == 'X') {
      output |= ((u128) 1 << index);
      index++;
    } else {}
  }
  return output;
}


void print_layer(u128 layer) {
  char string[374];


  // initialize empty string
  // printf("hit\n");
  memset(string, ' ', 373);
  string[373] = '\0';


  int i;


  // insert newlines
  for (i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate char
  for (i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = ((i * 3) + 1) + newline_offset;
    if (layer & ((u128) 1 << i)) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
}

u128 test128and(u128 x, u128 y) {
  u128 output = x & y;
  return output;
}

int test128actz(u128 x) {
  int output = __builtin_ctz(x);
  return output;
}

u128 test128sub(u128 input) {
  return input - 1;
}

uint64_t test64(uint64_t input) {
  uint64_t full_board = ((uint64_t) 1 << 64) - 1;
  uint64_t output = full_board ^ input;
  return output;
}

const char* corners_string =
  "X  .  .  .  .  .  .  .  .  .  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  X  .  X  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  X  .  .  .  .  .  X  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  X  .  X  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  .  .  .  .  .  .  .  .  .  X";

int main(int argc, char **argv) {
  u128 l = read_layer(corners_string);



  printf("finish read\n");
  printf("layer = %ld\n", l);
  print_layer(l);
  print_layer(((u128) 1 << 128) - 1);
  u128 l2 = test128(l);
  print_layer(l2);

}
