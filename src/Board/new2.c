/*
  same as new but using 2 ints
 */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <x86intrin.h>

typedef uint64_t layer[2];

const layer EMPTY_LAYER = {0,0};

const char sub_layer[121] = {
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

const char sub_layer_offset[121] = {
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

const uint64_t lower_5 = 0x7fffffffffffff;

const uint64_t inverted_throne_mask = 0b11111011111;

typedef struct board {
  layer white_pawns;
  layer king;
  layer black_pawns;
} board;

void read_layer(const char *string, char symbol, layer output) {
  int len = strlen(string);
  int i;
  char c;
  int index = 0;
  for (i = 0; i < len; i++) {
    char c = string[i];
    if (c == symbol) {
      output[sub_layer[index]] |= ((uint64_t) 1 << (index - sub_layer_offset[index]));
      index++;
    } else if (c == ' ') {
      // skip space
    } else {
      // skip other chars but increment
      index++;
    }
  }
  // printf("done read_layer\n");
}

void rotate_layer(const layer input, layer output) {
  int i, r;
   for (i = 0; i < 121; i++) {
     if (input[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
       r = rotate_right[i];
       output[sub_layer[r]] |= ((uint64_t) 1 << (r - sub_layer_offset[r]));
     }
  }
}


void print_layer(layer layer) {
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
    if (layer[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
}


const char* corners_string =
  "X  .  .  .  .  .  .  .  .  .  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  .  .  .  .  .  .  .  .  .  X";

const char* test_string =
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

const char* angle_test =
  "X  X  X  X  X  X  X  X  X  X  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  X  X  X  X  X  X  X  X  X  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  X  X  X  X  X  X  X  X  X  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  X  X  X  X  X  X  X  X  X  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  X  X  X  X  X  X  X  X  X  X";

unsigned short reverse_16bit(unsigned short v) {
  // swap odd and even bits
  v = ((v >> 1) & 0x55555555) | ((v & 0x55555555) << 1);
  // swap consecutive pairs
  v = ((v >> 2) & 0x33333333) | ((v & 0x33333333) << 2);
  // swap nibbles
  v = ((v >> 4) & 0x0F0F0F0F) | ((v & 0x0F0F0F0F) << 4);
  // swap bytes
  v = ((v >> 8) & 0x00FF00FF) | ((v & 0x00FF00FF) << 8);
  // swap 2-byte long pairs
  v = (v >> 16) | (v << 16);
  return v;
}

/* Based on obstruction difference, but taking row position as input
   and determining the lower and upper masks inline. */
/*
unsigned short get_row_moves(unsigned short occ, char pos) {
  unsigned short lower, upper, ms1B, odiff;
  static const unsigned short lowers[11] = {
    0b0000000000000000,
    0b0000000000000001,
    0b0000000000000011,
    0b0000000000000111,
    0b0000000000001111,
    0b0000000000011111,
    0b0000000000111111,
    0b0000000001111111,
    0b0000000011111111,
    0b0000000111111111,
    0b0000001111111111
  };
  static const unsigned short uppers[11] = {
    0b0000011111111110,
    0b0000011111111100,
    0b0000011111111000,
    0b0000011111110000,
    0b0000011111100000,
    0b0000011111000000,
    0b0000011110000000,
    0b0000011100000000,
    0b0000011000000000,
    0b0000010000000000,
    0b0000000000000000
  };
  lower = occ & lowers[pos];
  upper = occ & uppers[pos];
  ms1B = (((unsigned short) 1 << 15)) >> __lzcnt16(lower | 1); 
  unsigned short ls1b  = upper & -upper;
  //return ms1B;
  odiff = ls1b - (ms1B);
  //odiff = upper ^ ((upper - ms1B));
  return (lowers[pos] | uppers[pos]) & odiff;
}
*/

unsigned short get_row_moves(const unsigned short occ, const char pos) {
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
    // The below is never used as a mask, only by `rightward` when
    // `upper` is empty
    0b11111111111
  };
  static const unsigned short uppers[11] = {
    0b11111111110,
    0b11111111100,
    0b11111111000,
    0b11111110000,
    0b11111100000,
    0b11111000000,
    0b11110000000,
    0b11100000000,
    0b11000000000,
    0b10000000000,
    0b00000000000
  };
  unsigned short lower = occ & lowers[pos];
  //unsigned short lower = occ & 0b01111111111 >> (10-pos);
  //unsigned short upper = occ & uppers[pos];
  unsigned short upper = occ & (0b11111111110 << pos);
  //unsigned short rightward = (1 << __builtin_ctz(upper | 0x800)) - 1;
  unsigned short rightward = lowers[_tzcnt_u16(upper | 0x800)];
  unsigned short blocked = 0xFFFF >> __lzcnt16(lower);
  return (rightward - blocked) ^ (1 << pos);
}

typedef struct move {
  char orig;
  char dest;
} move;

typedef struct moves {
  int num;
  move *moves;
} moves;

void print_row(unsigned short row) {
  char output[12];
  memset(output, '0', 11);
  output[11] = '\0';
  int index;
  while (row) {
    index = _tzcnt_u16(row);
    output[10 - index] = '1';
    row &= ~(1 << index);
  }
  puts(output);
  printf("\n");
}

const char row_indexes[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
  22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
  33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
  44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,
  55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
  66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
  77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
  88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
  99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
  110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110
}; 

 
void get_team_moves(const layer occ, const layer team,
		    const layer occ_90, const layer team_90,
		    int *total, move *moves ) {

  uint64_t prog;
  int next, next_r, offset, row_index, dest_index, board_index;
  unsigned short row, row_moves;

  *total = 0;


  /*
  */
  // lower 5 rows
  prog = team[0] & lower_5;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;
    // printf("next = %d\n", next);


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ[0] >> offset;
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    while (row_moves) {
      dest_index = _tzcnt_u16(row_moves);
      row_moves &= row_moves - 1;
      board_index = offset + dest_index;
      // printf("(%d, %d)\n", next, board_index);
      moves[*total] = (struct move) {next, board_index};
      (*total)++;
    }
  }

  // upper 5 rows
  prog = team[1] >> 2;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;
    // printf("next = %d\n", next);


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ[1] >> (offset + 2);
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    while (row_moves) {
      dest_index = _tzcnt_u16(row_moves);
      row_moves &= row_moves - 1;
      board_index = offset + dest_index;
      // printf("(%d, %d)\n", next, board_index);
      moves[*total] = (struct move) {next, board_index};
      (*total)++;
    }
  }

  // lower 5 rows 90
  prog = team_90[0] & lower_5;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;
    // printf("next = %d\n", next);


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ[0] >> offset;
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    while (row_moves) {
      dest_index = _tzcnt_u16(row_moves);
      row_moves &= row_moves - 1;
      board_index = offset + dest_index;
      // printf("(%d, %d)\n", next, board_index);
      moves[*total] = (struct move) {next, board_index};
      (*total)++;
    }
  }

  // upper 5 rows 90
  prog = team_90[1] >> 2;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ_90[1] >> (offset + 2);
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    while (row_moves) {
      dest_index = _tzcnt_u16(row_moves);
      row_moves &= row_moves - 1;
      board_index = offset + dest_index;
      // printf("(%d, %d)\n", rotate_left[next + 66], rotate_left[board_index + 66]);
      moves[*total] = (struct move) {rotate_left[next + 66], rotate_left[board_index + 66]};
      (*total)++;
    }
  }

  // center horzontal
  row = (occ[0] >> 55) | ((occ[1] & 0x3) << 9);
  prog = (team[0] >> 55) | ((team[1] & 0x3) << 9);
  // print_row(row);
  // print_row(prog);
  while (prog) {
    // printf("------------------------------------------------------------\n");
    next = _tzcnt_u64(prog);
    prog &= prog - 1;

    row_moves = get_row_moves(row, next) & inverted_throne_mask;
    // print_row(row_moves);
    while (row_moves) {
      dest_index = _tzcnt_u16(row_moves);
      row_moves &= row_moves - 1;
      moves[*total] = (struct move) {next + 55, dest_index + 55};
      // printf("(%d, %d)\n", next + 55, dest_index + 55);
      (*total)++;
    }
  }

  // center vertical
  row = (occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9);
  prog = (team_90[0] >> 55) | ((team_90[1] & 0x3) << 9);
  // print_row(row);
  // print_row(prog);
  while (prog) {
    // printf("------------------------------------------------------------\n");
    next = _tzcnt_u64(prog);
    prog &= prog - 1;

    row_moves = get_row_moves(row, next) & inverted_throne_mask;
    // print_row(row_moves);
    while (row_moves) {
      dest_index = _tzcnt_u16(row_moves);
      row_moves &= row_moves - 1;
      // printf("(%d, %d)\n", rotate_left[next + 55], rotate_left[dest_index + 55]);
      moves[*total] = (struct move) {rotate_left[next + 55], rotate_left[board_index + 55]};
      (*total)++;
    }
  }
  

  // printf("move count: %d\n", total);
  //return (struct moves) {0, moves};
}

int get_team_move_count(const layer occ, const layer team,
			const layer occ_90, const layer team_90) {

  uint64_t prog;
  int total, next, next_r, offset, row_index, dest_index, board_index;
  unsigned short row, row_moves;

  total = 0;

  /*
  */
  // lower 5 rows
  prog = team[0] & lower_5;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;
    // printf("next = %d\n", next);


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ[0] >> offset;
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    //total += _mm_popcnt_u32(row_moves);
    total += __builtin_popcount(row_moves);
  }


  // upper 5 rows
  prog = team[1] >> 2;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;
    // printf("next = %d\n", next);


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ[1] >> (offset + 2);
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    total += __builtin_popcount(row_moves);
  }

  // lower 5 rows 90
  prog = team_90[0] & lower_5;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;
    // printf("next = %d\n", next);


    // horizontal
    offset = 11 * (next / 11);
    //offset = row_indexes[next];
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ[0] >> offset;
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    total += __builtin_popcount(row_moves);
  }

  // upper 5 rows 90
  prog = team_90[1] >> 2;
  while (prog) {
    next = _tzcnt_u64(prog);
    // printf("next = %d\n", next);
    prog &= prog - 1;


    // horizontal
    //offset = row_indexes[next];
    offset = 11 * (next / 11);
    // printf("offset = %d\n", offset);
    row_index = next % 11;
    //row_index = row_indexes[next];
    // printf("row_index = %d\n", row_index);
    row = (uint64_t) occ_90[1] >> (offset + 2);
    row_moves = get_row_moves(row, row_index);
    // print_row(row);
    // print_row(row_moves);
    total += __builtin_popcount(row_moves);
  }

  // center horzontal
  row = (occ[0] >> 55) | ((occ[1] & 0x3) << 9);
  prog = (team[0] >> 55) | ((team[1] & 0x3) << 9);
  // print_row(row);
  // print_row(prog);
  while (prog) {
    // printf("------------------------------------------------------------\n");
    next = _tzcnt_u64(prog);
    prog &= prog - 1;

    row_moves = get_row_moves(row, next) & inverted_throne_mask;
    // print_row(row_moves);
    total += __builtin_popcount(row_moves);
  }

  // center vertical
  row = (occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9);
  prog = (team_90[0] >> 55) | ((team_90[1] & 0x3) << 9);
  // print_row(row);
  // print_row(prog);
  while (prog) {
    // printf("------------------------------------------------------------\n");
    next = _tzcnt_u64(prog);
    prog &= prog - 1;

    row_moves = get_row_moves(row, next) & inverted_throne_mask;
    // print_row(row_moves);
    total += __builtin_popcount(row_moves);
  }
  
  /*
  */

  // printf("move count: %d\n", total);
  //return (struct moves) {0, moves};
  return total;
}

/*
int main(int argc, char **argv) {
  layer l = {0,0,0};
  read_layer(corners_string, 'X', l);
  layer l2 = {0,0,0};
  rotate_layer(l, l2);
  // printf("finish read\n");
  // printf("layer = %ld\n", l[0]);
  print_layer(l);
  unsigned short occ = 0b0000001000000100;
  print_row(occ);
  print_row(get_row_moves(occ, 0));
  print_row(get_row_moves(occ, 1));
  print_row(get_row_moves(occ, 2));
  print_row(get_row_moves(occ, 3));
  print_row(get_row_moves(occ, 4));
  print_row(get_row_moves(occ, 5));
  print_row(get_row_moves(occ, 6));
  print_row(get_row_moves(occ, 7));
  print_row(get_row_moves(occ, 8));
  print_row(get_row_moves(occ, 9));
  print_row(get_row_moves(occ, 10));
  get_team_moves(l, l, l2, l2);

}
*/

/*
  We need three loops for move generation in any event; if:
  - we use three integers
  - we use two and we need to combine the edges in the middle row
  - we acount for the throne only in the middle row to void the check
    where unnecessary

  therefore it may be better to use two ints instead of 3 even though
  we split the middle row, because we will need special handling of
  the middle row /anyways/ and we may then save on math in other
  contexts.

  the date layout strategies to test (move gen and then move count):
  - original
  - split over 3 ints with rotated bitboards
  - split over 3 ints and generating vertical slices
  - split over 2 ints (rotated or gen vert depending on above winner)

  then test the fastest of the above against:
  - move lookup tables with row bitboards
  - move lookup table with dense packed destinations
    (char moves[11] - where the first byte encodes the number of moves)
  
 */


const char* start_board_string =
  " .  .  .  X  X  X  X  X  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " X  .  .  .  .  O  .  .  .  .  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  X  .  O  O  #  O  O  .  X  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  .  .  .  .  O  .  .  .  .  X "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  X  X  X  X  X  .  .  . ";

void layer_or(layer base, const layer input) {
  base[0] |= input[0];
  base[1] |= input[1];
}

int main(int argc, char **argv) {
  printf("New 2: Running test\n");

  // read and verify boards
  layer corners = {0,0};
  read_layer(corners_string, 'X', corners);
  print_layer(corners);
  printf("\n");
  layer black = {0,0};
  read_layer(start_board_string, 'X', black);
  print_layer(black);
  printf("\n");
  layer white = {0,0};
  read_layer(start_board_string, 'O', white);
  print_layer(white);
  printf("\n");
  layer occ = {0,0};
  layer_or(occ, corners);
  layer_or(occ, black);
  layer_or(occ, white);
  print_layer(occ);
  printf("\n");

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  // setup
  // none


  /*
  // run for result
  move moves[235]; // 235 is a generous max move count
  int total;
  get_team_moves(occ, black, occ, black, &total, moves);
  printf("move_count: %d\n", total);


  // run for bench
  int bench_count = 15000000;
  while (bench_count) { 
    get_team_moves(occ, black, occ, black, &total, moves);
    bench_count--;
  }
  */

  // run for result
  int total;
  total = get_team_move_count(occ, black, occ, black);
  printf("move_count: %d\n", total);


  // run for bench
  int bench_count = 15000000;
  while (bench_count) { 
    get_team_move_count(occ, black, occ, black);
    bench_count--;
  }

  /*
  */

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
}
