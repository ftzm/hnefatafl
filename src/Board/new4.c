/*
  same as new2 but using a lookup table for row moves
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

/*
const char exp[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, T, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, P, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, X, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, P, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, T, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
};

const char exp[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, X, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, P, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, T, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
};

const char sub_layer[121] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, X, P, T, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
};

*/

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

const char sub_layer_offset_direct[121] = {
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
  22, 13, 24, 25, 26, 27, 28, 29, 30, 31, 32,
  33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
  44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
  55, 56, 57, 58, 59, 60, 61, 62, 63, 0,  1,
  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12,
  13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 13,
  24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
  35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
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
  printf("\n");
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
  unsigned short upper = occ & (0b11111111110 << pos);
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

uint16_t row_moves_table[2048][11];
uint16_t center_row_moves_table[2048][11];

void gen_row_moves() {
  uint16_t row;
  char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      row_moves_table[row][pos] = get_row_moves(row, pos);
    }
  }
}

void gen_center_row_moves() {
  uint16_t row;
  char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      center_row_moves_table[row][pos] = get_row_moves(row, pos) & inverted_throne_mask;
    }
  }
}


int row_move_count_table[2048][11];
int center_row_move_count_table[2048][11];

void gen_row_move_counts() {
  uint16_t row;
  char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      row_move_count_table[row][pos] = __builtin_popcount(get_row_moves(row, pos));
    }
  }
}

void gen_center_row_move_counts() {
  uint16_t row;
  char pos;
  for (row = 0; row < 2048; row++) {
    for (pos = 0; pos < 11; pos++) {
      center_row_move_count_table[row][pos] =
	__builtin_popcount(get_row_moves(row, pos)
			   & inverted_throne_mask);
    }
  }
}
 
inline void get_row_total_moves(const uint64_t team, const uint64_t occ,
			        const int offset, int *total) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
  while (movers) {
    (*total) += row_move_count_table[blockers][_tzcnt_u16(movers)];
    movers &= movers - 1;
  }
}

inline void get_all_row_moves(const uint64_t team, const uint64_t occ,
			      const int offset, int *total, move *moves, move *moves_r) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  //const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
  uint64_t row_moves;
  char local_orig, orig, orig_r, dest;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = offset + local_orig;
    orig_r = rotate_right[orig];
    const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
    row_moves = (uint64_t) row_moves_table[blockers][local_orig] << offset;
    while (row_moves) {
      dest = _tzcnt_u16(row_moves);
      moves[*total] = (struct move) {orig, dest};
      moves_r[*total] = (struct move) {orig_r, rotate_right[dest]};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

inline void get_all_row_moves_r(const uint64_t team, const uint64_t occ,
				const int offset, int *total, move *moves, move *moves_r) {
  unsigned short movers = ((uint64_t) team >> offset) & 0b11111111111;
  unsigned short row_moves;
  char local_orig, orig, orig_r, dest;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = offset + local_orig;
    orig_r = rotate_left[orig];
    const unsigned short blockers = ((uint64_t) occ >> offset) & 0b11111111111;
    row_moves = row_moves_table[blockers][local_orig];
    while (row_moves) {
      dest = offset + _tzcnt_u16(row_moves);
      moves[*total] = (struct move) {orig_r, rotate_left[dest]};
      moves_r[*total] = (struct move) {orig, dest};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

void get_team_moves(const layer occ, const layer team,
		    const layer occ_90, const layer team_90,
		    int *total, move *moves, move *moves_r) {
  *total = 0;

  // upper 5 rows
  get_all_row_moves(team[0], occ[0], 0, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 11, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 22, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 33, total, moves, moves_r);
  get_all_row_moves(team[0], occ[0], 44, total, moves, moves_r);

  // lower 5 rows
  get_all_row_moves(team[1], occ[1], 2, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 13, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 24, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 35, total, moves, moves_r);
  get_all_row_moves(team[1], occ[1], 46, total, moves, moves_r);

  // upper 5 rows
  get_all_row_moves_r(team_90[0], occ_90[0], 0, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 11, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 22, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 33, total, moves, moves_r);
  get_all_row_moves_r(team_90[0], occ_90[0], 44, total, moves, moves_r);

  // lower 5 rows
  get_all_row_moves_r(team_90[1], occ_90[1], 2, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 13, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 24, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 35, total, moves, moves_r);
  get_all_row_moves_r(team_90[1], occ_90[1], 46, total, moves, moves_r);

  unsigned short movers, row_moves;
  char local_orig, orig, orig_r, dest;

  // center horizontal
  movers = (team[0] >> 55) | ((team[1] & 0x3) << 9) & 0b11111111111;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = local_orig + 55;
    orig_r = rotate_right[orig];
    const unsigned short blockers_h =
      (occ[0] >> 55) | ((occ[1] & 0x3) << 9) & 0b11111111111;
    row_moves = center_row_moves_table[blockers_h][_tzcnt_u16(movers)];
    while (row_moves) {
      dest = _tzcnt_u16(row_moves) + 55;
      moves[*total] = (struct move) {orig, dest};
      moves_r[*total] = (struct move) {orig_r, rotate_right[dest]};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }

  // center vertical
  movers = ((team_90[0] >> 55) | ((team_90[1] & 0x3) << 9)) & 0b11111111111;
  while (movers) {
    local_orig = _tzcnt_u16(movers);
    orig = local_orig + 55;
    orig_r = rotate_left[orig];
    const unsigned short blockers_v =
      ((occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9)) & 0b11111111111;
    row_moves = center_row_moves_table[blockers_v][local_orig];
    while (row_moves) {
      dest = _tzcnt_u16(row_moves) + 55;
      moves[*total] = (struct move) {orig_r, rotate_left[dest]};
      moves_r[*total] = (struct move) {orig, dest};
      (*total)++;
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

int get_team_move_count(const layer occ, const layer team,
			const layer occ_90, const layer team_90) {

  uint16_t prog;
  int total, next, next_r, offset, row_index, dest_index, board_index;
  unsigned short row, row_moves;

  total = 0;

  // upper 5 rows
  get_row_total_moves(team[0], occ[0], 0, &total);
  get_row_total_moves(team[0], occ[0], 11, &total);
  get_row_total_moves(team[0], occ[0], 22, &total);
  get_row_total_moves(team[0], occ[0], 33, &total);
  get_row_total_moves(team[0], occ[0], 44, &total);

  // lower 5 rows
  get_row_total_moves(team[1], occ[1], 2, &total);
  get_row_total_moves(team[1], occ[1], 13, &total);
  get_row_total_moves(team[1], occ[1], 24, &total);
  get_row_total_moves(team[1], occ[1], 35, &total);
  get_row_total_moves(team[1], occ[1], 46, &total);

  // upper 5 rows rotated
  get_row_total_moves(team_90[0], occ_90[0], 0, &total);
  get_row_total_moves(team_90[0], occ_90[0], 11, &total);
  get_row_total_moves(team_90[0], occ_90[0], 22, &total);
  get_row_total_moves(team_90[0], occ_90[0], 33, &total);
  get_row_total_moves(team_90[0], occ_90[0], 44, &total);

  // lower 5 rows rotated
  get_row_total_moves(team_90[1], occ_90[1], 2, &total);
  get_row_total_moves(team_90[1], occ_90[1], 13, &total);
  get_row_total_moves(team_90[1], occ_90[1], 24, &total);
  get_row_total_moves(team_90[1], occ_90[1], 35, &total);
  get_row_total_moves(team_90[1], occ_90[1], 46, &total);

  // center horizontal
  row = ((occ[0] >> 55) | ((occ[1] & 0x3) << 9));
  prog = (team[0] >> 55) | ((team[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  // center vertical
  row = ((occ_90[0] >> 55) | ((occ_90[1] & 0x3) << 9));
  prog = (team_90[0] >> 55) | ((team_90[1] & 0x3) << 9);
  while (prog) {
    total += center_row_move_count_table[row][_tzcnt_u16(prog)];
    prog &= prog - 1;
  }

  return total;
}


//******************************************************************************
// Capture
//******************************************************************************

uint64_t foe_masks[120][2];

void gen_foe_masks() {
  int i, modDest, target;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 11;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
    if (i > 21) {
      target = i - 11;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
    if (modDest < 9) {
      target = i + 1;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
    if (modDest > 1) {
      target = i - 1;
      foe_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
  }
}

uint64_t ally_masks[120][2];

void gen_ally_masks() {
  int i, modDest, target;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 22;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
    if (i > 21) {
      target = i - 22;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
    if (modDest < 9) {
      target = i + 2;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
    if (modDest > 1) {
      target = i - 2;
      ally_masks[i][sub_layer[target]] |= ((uint64_t) 1 << (target - sub_layer_offset[target]));
    }
  }
}

void (*index_functions[120])(const layer, const layer, const layer, const layer, layer);

void apply_captures_both(const layer friends, const layer foes,
		    const layer friend_mask, const layer foe_mask, layer output) {

  const uint64_t attackers = _pext_u64(friends[0], friend_mask[0]);
  const uint64_t attackees_ext = _pext_u64(foes[0], foe_mask[0]) & attackers;
  uint64_t attackees_dep = _pdep_u64(attackees_ext, foe_mask[0]);
  output[0] -= attackees_dep;

  const uint64_t attackers_1 = _pext_u64(friends[1], friend_mask[1]);
  const uint64_t attackees_ext_1 = _pext_u64(foes[1], foe_mask[1]) & attackers_1;
  uint64_t attackees_dep_1 = _pdep_u64(attackees_ext_1, foe_mask[1]);
  output[1] -= attackees_dep_1;

  //extract indices
  int r;
  while (attackees_dep) {
    r = rotate_right[_tzcnt_u64(attackees_dep)];
    output[sub_layer[r]] |= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep = _blsr_u64(attackees_dep);
  }


  while (attackees_dep_1) {
    r = rotate_right[_tzcnt_u64(attackees_dep_1)];
    output[sub_layer[r]] |= ((uint64_t) 1 << sub_layer_offset_direct[r]);
    attackees_dep_1 = _blsr_u64(attackees_dep_1);
  }

}

void gen_index_functions() {
  int i;
  for (i = 0; i < 120; i++) {
    index_functions[i] = apply_captures_both;
  }
}

/*
 capture map

  (rotated 180)
 
  _, s, s, s, s, s, s, s, s, s, _,
  e, l, l, l, l, l, l, l, l, l, w,
  e, l, l, l, l, l, l, l, l, l, w,
  e, l, l, l, l, l, l, l, l, r, w,
  e, R, R, b, y, y, b, r, r, r, w,
  e, R, R, x, x, x, x, r, r, r, w,
  e, R, R, x, x, x, x, r, r, r, w,
  e, R, R, 1, y, y, 1, r, r, u, w,
  e, u, u, u, u, u, u, u, u, u, w,
  e, u, u, u, u, u, u, u, u, u, w,
  _, n, n, n, n, n, n, n, n, n, _,

l: capture check on [0] only
u: capture check on [1] only
n: u + north shield wall check
s: l + south shield wall check
r: rotated l
R: rotated u
e: R + east shield wall check
w: r + west shield wall check
x: capture check on both [0] and [1]
y: rotated x
b: capture check on lower, ally check on above and overlay the maybe present bit from upper position in highest spot
B: capture check on upper, ally check on ally and overlay the maybe present bit from lower position in lowest spot, shifting to make space

*/


void apply_captures(const layer friends, const layer foes, layer output, int pos) {
  index_functions[pos](friends, foes, foe_masks[pos], ally_masks[pos], output);
}

void apply_captures_niave(layer friends, layer foes, layer output, int dest) {
  int modDest = dest % 11;
  int target;
  int behind;

  //northCapture
  target = dest + 11;
  behind = dest + 22;
  if (dest < 99 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << (target - sub_layer_offset[target])) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << (behind - sub_layer_offset[behind])))
    {
      //printf("north");
      output[sub_layer[target]] &= ~((uint64_t) 1 << (target - sub_layer_offset[target]));
  }

  //southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 23 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << (target - sub_layer_offset[target])) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << (behind - sub_layer_offset[behind])))
    {
      //printf("south");
      output[sub_layer[target]] &= ~((uint64_t) 1 << (target - sub_layer_offset[target]));
  }

  //westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 8 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << (target - sub_layer_offset[target])) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << (behind - sub_layer_offset[behind])))
    {
      //printf("west\n");
      output[sub_layer[target]] &= ~((uint64_t) 1 << (target - sub_layer_offset[target]));
  }
   
  //eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 2 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << (target - sub_layer_offset[target])) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << (behind - sub_layer_offset[behind])))
    {
      //printf("east\n");
      output[sub_layer[target]] &= ~((uint64_t) 1 << (target - sub_layer_offset[target]));
  }
}

const char* mask_string =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  O  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* capture_string_1 =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* capture_string_2 =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************


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

int bench() {
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
  gen_row_moves();
  gen_center_row_moves();
  //gen_row_move_counts();
  //gen_center_row_move_counts();


  move moves[235]; // 235 is a generous max move count
  move moves_r[235]; // 235 is a generous max move count
  int total;
  get_team_moves(occ, black, occ, black, &total, moves, moves_r);
  printf("move_count: %d\n", total);


  // run for bench
  int bench_count = 5000000;
  while (bench_count) { 
    get_team_moves(occ, black, occ, black, &total, moves, moves_r);
    bench_count--;
  }
  /*
  */

  /*

  // run for result
  int total;
  total = get_team_move_count(occ, black, occ, black);
  printf("move_count: %d\n", total);


  // run for bench
  int bench_count = 25000000;
  while (bench_count) { 
    get_team_move_count(occ, black, occ, black);
    bench_count--;
  }

  */

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
}

int main(int argc, char **argv) {

  bench();

  /*

  gen_foe_masks();
  gen_ally_masks();
  gen_index_functions();

  int bench_count = 100000000;

  layer foe_mask = {0,0};
  read_layer(mask_string, 'X', foe_mask);
  layer friend_mask = {0,0};
  read_layer(mask_string, 'O', friend_mask);
  layer black = {0,0};
  read_layer(capture_string_1, 'X', black);
  layer white = {0,0};
  read_layer(capture_string_1, 'O', white);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  while (bench_count) {
    layer output = {black[0],black[1]};
    //print_layer(output);
    //printf("\n");
    //apply_captures_niave(white, white, output, 38);
    apply_captures(white, black, output, 38);
    //printf("\n");
    //print_layer(output);
    bench_count--;
  }

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
  */
  /*
  */
}
