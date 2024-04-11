#include <x86intrin.h>
#include <string>
#include "layer.h"
#include "board2.h"

using std::string;

void print_board(board board) {
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
    if (board.black[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else if (board.white[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'O';
    } else if (board.king[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = '#';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}


inline uint pp_index(uint index) {
  int output_index = 120 - index;
  int top_offset = 111;
  int rank_offset = (56 * (output_index / 11));
  int file_offset = 9 + (4 * (output_index % 11));
  int string_index = top_offset + rank_offset + file_offset;
  return string_index;
};

string pretty_fmt_board(board board) {
  string base = "     ┌─────────────────────────────────┐\n"
                " 11  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                " 10  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  9  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  8  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  7  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  6  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  5  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  4  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  3  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  2  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "  1  │ ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  · │\n"
                "     └─────────────────────────────────┘\n"
                "       a  b  c  d  e  f  g  h  i  j  k  \n\n";

  for (int board_index = 120; board_index > -1; board_index--) {
    int string_index = pp_index(board_index);

    if (board.black[sub_layer[board_index]] &
        ((uint64_t)1 << sub_layer_offset_direct[board_index])) {
      base[string_index] = 'X';
    } else if (board.white[sub_layer[board_index]] &
               ((uint64_t)1 << sub_layer_offset_direct[board_index])) {
      base[string_index] = 'O';
    } else if (board.king[sub_layer[board_index]] &
               ((uint64_t)1 << sub_layer_offset_direct[board_index])) {
      base[string_index] = '#';
    }
  }

  return base;
}
string overlay_move(string board, int orig, int dest, layer captures) {
  board[pp_index(orig) - 1] = '[';
  board[pp_index(orig) + 2] = ']';
  board[pp_index(orig)] = ' ';
  board[pp_index(dest) - 1] = '[';
  board[pp_index(dest) + 2] = ']';

  while (captures[0]) {
    board[pp_index(_tzcnt_u64(captures[0]))] = '!';
    captures[0] = _blsr_u64(captures[0]);
  }
  while (captures[1]) {
    board[pp_index(64 + _tzcnt_u64(captures[1]))] = '!';
    captures[1] = _blsr_u64(captures[1]);
  }
  return board;
};

void print_board_r(board board) {
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
    if (board.black_r[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'X';
    } else if (board.white_r[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = 'O';
    } else if (board.king_r[sub_layer[i]] & ((uint64_t) 1 << (i - sub_layer_offset[i]))) {
      string[index] = '#';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}
