#include "board.h"
#include "stdio.h"
#include "string.h"
#include "sys/types.h"
#include "x86intrin.h"
#include "io.h"

/** This implementation reads layer strings from top left to bottom right, but starts at index 120 and works down, 
 */
layer read_layer(const char *string, uint8_t symbol) {
  layer output = EMPTY_LAYER;
  int len = strlen(string);
  int index = 120;
  for (int i = 0; i < len; i++) {
    char c = string[i];
    if (c == symbol) {
      output._[sub_layer(index)] |= ((uint64_t) 1 << sub_layer_offset_direct[index]);
      index--;
    } else if (c == ' ') {
      // skip space
    } else {
      index--; // skip other chars but increment
    }
  }
  return output;
}

layer_string stringify(layer layer) {
  // initialize empty string
  layer_string string;
  memset(string._, ' ', 373);
  string._[373] = '\0';

  // insert newlines
  for (int i = 33; i < 373; i+=34) {
    string._[i] = '\n';
  }

  // set board positions with the appropriate unsigned char
  for (int i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = 373 - (((i * 3) + 1) + newline_offset);
    if (layer._[sub_layer(i)] & ((uint64_t) 1 << sub_layer_offset_direct[i])) {
      string._[index] = 'X';
    } else {
      string._[index] = '.';
    }
  }
  return string;
}

void print_layer(layer layer) {
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
    if (layer._[sub_layer(i)] & ((uint64_t) 1 << sub_layer_offset_direct[i])) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }

  puts(string);
  printf("\n");
}

/*
Convert a position to positional notation. The output buffer needs to be 4 bytes in length.
*/ 
void as_notation(uint8_t position, char *output) {
  const char rank2[11] = {'1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1'};
  const char fileChar[11] = {'k', 'j', 'i', 'h', 'g', 'f', 'e', 'd', 'c', 'b', 'a'};
  int rank = position / 11;
  int file = position % 11;
  output[0] = (rank > 8) ? '1' : ' ';
  output[1] = rank2[rank];
  output[2] = fileChar[file];
  output[3] = '\0';
}

board read_board(const char *string) {
  layer black = read_layer(string, 'X');
  layer white = read_layer(string, 'O');
  layer king = read_layer(string, '#');
  layer black_r = rotate_layer_right(black);
  layer white_r = rotate_layer_right(white);
  layer king_r = rotate_layer_right(king);
  board board = {black, black_r, white, white_r, king, king_r};
  return board;
}


const char base[] = "     +---------------------------------+\n"
                    " 11  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    " 10  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  9  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  8  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  7  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  6  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  5  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  4  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  3  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  2  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "  1  | .  .  .  .  .  .  .  .  .  .  . |\n"
                    "     +---------------------------------+\n"
                    "       a  b  c  d  e  f  g  h  i  j  k  \n\n";

inline uint fmt_index(uint index) {
  int output_index = 120 - index;
  int top_offset = 40;
  int rank_offset = (41 * (output_index / 11));
  int file_offset = 8 + (3 * (output_index % 11));
  int string_index = top_offset + rank_offset + file_offset;
  return string_index;
};

void fmt_board(board board, char *input) {
  for (int board_index = 120; board_index > -1; board_index--) {
    int string_index = fmt_index(board_index);
    if (board.black._[sub_layer(board_index)] &
        ((uint64_t)1 << sub_layer_offset_direct[board_index])) {
      input[string_index] = 'X';
    } else if (board.white._[sub_layer(board_index)] &
               ((uint64_t)1 << sub_layer_offset_direct[board_index])) {
      input[string_index] = 'O';
    } else if (board.king._[sub_layer(board_index)] &
               ((uint64_t)1 << sub_layer_offset_direct[board_index])) {
      input[string_index] = '#';
    }
  }
}

void print_board(board b) {
  char output[strlen(base) + 1];
  strcpy(output, base);
  fmt_board(b, output);
  puts(output);
}


// TODO: new stringify read write functions which converts the the board using either sscanf or strtok+strtol
// 18446744073709551615:18446744073709551615:18446744073709551615:18446744073709551615:121


void print_row(uint16_t row) {
  char output[18];
  memset(output, '0', 17);
  output[17] = '\0';
  output[5] = '|';
  int index;
  while (row) {
    index = _tzcnt_u16(row);
    if (index > 10) index++;
    output[16 - index] = '1';
    row &= row - 1;
  }
  puts(output);
  printf("\n");
}

void overlay_move(char *board, int orig, int dest, layer captures) {
  board[fmt_index(orig) - 1] = '[';
  board[fmt_index(orig) + 1] = ']';
  board[fmt_index(orig)] = ' ';
  board[fmt_index(dest) - 1] = '[';
  board[fmt_index(dest) + 1] = ']';

  while (captures._[0]) {
    board[fmt_index(_tzcnt_u64(captures._[0]))] = '!';
    captures._[0] = _blsr_u64(captures._[0]);
  }
  while (captures._[1]) {
    board[fmt_index(64 + _tzcnt_u64(captures._[1]))] = '!';
    captures._[1] = _blsr_u64(captures._[1]);
  }
};


void print_board_move(board b, int orig, int dest, layer captures) {
    char output[strlen(base) + 1];
    strcpy(output, base);
    fmt_board(b, output);
    overlay_move(output, orig, dest, captures);
    puts(output);
}


struct move_string fmt_move(int orig, int dest) {
  struct move_string ret;
  char orig_notation[] = "   ";
  as_notation(orig, orig_notation);
  char dest_notation[] = "   ";
  as_notation(dest, dest_notation);
  snprintf(ret.buf, sizeof(ret.buf), "%s -> %s", orig_notation, dest_notation);
  return ret;
}

void print_move(int orig, int dest) { printf("%s", fmt_move(orig, dest).buf); }
