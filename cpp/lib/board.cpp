/**
 * board.cpp
 *
 * The board
 */

#pragma once

#include <x86intrin.h>
#include <string>
#include <regex>
#include <format>
#include "layer.cpp"

using std::string;


string as_notation(uint8_t position) {
  string ranks[11] = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"};
  string files[11] = {"k", "j", "i", "h", "g", "f", "e", "d", "c", "b", "a"};
  return ranks[position / 11] + files[position % 11];
}

typedef struct board {
  bool operator==(const board &rhs) const {
    return (black == rhs.black) && (white == rhs.white) && (king == rhs.king);
  }
  bool operator!=(const board &rhs) const { return !operator==(rhs); }
  layer get_occ() const {
    return black | white | king;
  };
  layer get_occ_r() const {
    return black_r | white_r | king_r;
  };
  layer black;
  layer black_r;
  layer white;
  layer white_r;
  // king can maybe also just be a char
  layer king;
  layer king_r;
} board;

constexpr board read_board(const char *string) {
  layer black = read_layer(string, 'X');
  layer white = read_layer(string, 'O');
  layer king = read_layer(string, '#');
  layer black_r = rotate_layer(black);
  layer white_r = rotate_layer(white);
  layer king_r = rotate_layer(king);
  board board = {black, black_r, white, white_r, king, king_r};
  return board;
}

typedef struct move {
  bool operator==(const move &rhs) const {
    return (orig == rhs.orig) && (dest == rhs.dest);
  }
  bool operator<(const move &rhs) const {
    return orig < rhs.orig || (orig == rhs.orig && dest < rhs.dest);
  }
  unsigned char orig;
  unsigned char dest;
} move;

typedef struct moves {
  int num;
  move *moves;
} moves;

std::ostream& operator << ( std::ostream& os, move const& value ) {
  // something about the unicode bars conflicts with the catch2
  // printing, so we replace them with slightly uglier dashes
  os << std::format("{} -> {}", as_notation(value.orig), as_notation(value.dest));
    return os;
}

constexpr inline layer corners = {1025, 72127962782105600};
constexpr inline layer not_corners = ~corners;

#define check_index(layer, i)                                                          \
  layer[sub_layer[i]] & ((uint64_t)1 << (i - sub_layer_offset[i]))

#define toggle_index(layer, i)                                                          \
  layer[sub_layer[i]] |= ((uint64_t)1 << (i - sub_layer_offset[i]))

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


inline uint pp_index_basic(uint index) {
  int output_index = 120 - index;
  int top_offset = 40;
  int rank_offset = (41 * (output_index / 11));
  int file_offset = 8 + (3 * (output_index % 11));
  int string_index = top_offset + rank_offset + file_offset;
  return string_index;
};

string basic_fmt_board(board board) {
  string base = "     +---------------------------------+\n"
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

  for (int board_index = 120; board_index > -1; board_index--) {
    int string_index = pp_index_basic(board_index);

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


string overlay_move_basic(string board, int orig, int dest, layer captures) {
  board[pp_index_basic(orig) - 1] = '[';
  board[pp_index_basic(orig) + 1] = ']';
  board[pp_index_basic(orig)] = ' ';
  board[pp_index_basic(dest) - 1] = '[';
  board[pp_index_basic(dest) + 1] = ']';

  while (captures[0]) {
    board[pp_index_basic(_tzcnt_u64(captures[0]))] = '!';
    captures[0] = _blsr_u64(captures[0]);
  }
  while (captures[1]) {
    board[pp_index_basic(64 + _tzcnt_u64(captures[1]))] = '!';
    captures[1] = _blsr_u64(captures[1]);
  }
  return board;
};

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

std::ostream& operator << ( std::ostream& os, board const& value ) {
  // something about the unicode bars conflicts with the catch2
  // printing, so we replace them with slightly uglier dashes
    // auto s = std::regex_replace(pretty_fmt_board(value), std::regex("─"), "-");
    os << basic_fmt_board(value);
    return os;
}


struct mini_board {
  board to_full() const {
    layer king_layer = {0, 0};
    king_layer[sub_layer[king]] |= (uint64_t)1 << sub_layer_offset_direct[king];
    return {black,      rotate_layer(black),     white, rotate_layer(white),
            king_layer, rotate_layer(king_layer)};
  };
  layer black;
  layer white;
  uint8_t king;
};

struct mini_board to_mini(const board b) {
  return {
    b.black, b.white,
        static_cast<uint8_t>(b.king[0] ? _tzcnt_u64(b.king[0])
                                       : _tzcnt_u64(b.king[1]) + 64)
  };
}


std::string encode_mini(mini_board input) {
  // char src[] = "hello world";
  // uint8_t (&array_of_bytes)[sizeof(layer)] =
  // char(&src)[sizeof(uint64_t)*2] = *reinterpret_cast<char(*)[sizeof(uint64_t)*2]>(input.data());
  // char(&src)[sizeof(uint64_t)] = *reinterpret_cast<char(*)[sizeof(uint64_t)]>(input[0]);


  char src[33];
  memcpy(&src, &input, sizeof(src));

  size_t srclen = 33;
  char out[44];
  size_t outlen;
  base64_encode(src, srclen, out, &outlen, 0);

  std::string output(out, outlen);
  return output;
};


struct mini_board decode_mini(std::string input) {
  size_t srclen = 49;
  char out[120];
  size_t outlen;
  base64_decode(&input[0], srclen, out, &outlen, 0);
  struct mini_board b;
  memcpy(&b, out, sizeof(b));
  return b;
};

int white_pawn_count(const board b) {
  return __builtin_popcountll(b.white[0]) + __builtin_popcountll(b.white[1]);
}

int black_pawn_count(const board b) {
  return __builtin_popcountll(b.black[0]) + __builtin_popcountll(b.black[1]);
}

const char* start_board_string = \
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

const board start_board = read_board(start_board_string);
