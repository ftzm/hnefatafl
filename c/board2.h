/**
 * board.h
 *
 * The board
 */

#pragma once

#include <x86intrin.h>
#include <string>
#include "layer.h"

using std::string;

typedef struct board {
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
  unsigned char orig;
  unsigned char dest;
} move;

typedef struct moves {
  int num;
  move *moves;
} moves;

void print_board(board board);
uint pp_index(uint index);
string pretty_fmt_board(board board);
string overlay_move(string board, int orig, int dest, layer captures);
void print_board_r(board board);

inline layer corners = {1025, 72127962782105600};
