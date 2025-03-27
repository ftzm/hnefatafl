#pragma once

#include "layer.h"
#include "board.h"

layer read_layer(const char *string, uint8_t symbol);

void print_layer(layer layer);

typedef struct layer_string {
  char _[374];
} layer_string;

layer_string stringify(layer layer);

void as_notation(uint8_t position, char *output);

board read_board(const char *string);

extern const char base[];

void fmt_board(board board, char *input);

void print_board(board b);

void print_row(uint16_t row);

void overlay_move(char *board, int orig, int dest, layer captures);

void print_board_move(board b, int orig, int dest, layer captures);

struct move_string {
  char buf[12];
};

struct move_string fmt_move(int orig, int dest); 

void print_move(int orig, int dest); 
