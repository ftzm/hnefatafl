#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <stdbool.h>
#include <x86intrin.h>
#include <immintrin.h>
#include <avx2intrin.h>

// TODO: remove unnecessary includes

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

/*******************************************************************************
 * Layer
 ******************************************************************************/

typedef struct layer {
  unsigned short ranks[11];
} layer;

layer layer_and(layer x, layer y) {
  return (layer){x.ranks[0] | y.ranks[0],  x.ranks[1] | y.ranks[1],
                 x.ranks[2] | y.ranks[2],  x.ranks[3] | y.ranks[3],
                 x.ranks[4] | y.ranks[4],  x.ranks[5] | y.ranks[5],
                 x.ranks[6] | y.ranks[6],  x.ranks[7] | y.ranks[7],
                 x.ranks[8] | y.ranks[8],  x.ranks[9] | y.ranks[9],
                 x.ranks[10] | y.ranks[10]};
}

layer read_layer(const char *string, unsigned char symbol) {
  int len = strlen(string);
  int index = 0;
  layer output = {0};


  for (int i = 0; i < len; i++) {
    char c = string[i];
    if (c == symbol) {
      int rank = index / 11;
      int file = index % 11;
      output.ranks[rank] |= ((unsigned short) 1 << file);
      index++;
    } else if (c == ' ') {
      // skip space
    } else {
      index++; // skip other chars but increment
    }
  }

  return output;
}

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
  int i = 0;
  for (int rank = 0; rank < 11; rank++) {
    for (int file = 0; file < 11; file++) {
      int index = ((i * 3) + 1) + rank;
      if (layer.ranks[rank] & ((uint16_t)1 << file)) {
        string[index] = 'X';
      } else {
        string[index] = '.';
      }
      i++;
    }
  }

  puts(string);
  printf("\n");
}

/*******************************************************************************
 * Board
 ******************************************************************************/

typedef struct board {
  layer black;
  layer black_r;
  layer white;
  layer white_r;
  layer king;
  layer king_r;
} board;

board read_board(const char *string) {
  board board = {
    read_layer(string, 'X'),
    read_layer(string, 'X'),
    read_layer(string, 'O'),
    read_layer(string, 'O'),
    read_layer(string, '#'),
    read_layer(string, '#')
  }; 
  /*
  rotate_layer(board.black, board.black_r);
  rotate_layer(board.white, board.white_r);
  rotate_layer(board.king, board.king_r);
  */
  return board;
}

/*******************************************************************************
 * Moves
 ******************************************************************************/

typedef struct move {
  int orig_rank;
  int orig_file;
  int dest_rank;
  int dest_file;
} move;

typedef struct move_detail {
  bool is_rank;
  uint8_t index;
  uint8_t orig;
  uint8_t dest;
  uint16_t axis_captures;
  uint16_t perp_captures;
} move_detail;

typedef struct move_line {
  uint8_t orig;
  uint8_t dest;
  uint16_t axis_captures;
  uint16_t perp_captures;
} move_line;

typedef struct move_set {
  move move;
  board board;
} move_set;

typedef struct row_moves {
  int count;
  move_line moves[20];
} row_moves;

typedef struct row_moves_reset {
  int index; 
  row_moves moves;
} row_moves_reset;

/* mirrors the structure of the moves so we can find the board associated with a move*/
typedef struct row_boards {
  uint8_t count;
  board boards[20];
} row_boards;

unsigned short get_row_moves(const unsigned short occ, const unsigned char pos) {
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
  /*
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
  */
  unsigned short lower = occ & lowers[pos];
  unsigned short upper = occ & (0b11111111110 << pos);
  unsigned short rightward = lowers[_tzcnt_u16(upper | 0x800)];
  unsigned short blocked = 0xFFFF >> __lzcnt16(lower);
  return (rightward - blocked) ^ (1 << pos);
}

/*

inline void apply_captures_niave(const layer friends, layer foes, layer output, int dest) {
  int modDest = dest % 11;
  int target;
  int behind;

  //northCapture
  target = dest + 11;
  behind = dest + 22;
  if (dest < 99 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("north");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }

  //southCapture
  target = dest - 11;
  behind = dest - 22;
  if (dest > 23 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("south");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }

  //westCapture
  target = dest + 1;
  behind = dest + 2;
  if (modDest < 8 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("west\n");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }
   
  //eastCapture
  target = dest - 1;
  behind = dest - 2;
  if (modDest > 2 &&
      foes[sub_layer[target]] & ((uint64_t) 1 << sub_layer_offset_direct[target]) &&
      friends[sub_layer[behind]] & ((uint64_t) 1 << sub_layer_offset_direct[behind]))
    {
      //printf("east\n");
      output[sub_layer[target]] &= ~((uint64_t) 1 << sub_layer_offset_direct[target]);
  }
}

*/


// void get_next_row_boards_black(const int rank, const board base_board, int *total, move *moves, board *boards) {

uint16_t get_perp_captures(uint16_t ally_row, uint16_t foe_row, int dest) {
  uint16_t victims = foe_row & (1 << (dest+1)) & (1 << (dest-1));
  uint16_t attackers = ally_row & (1 << (dest+2)) & (1 << (dest-2));
  return (uint16_t)(victims & (attackers >> 1)) | (victims & (attackers << 1));
}

void get_next_row_boards_black(const int rank, const board base_board, int *total, move_set *moves) {

  uint16_t row_moves;
  unsigned char orig, dest;
  uint16_t movers = base_board.black.ranks[rank];
  while (movers) {
    orig = _tzcnt_u16(movers);
    const uint16_t blockers = base_board.black.ranks[rank] |
                              base_board.white.ranks[rank] |
                              base_board.white.ranks[rank];
    row_moves = get_row_moves(blockers, orig);
    while (row_moves) {
      // get destination
      dest = _tzcnt_u16(row_moves);

      // Generate board
      board new_board = base_board;

      // TODO: move orig removal out and bench to see if it's faster
      new_board.black.ranks[rank] ^= (uint16_t)1 << orig;
      new_board.black.ranks[rank] |= (uint16_t)1 << dest;
      new_board.black_r.ranks[orig] ^= (uint64_t)1 << rank;
      new_board.black_r.ranks[dest] |= (uint64_t)1 << rank;

      // TODO: capture

      // register move
      moves[*total] = (struct move_set){(struct move){rank, orig, rank, dest}, new_board};
      (*total)++;

      // decrement
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}

row_moves get_next_row_moves_black(const int rank, const board base_board) {
  // print_row(base_board.black.ranks[rank]);
  // print_row(base_board.white.ranks[rank]);
  int current;
  // printf("total: %d\n", *total);

  row_moves output = {0};

  bool should_add = false;
  bool can_attack = false; // the previous piece was black
  bool can_capture = false; // the previous piece was white and behind a black
  for (int file = 0; file < 11; file++) {
    if (base_board.black.ranks[rank] & (1 << file)) {
      // printf("black\n");
      current = file;
      if (can_capture) {
	output.moves[output.count].axis_captures |= (1 << (file - 1));
      }
      should_add = true;
    } else if (base_board.white.ranks[rank] & (1 << file)) {
      // printf("white\n");
      should_add = false;
      can_capture = can_attack;
      can_attack = false;
    } else if (should_add) {
      can_attack = true;
      output.moves[output.count] = (struct move_line){current, file};
      output.moves[output.count].perp_captures = get_perp_captures(base_board.black.ranks[file], base_board.white.ranks[file], rank);
      output.count++;
    } else {
      // printf("empty unreachable\n");
    }
    // printf("\n");
  }

  should_add = false;
  can_attack = false; // the previous piece was black
  can_capture = false; // the previous piece was white and behind a black
  for (int file = 10; file >= 0; file--) {
    if (base_board.black.ranks[rank] & (1 << file)) {
      // printf("black\n");
      current = file;
      if (can_capture) {
	output.moves[output.count].axis_captures |= (1 << (file + 1));
      }
      should_add = true;
    } else if (base_board.white.ranks[rank] & (1 << file)) {
      // printf("white\n");
      should_add = false;
      can_capture = can_attack;
      can_attack = false;
    } else if (should_add) {
      // printf("row_move_count: %d\n", row_move_count);
      can_attack = true;
      output.moves[output.count] = (struct move_line){current, file};
      output.moves[output.count].perp_captures = get_perp_captures(base_board.black.ranks[file], base_board.white.ranks[file], rank);
      output.count++;
    } else {
      // printf("empty unreachable\n");
    }
    // printf("\n");
  }
  // printf("---------------------------------------------------\n");
  return output;
}

void get_next_row_boards_black_s(const int rank, const board base_board, row_moves moves[22], row_boards *boards) {
  // print_row(base_board.black.ranks[rank]);
  // print_row(base_board.white.ranks[rank]);
  int current;
  int row_move_count = 0;
  // printf("total: %d\n", *total);

  bool should_add = false;
  bool can_attack = false; // the previous piece was black
  bool can_capture = false; // the previous piece was white and behind a black
  for (int file = 0; file < 11; file++) {
    if (base_board.black.ranks[rank] & (1 << file)) {
      // printf("black\n");
      current = file;
      if (can_capture) {
	moves[rank].moves[row_move_count].axis_captures |= (1 << (file +1));
      }
      should_add = true;
    } else if (base_board.white.ranks[rank] & (1 << file)) {
      // printf("white\n");
      should_add = false;
      can_capture = can_attack;
      can_attack = false;
    } else if (should_add) {
      can_attack = true;
      // printf("empty dest\n");
      board new_board = base_board;

      // TODO: move orig removal out and bench to see if it's faster
      new_board.black.ranks[rank] ^= (uint16_t)1 << current;
      new_board.black.ranks[rank] |= (uint16_t)1 << file;
      new_board.black_r.ranks[current] ^= (uint64_t)1 << rank;
      new_board.black_r.ranks[file] |= (uint64_t)1 << rank;

      // TODO: capture

      moves[rank].moves[row_move_count] = (struct move_line){current, file};
      moves[rank].moves[row_move_count].perp_captures = get_perp_captures(base_board.black.ranks[file], base_board.white.ranks[file], rank);
      boards[rank].boards[row_move_count] = new_board;
      row_move_count++;
    } else {
      // printf("empty unreachable\n");
    }
    // printf("\n");
  }

  should_add = false;
  can_attack = false; // the previous piece was black
  can_capture = false; // the previous piece was white and behind a black
  for (int file = 10; file >= 0; file--) {
    if (base_board.black.ranks[rank] & (1 << file)) {
      // printf("black\n");
      current = file;
      if (can_capture) {
	moves[rank].moves[row_move_count].axis_captures |= (1 << (file -1));
      }
      should_add = true;
    } else if (base_board.white.ranks[rank] & (1 << file)) {
      // printf("white\n");
      should_add = false;
      can_capture = can_attack;
      can_attack = false;
    } else if (should_add) {
      can_attack = true;
      // printf("empty dest\n");
      board new_board = base_board;

      // TODO: move orig removal out and bench to see if it's faster
      new_board.black.ranks[current] ^= (uint16_t)1 << rank;
      new_board.black.ranks[file] |= (uint16_t)1 << rank;
      new_board.black_r.ranks[rank] ^= (uint16_t)1 << current;
      new_board.black_r.ranks[rank] |= (uint16_t)1 << file;

      // TODO: capture

      // printf("row_move_count: %d\n", row_move_count);
      moves[rank].moves[row_move_count] = (struct move_line){current, file};
      moves[rank].moves[row_move_count].perp_captures = get_perp_captures(base_board.black.ranks[file], base_board.white.ranks[file], rank);
      boards[rank].boards[row_move_count] = new_board;
      row_move_count++;
    } else {
      // printf("empty unreachable\n");
    }
    // printf("\n");
  }
  moves[rank].count = row_move_count;
  // printf("---------------------------------------------------\n");
}

void get_next_row_boards_black_r_s(const int rank, const board base_board, row_moves *moves, row_boards *boards, int row_index) {
  // print_row(base_board.black.ranks[rank]);
  // print_row(base_board.white.ranks[rank]);
  int current;
  int row_move_count = 0;

  int stack[10];
  int stack_count = 0;
  bool should_add = false;
  for (int file = 0; file < 11; file++) {
    if (base_board.black.ranks[rank] & (1 << file)) {
      // printf("black\n");
      current = file;
      should_add = true;
    } else if (base_board.white.ranks[rank] & (1 << file)) {
      // printf("white\n");
      should_add = false;
    } else if (should_add) {
      // printf("empty dest\n");
      board new_board = base_board;

      // TODO: move orig removal out and bench to see if it's faster
      new_board.black.ranks[current] ^= (uint16_t)1 << rank;
      new_board.black.ranks[file] |= (uint16_t)1 << rank;
      new_board.black_r.ranks[rank] ^= (uint16_t)1 << current;
      new_board.black_r.ranks[rank] |= (uint16_t)1 << file;

      // TODO: capture

      moves[row_index].moves[row_move_count] = (struct move_line){current, file};
      moves[rank].moves[row_move_count].perp_captures = get_perp_captures(base_board.black.ranks[file], base_board.white.ranks[file], rank);
      boards[row_index].boards[row_move_count] = new_board;
      row_move_count++;
    } else {
      // printf("empty unreachable\n");
    }
    // printf("\n");
  }

  should_add = false;
  for (int file = 10; file >= 0; file--) {
    if (base_board.black.ranks[rank] & (1 << file)) {
      // printf("black\n");
      current = file;
      should_add = true;
    } else if (base_board.white.ranks[rank] & (1 << file)) {
      // printf("white\n");
      should_add = false;
    } else if (should_add) {
      // printf("empty dest\n");
      board new_board = base_board;

      // TODO: move orig removal out and bench to see if it's faster
      new_board.black.ranks[current] ^= (uint16_t)1 << rank;
      new_board.black.ranks[file] |= (uint16_t)1 << rank;
      new_board.black_r.ranks[rank] ^= (uint16_t)1 << current;
      new_board.black_r.ranks[rank] |= (uint16_t)1 << file;

      // TODO: capture

      moves[row_index].moves[row_move_count] = (struct move_line){current, file};
      moves[rank].moves[row_move_count].perp_captures = get_perp_captures(base_board.black.ranks[file], base_board.white.ranks[file], rank);
      boards[row_index].boards[row_move_count] = new_board;
      row_move_count++;
    } else {
      // printf("empty unreachable\n");
    }
    // printf("\n");
  }
  // printf("---------------------------------------------------\n");
  moves[row_index].count = row_move_count;
}

// void get_next_row_boards_black_r(const int rank, const board base_board, int *total, move *moves, board *boards) {
void get_next_row_boards_black_r(const int rank, const board base_board, int *total, move_set *moves) {

  uint16_t row_moves;
  unsigned char orig, dest;
  uint16_t movers = base_board.black_r.ranks[rank];
  while (movers) {
    orig = _tzcnt_u16(movers);
    const uint16_t blockers = base_board.black_r.ranks[rank] |
                              base_board.white_r.ranks[rank] |
                              base_board.white_r.ranks[rank];
    row_moves = get_row_moves(blockers, orig);
    while (row_moves) {
      // get destination
      dest = _tzcnt_u16(row_moves);

      // register move
      // moves[*total] = (struct move){orig, rank, dest, rank};

      // Generate board
      board new_board = base_board;

      // TODO: move orig removal out and bench to see if it's faster
      new_board.black.ranks[orig] ^= (uint16_t)1 << rank;
      new_board.black.ranks[dest] |= (uint16_t)1 << rank;
      new_board.black_r.ranks[rank] ^= (uint16_t)1 << orig;
      new_board.black_r.ranks[rank] |= (uint16_t)1 << dest;

      // TODO: capture


      moves[*total] = (struct move_set){(struct move){rank, orig, rank, dest}, new_board};
      // boards[*total] = new_board;
      (*total)++;

      // decrement
      row_moves &= row_moves - 1;
    }
    movers &= movers - 1;
  }
}


void get_team_moves_black(const board current, int *total, row_moves moves[22], row_boards boards[22]) {
  int start_total = *total;

  get_next_row_boards_black_s(0, current, moves, boards);
  get_next_row_boards_black_s(1, current, moves, boards);
  get_next_row_boards_black_s(2, current, moves, boards);
  get_next_row_boards_black_s(3, current, moves, boards);
  get_next_row_boards_black_s(4, current, moves, boards);
  get_next_row_boards_black_s(5, current, moves, boards);
  get_next_row_boards_black_s(6, current, moves, boards);
  get_next_row_boards_black_s(7, current, moves, boards);
  get_next_row_boards_black_s(8, current, moves, boards);
  get_next_row_boards_black_s(9, current, moves, boards);
  get_next_row_boards_black_s(10, current, moves, boards);

  get_next_row_boards_black_r_s(0, current, moves, boards, 11);
  get_next_row_boards_black_r_s(1, current, moves, boards, 12);
  get_next_row_boards_black_r_s(2, current, moves, boards, 13);
  get_next_row_boards_black_r_s(3, current, moves, boards, 14);
  get_next_row_boards_black_r_s(4, current, moves, boards, 15);
  get_next_row_boards_black_r_s(5, current, moves, boards, 16);
  get_next_row_boards_black_r_s(6, current, moves, boards, 17);
  get_next_row_boards_black_r_s(7, current, moves, boards, 18);
  get_next_row_boards_black_r_s(8, current, moves, boards, 19);
  get_next_row_boards_black_r_s(9, current, moves, boards, 20);
  get_next_row_boards_black_r_s(10, current, moves, boards, 21);

  /*
  for (int i = start_total; i < *total; i++) {
    move m = moves[i];
    board new_board = current;
    new_board.black.ranks[m.orig_file] ^= (uint16_t)1 << m.orig_rank;
    new_board.black.ranks[m.dest_file] |= (uint16_t)1 << m.dest_rank;
    new_board.black_r.ranks[m.orig_rank] ^= (uint16_t)1 << m.orig_file;
    new_board.black_r.ranks[m.dest_rank] |= (uint16_t)1 << m.dest_file;
    boards[i] = new_board;
  }
  */
}

void update_moves(board board, row_moves black_moves[22], row_moves white_moves[22], move_detail move, int *reset_count, row_moves_reset *black_resets, row_moves_reset *white_resets) {
  int axis_offset = move.is_rank ? 0 : 11;
  int intersect_offset = move.is_rank ? 11 : 0;

  // axis moves
  int axis_index = axis_offset + move.index;
  black_resets[0] = (row_moves_reset){axis_index, black_moves[axis_index]};
  black_moves[axis_offset] = get_next_row_moves_black(move.index, board);
  // mocked white moves
  white_resets[0] = (row_moves_reset){axis_index, white_moves[axis_index]};
  white_moves[axis_offset] = get_next_row_moves_black(move.index, board);

  // intersect moves
  int orig_index = intersect_offset + move.orig;
  black_resets[1] = (row_moves_reset){orig_index, black_moves[orig_index]};
  black_moves[orig_index] = get_next_row_moves_black(move.orig, board);
  // mocked white moves
  white_resets[1] = (row_moves_reset){orig_index, white_moves[orig_index]};
  white_moves[orig_index] = get_next_row_moves_black(move.orig, board);

  int dest_index = intersect_offset + move.dest;
  black_resets[2] = (row_moves_reset){dest_index, black_moves[dest_index]};
  black_moves[dest_index] = get_next_row_moves_black(move.dest, board);
  // mocked white moves
  white_resets[2] = (row_moves_reset){dest_index, white_moves[dest_index]};
  white_moves[dest_index] = get_next_row_moves_black(move.dest, board);

  (*reset_count) = 3;

  if (move.axis_captures) {
    black_moves[axis_offset] = get_next_row_moves_black(move.index, board);
    black_resets[4] = (row_moves_reset){axis_index, black_moves[axis_index]};
    black_resets[4] = (row_moves_reset){axis_index, black_moves[axis_index]};
    (*reset_count)++;
  }

  if (move.perp_captures) {
    black_moves[axis_offset] = get_next_row_moves_black(move.index, board);
    black_resets[4] = (row_moves_reset){axis_index, black_moves[axis_index]};
    black_resets[4] = (row_moves_reset){axis_index, black_moves[axis_index]};
    (*reset_count)++;
  }

}

void apply_resets(row_moves moves[22], int reset_count, row_moves_reset *resets) {
  while (reset_count) {

    row_moves_reset reset = resets[--reset_count]; 
    moves[reset.index] = reset.moves;
  }
}

void get_team_moves_black_reuse(const board current, row_moves moves[22], row_boards *boards) {
  for (int i = 0; i < 22; i++) {
    for (int j = 0; j < moves[i].count; j++) {
      move_line m = moves[i].moves[j];
      board new_board = current;
      new_board.black.ranks[i] ^= (uint16_t)1 << m.orig;
      new_board.black.ranks[i] |= (uint16_t)1 << m.dest;
      new_board.black.ranks[i] &= ~m.axis_captures;
      new_board.black_r.ranks[m.orig] ^= (uint16_t)1 << (i - 11);
      new_board.black_r.ranks[m.dest] |= (uint16_t)1 << (i - 11);
      new_board.black_r.ranks[m.dest] &= ~m.perp_captures;
      boards[i].boards[j] = new_board;
    }
  }
}


int main() {
  layer black = read_layer(start_board_string, 'X');
  layer white = read_layer(start_board_string, 'O');
  layer occ = layer_and(black, white);

  row_moves black_moves[22] = {0}; 
  row_moves white_moves[22] = {0}; 
  row_boards boards[22]; 
  row_boards boards_2[22]; 
  board start_board = read_board(start_board_string);
  int total;

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  // run for bench
  int bench_count = 1000;

  int sum = 0;
  while (bench_count) {
    total = 0;
    get_team_moves_black(start_board, &total, black_moves, boards);
    get_team_moves_black(start_board, &total, white_moves, boards);

    for (int i = 0; i < 22; i++) {
      for (int j = 0; j < black_moves[i].count; j++) {
	move_line move = black_moves[i].moves[j];
	move_detail md = {(i < 11 ? true : false), (i < 11 ? i : i - 11), move.orig, move.dest};
        int reset_count = 0;
	row_moves_reset black_resets[7];
	row_moves_reset white_resets[7];
	board board = boards[i].boards[j];
	update_moves(board, black_moves, white_moves, md, &reset_count, black_resets, white_resets);

	// count moves and sum
        for (int i = 0; i < 22; i++) {
	  sum += black_moves[i].count;
        }

        get_team_moves_black_reuse(board, black_moves, boards_2);
        apply_resets(black_moves, reset_count, black_resets);
        apply_resets(white_moves, reset_count, white_resets);
      }
    }
    bench_count--;
  }

  printf("all moves: %d\n", sum);
  printf("total: %d\n", total);

  /*
  for (int i = 0; i < total; i++) {
    printf("--------------------------------------------------\n");
    print_layer(moves[i].board.black);
    // print_layer(moves[i].board.black_r);
  }
  */

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
  // print_layer(black);

  printf("board count: %d\n", total);

}

/* Notes

   move line has axis captures and perp captures, each of which are uint16_t
reps of captures. that way the moves function can account for it by doing a "while (perp_captures)" etc. Adds two extra checks to each move handling func, might be slow.

   
 */
