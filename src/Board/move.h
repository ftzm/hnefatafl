#include <stdint.h>
#include "u128.h"

typedef struct move {
  char orig;
  char dest;
} move;

typedef struct moves_t {
  int num;
  move *moves;
} moves_t;

typedef struct board {
  u128 white_pawns;
  u128 king;
  u128 black_pawns;
} board;

typedef struct boards_dto {
  int boards_num;
  move *moves;
  board *boards;
} boards_dto;

/*
typedef struct moves_dto {
  int move_num;
  char *orig;
  char *dest;
} move;
*/

typedef struct move_board_zobrist {
  move move;
  board board;
  uint64_t zobrist;
} move_board_zobrist;

const struct u128 PAWN_ILLEGAL_DESTINATIONS = {72127962782105600,
                                         1152921504606848001};

const struct u128 WHITE_ALLIED_SQUARES = PAWN_ILLEGAL_DESTINATIONS;

const struct u128 CORNERS = {72127962782105600,
                                         1025};
const struct u128 OUTSIDE = {144080055268552710, 54069596698710015};
const struct u128 INSIDE = {~OUTSIDE.hi, ~OUTSIDE.lo};

const struct u128 START_BOARD_WHITE_PAWNS = {262592, 7784190755811098624};
const struct u128 START_BOARD_KING = {0, 1152921504606846976};
const struct u128 START_BOARD_BLACK_PAWNS = {17452548076089351, 126127186435440888};
const struct board START_BOARD = {START_BOARD_WHITE_PAWNS, START_BOARD_KING, START_BOARD_BLACK_PAWNS};
