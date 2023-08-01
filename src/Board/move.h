#include <stdint.h>

typedef struct move {
  char orig;
  char dest;
} move;

typedef struct moves_t {
  int num;
  move *moves;
} moves_t;

typedef struct u128 {
  uint64_t hi;
  uint64_t lo;
} u128;

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
