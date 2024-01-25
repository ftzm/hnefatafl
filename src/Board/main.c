#include <stdio.h>
#include <assert.h>
#include <locale.h>
#include "move.c"

  /*
  for (r = 0; r < count; r++) {
    struct move_board_zobrist result = move_board_zobrists_white[r];
    char r_board_string[374];
    board_to_string(result.board, r_board_string);
    puts(r_board_string);
    printf("\n");
  }
  // */

int main(int argc, char **argv) {
  printf("Running test\n");

  //print startBoard
  char board_string[374];
  board_to_string(START_BOARD, board_string);
  puts(board_string);

  //nextMoveBoardZobristsWhite
  size_t count;
  move_board_zobrist *move_board_zobrists_white = next_move_board_zobrists_white(START_BOARD, &count);
  printf("%ld\n", count);
  int r;
  //printf("%ld", count);

  //nextMoveBoardZobristsBlack
  move_board_zobrist *move_board_zobrists_black = next_move_board_zobrists_black(START_BOARD, &count);
  printf("%ld\n", count);
  //printf("%ld", count);

  //test score
  int start_board_score_black = score_board(&START_BOARD, true);
  printf("%d\n", start_board_score_black);
  int start_board_score_white = score_board(&START_BOARD, false);
  printf("%d\n", start_board_score_white);

  //test negamax
  int tally = 0;
  negamax_result negamax_result = negamax_ab((move){0, 0}, START_BOARD,true, 5, INT_MIN, INT_MAX, &tally);
  printf("score: %d\n", negamax_result.score);
  setlocale(LC_NUMERIC, "");
  printf("tally: %'d\n", tally);

  // black team move count

  printf("\n\n\n\n\n");
  u128 occ = u128_or(
      PAWN_ILLEGAL_DESTINATIONS,
      u128_or(START_BOARD.white_pawns, u128_or(START_BOARD.king, START_BOARD.black_pawns)));
  int black_count =  team_move_count_2(START_BOARD.black_pawns, occ);
  printf("black_count 2: %d\n", black_count);

  int black_count_orig =  team_move_count(START_BOARD.black_pawns, occ);
  printf("black_count orig: %d\n", black_count_orig);


  // white team move count
  int white_count =  team_move_count_2(START_BOARD.white_pawns, occ);
  printf("white_count 2: %d\n", white_count);

  int white_count_orig =  team_move_count(START_BOARD.white_pawns, occ);
  printf("white_count orig: %d\n", white_count_orig);

  //test constant

  /*
  printf("corners hi: %ld, lo: %ld\n", CORNERS_2.hi, CORNERS_2.lo);
  char tb[374];
  board_to_string((board){u128_zero, u128_zero, CORNERS_2}, tb);
  puts(tb);


  u128 t2 = read_layer(corners_string);
  char tb2[374];
  board_to_string((board){u128_zero, u128_zero, t2}, tb2);
  puts(tb2);
  */

  return 0;
}

typedef struct layer {
  uint64_t lo;
  uint15_t mid;
  uint64_t hi;
}
