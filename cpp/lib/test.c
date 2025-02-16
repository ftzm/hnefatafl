#include "board.h"
#include "io.h"
#include "stdio.h"

int main() {
  board start_board = read_board(start_board_string);
  print_board(start_board);
  printf("%lx:%lx:%lx:%lx", start_board.black._[0], start_board.black._[1], start_board.white._[0], start_board.white._[1]);
  printf("\n");
}
