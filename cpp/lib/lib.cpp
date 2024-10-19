// #include "lib.h"
#include "board.cpp"

extern "C" {

  struct move_result {
    move m;
    board b;
    layer c;
    bool game_over; 
  };

void board_to_code(char *code, struct mini_board *b) {
  string str = encode_mini(*b);
  memcpy(code, str.c_str(), 44);
}

void start_board_extern(struct mini_board *b) {
  *b = to_mini(start_board);
}
}
