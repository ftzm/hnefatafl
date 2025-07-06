#include "constants.h"
#include "board.h"

const char *start_board_string = " .  .  .  X  X  X  X  X  .  .  . "
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

int white_pawn_count(const board *b) {
  return __builtin_popcountll(b->white._[0]) +
         __builtin_popcountll(b->white._[1]);
}

int black_pawn_count(const board *b) {
  return __builtin_popcountll(b->black._[0]) +
         __builtin_popcountll(b->black._[1]);
}

int boards_equal(board a, board b) {
  return (a.black._[0] == b.black._[0]) && (a.black._[1] == b.black._[1]) &&
         (a.white._[0] == b.white._[0]) && (a.white._[1] == b.white._[1]) &&
         (a.king._[0] == b.king._[0]) && (a.king._[1] == b.king._[1]);
}

inline layer board_occ(board b) {
  return (layer){
      // We can't put the throne in here becase it shouldn't function as a blocker
      b.black._[0] | b.white._[0] | b.king._[0] | corners._[0],
      b.black._[1] | b.white._[1] | b.king._[1] | corners._[1]};
}

inline layer board_occ_r(board b) {
  return (layer){
      // We can't put the throne in here becase it shouldn't function as a blocker
      b.black_r._[0] | b.white_r._[0] | b.king_r._[0] | corners._[0],
      b.black_r._[1] | b.white_r._[1] | b.king_r._[1] | corners._[1]};
}
 

inline layer king_board_occ(board b) {
  return (layer){
      b.black._[0] | b.white._[0] | b.king._[0],
      b.black._[1] | b.white._[1] | b.king._[1]};
}

inline layer king_board_occ_r(board b) {
  return (layer){
      b.black_r._[0] | b.white_r._[0] | b.king_r._[0],
      b.black_r._[1] | b.white_r._[1] | b.king_r._[1]};
}
 
