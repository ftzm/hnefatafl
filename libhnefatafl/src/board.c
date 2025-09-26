#include "board.h"
#include "constants.h"

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

int white_pawn_count(const board *b) { return LAYER_POPCOUNT(b->white); }

int black_pawn_count(const board *b) { return LAYER_POPCOUNT(b->black); }

// todo: take reference
int boards_equal(board a, board b) {
  return LAYERS_EQUAL(a.black, b.black) && LAYERS_EQUAL(a.white, b.white) &&
         LAYERS_EQUAL(a.king, b.king);
}

// todo: take reference
inline layer board_occ(board b) {
  return (layer){{// We can't put the throne in here becase it shouldn't function
                 // as a blocker
                 b.black._[0] | b.white._[0] | b.king._[0] | corners._[0],
                 b.black._[1] | b.white._[1] | b.king._[1] | corners._[1]}};
}

// todo: take reference
inline layer board_occ_r(board b) {
  return (layer){{
      // We can't put the throne in here becase it shouldn't function as a
      // blocker
      b.black_r._[0] | b.white_r._[0] | b.king_r._[0] | corners._[0],
      b.black_r._[1] | b.white_r._[1] | b.king_r._[1] | corners._[1]}};
}

inline layer king_board_occ(board b) {
  return (layer){{
      b.black._[0] | b.white._[0] | b.king._[0],
      b.black._[1] | b.white._[1] | b.king._[1]}};
}

inline layer king_board_occ_r(board b) {
  return (layer){{
      b.black_r._[0] | b.white_r._[0] | b.king_r._[0],
      b.black_r._[1] | b.white_r._[1] | b.king_r._[1]}};
}

board rotate_board_right(board b) {
  return (board){
      .black = b.black_r,
      .black_r = rotate_layer_right(b.black_r),
      .white = b.white_r,
      .white_r = rotate_layer_right(b.white_r),
      .king = b.king_r,
      .king_r = rotate_layer_right(b.king_r)};
}

layer find_capture_destinations(
    const layer allies,
    const layer foes,
    const layer occ) {
  layer north =
      LAYER_SHIFTL_SHORT(LAYER_AND(LAYER_SHIFTL_SHORT(allies, 11), foes), 11);
  layer south = LAYER_SHIFTR(LAYER_AND(LAYER_SHIFTR(allies, 11), foes), 11);
  layer east = LAYER_SHIFTR(
      LAYER_AND(LAYER_SHIFTR(LAYER_AND(allies, drop_2_east), 1), foes),
      1);
  layer west = LAYER_SHIFTL_SHORT(
      LAYER_AND(LAYER_SHIFTL_SHORT(LAYER_AND(allies, drop_2_west), 1), foes),
      1);
  return (layer){{
      (north._[0] | south._[0] | east._[0] | west._[0]) & (~occ._[0]),
      (north._[1] | south._[1] | east._[1] | west._[1]) & (~occ._[1])}};
}
