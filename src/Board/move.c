#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <limits.h>

#include "move.h"
#include "u128.h"

// TODO: const everywhere applicable
// TODO: hide u128 internals


//int northMoveCount(const uint64_t hi, const uint64_t lo, int pos) {
int northMoveCount(const u128 occ, const int pos) {
  int count = 0;
  int i;
  for (i = pos + 11; i < 121; i = i + 11) {
    ////printf("C: %d \n",i);
    if (u128_check_bit(occ, i)) {
      break;
    };
    count++;
  }
  ////printf("north: %d \n",count);
  return count;
}

int southMoveCount(const u128 occ, int pos) {
  int count = 0;
  int i;
  for (i = pos - 11; i >= 0; i = i - 11) {
    ////printf("C: %d \n",i);
    if (u128_check_bit(occ, i)) {
      break;
    };
    count++;
  }
  ////printf("south: %d \n",count);
  return count;
}

int eastMoveCount(const u128 occ, int pos) {
  int count = 0;
  int limit = 10 - (pos % 11);
  while (count < limit) {
    pos++;
    ////printf("east pos: %d \n",pos);
    if (u128_check_bit(occ, pos)) {
      break;
    };
    count++;
  }
  ////printf("east: %d \n",count);
  return count;
}

int westMoveCount(const u128 occ, int pos) {
  int count = 0;
  int limit = pos % 11;
  while (count < limit) {
    pos--;
    if (u128_check_bit(occ, pos)) {
      break;
    };
    count++;
  }
  ////printf("west: %d \n",count);
  return count;
}

int pieceMoveCount(const uint64_t hi, const uint64_t lo, int pos) {
  u128 occ = {hi, lo};
  return northMoveCount(occ, pos) + southMoveCount(occ, pos) + eastMoveCount(occ, pos) + westMoveCount(occ, pos);
}

int pieceMoveCount2(const u128 occ, int pos) {
  return northMoveCount(occ, pos) + southMoveCount(occ, pos) + eastMoveCount(occ, pos) + westMoveCount(occ, pos);
}

/*
moves *test_ffi() {
  move output_array[10];
  output_array[0] = {1, 2};
  output_array[1] = {55, 67};
  moves = output
  return output_moves;
}
*/

move *test_ffi_move() {
  move *m;
  m = malloc(sizeof(move));
  m->orig = 55;
  m->dest = 66;
  return m;
}

int teamMoveCount(
	      const uint64_t teamHi,
	      const uint64_t teamLo,
	      const uint64_t occHi,
	      const uint64_t occLo
	      ) {
  u128 occ = {occHi, occLo};
  u128 team = {teamHi, teamLo};
  int i, result = 0;
  for (i = 0; i < 121; i++) {
    if (u128_check_bit(team, i)) {
      result += pieceMoveCount2(occ, i);
    };
  };
  return result;
}

int team_move_count(
	      const u128 team,
	      const u128 occ
	      ) {
  int i, result = 0;
  for (i = 0; i < 121; i++) {
    if (u128_check_bit(team, i)) {
      result += pieceMoveCount2(occ, i);
    };
  };
  return result;
}

int team_move_count_2(const u128 team, const u128 occ) {
  bool north_allies[11] = {false, false, false, false, false, false,
                           false, false, false, false, false};
  int vertical_tallies[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  bool left_ally;
  int line_tally, line_index, row_index;
  int total = 0;
  int total_index = 0;
  for (row_index = 0; row_index < 11; row_index++) {
    line_tally = 0;
    bool left_ally = false;
    for (line_index = 0; line_index < 11; line_index++) {
      if (u128_check_bit(team, total_index)) {
        // encountered an ally
        // handle horizontal
        total += line_tally;
        total += line_tally * left_ally;
        line_tally = 0;
        left_ally = true;
        // handle vertical
        total += vertical_tallies[line_index];
        total += vertical_tallies[line_index] * north_allies[line_index];
        vertical_tallies[line_index] = 0;
        north_allies[line_index] = true;
        // total
      } else if (u128_check_bit(occ, total_index)) {
        // encountered an occupied (non ally) square
        // handle horizontal
        total += line_tally * left_ally;
        line_tally = 0;
        left_ally = false;
        // handle vertical
        total += vertical_tallies[line_index] * north_allies[line_index];
        vertical_tallies[line_index] = 0;
        north_allies[line_index] = false;
      } else {
        // empty square
        vertical_tallies[line_index]++;
        line_tally++;
      }
      total_index++;
      // printf("moves after row %d, column %d: %d\n", row_index, line_index, total);
      // printf("line_tally after row %d, column %d: %d\n", row_index, line_index, line_tally);
    }
    if (left_ally) {
      total += line_tally;
    }
    // printf("moves after line %d: %d\n", row_index, total);
  }
  for (line_index = 0; line_index < 11; line_index++) {
    if (north_allies[line_index]) {
      total += vertical_tallies[line_index];
    }
  }

  return total;
}

moves_t *team_moves(const uint64_t teamHi, const uint64_t teamLo,
                    const uint64_t occHi, const uint64_t occLo) {

  u128 team = {teamHi, teamLo};
  u128 occ = {occHi, occLo};

  //printf("teamHi: %d \n", teamHi);
  //printf("teamLo: %d \n", teamLo);
  //printf("occHi: %d \n", occHi);
  //printf("occLo: %d \n", occLo);

  move *moves;
  moves = (move *)malloc(sizeof(move)*400);

  int move_index = 0;


  int board_index;
  for (board_index = 0; board_index < 121; board_index++) {
    //printf("index: %d \n", board_index);
    if (u128_check_bit(team, board_index)) {

      //printf("teamLo popCount: %d \n", __builtin_popcount(teamLo));
      //printf("found piece at index: %d \n", board_index);


      // for north and south
      int i;


      // for east and west
      int count;
      int limit;
      int pos;


      //north
      for (i = board_index + 11; i < 121; i = i + 11) {
        //printf("hit north\n");
        //printf("north: %d \n", i);
	if (u128_check_bit(occ, i)) {
	  break;
	}
	moves[move_index] = (struct move) {board_index, i};
	move_index++;
        //printf("move_index: %d \n", move_index);
      }


      //printf("post north\n");

      //south
      for (i = board_index - 11; i >= 0; i = i - 11) {
        //printf("hit south\n");
	if (u128_check_bit(occ, i)) {
	  break;
	};
	moves[move_index] = (struct move) {board_index, i};
	move_index++;
      }

      //printf("post south\n");

      //east
      count = 0;
      pos = board_index;
      limit = 10 - (pos % 11);
      while (count < limit) {
	pos++;
	count++;
	if (u128_check_bit(occ, pos)) {
	  break;
	};
	moves[move_index] = (struct move) {board_index, pos};
	move_index++;
      }

      //printf("post east\n");

      //west
      count = 0;
      pos = board_index;
      limit = pos % 11;
      while (count < limit) {
	//printf("in west\n");
	pos--;
	count++;
	if (u128_check_bit(occ, pos)) {
	  break;
	};
	moves[move_index] = (struct move) {board_index, pos};
	move_index++;
      }

      //printf("post west\n");
    };
  };

  moves_t *m;
  m = malloc(sizeof(moves_t));
  m->num = move_index;
  m->moves = moves;

  //printf("moves count: %d \n",move_index);
  int i;
  for (i = 0; i < move_index; i++) {
    //printf("orig: %d \n",moves[i].orig);
    //printf("dest: %d \n",moves[i].dest);
    //printf("\n");
  }

  return m;
}

moves_t *team_moves_ptr(const uint64_t teamHi, const uint64_t teamLo,
                        const uint64_t occHi, const uint64_t occLo,
                        int *move_index, move *moves) {

  u128 team = {teamHi, teamLo};
  u128 occ = {occHi, occLo};

  //printf("teamHi: %d \n", teamHi);
  //printf("teamLo: %d \n", teamLo);
  //printf("occHi: %d \n", occHi);
  //printf("occLo: %d \n", occLo);

  *move_index = 0;

  int board_index;
  for (board_index = 0; board_index < 121; board_index++) {
    //printf("index: %d \n", board_index);
    if (u128_check_bit(team, board_index)) {

      //printf("teamLo popCount: %d \n", __builtin_popcount(teamLo));
      //printf("found piece at index: %d \n", board_index);


      // for north and south
      int i;


      // for east and west
      int count;
      int limit;
      int pos;


      //north
      for (i = board_index + 11; i < 121; i = i + 11) {
	//exclude throne
	if (i == 60) {
	  continue;
	}

        //printf("hit north\n");
        //printf("north: %d \n", i);
	if (u128_check_bit(occ, i)) {
	  break;
	}
	moves[*move_index] = (struct move) {board_index, i};
	(*move_index)++;
        //printf("move_index: %d \n", move_index);
      }


      //printf("post north\n");

      //south
      for (i = board_index - 11; i >= 0; i = i - 11) {
	//exclude throne
	if (i == 60) {
	  continue;
	}

        //printf("hit south\n");
	if (u128_check_bit(occ, i)) {
	  break;
	};
	moves[*move_index] = (struct move) {board_index, i};
	(*move_index)++;
      }

      //printf("post south\n");

      //east
      count = 0;
      pos = board_index;
      limit = 10 - (pos % 11);
      while (count < limit) {
	pos++;
	count++;

	//exclude throne
	if (pos == 60) {
	  continue;
	}

	if (u128_check_bit(occ, pos)) {
	  break;
	};
	moves[*move_index] = (struct move) {board_index, pos};
	(*move_index)++;
      }

      //printf("post east\n");

      //west
      count = 0;
      pos = board_index;
      limit = pos % 11;
      while (count < limit) {
	//printf("in west\n");
	pos--;
	count++;

	//exclude throne
	if (pos == 60) {
	  continue;
	}

	if (u128_check_bit(occ, pos)) {
	  break;
	};
	moves[*move_index] = (struct move) {board_index, pos};
	(*move_index)++;
      }

      //printf("post west\n");
    };
  };

  return 0;
}

void free_team_moves(moves_t *moves) {
  free(moves->moves);
  free(moves);
}

size_t length(const char *s) {
  size_t i = 0;
  while (*s) {
    ++i;
    ++s;
  }
  return i;
}

/*
int max(const size_t n, const int *a) {
  int m;
  size_t i;

  if (n == 0) {
    return 0;
  }

  m = a[0];
  for (i = 1; i < n; ++i) {
    if (a[i] > m) {
      m = a[i];
    }
  }

  return m;
}
*/

uint64_t bump(const uint64_t lower, const uint64_t upper) {
  return lower << 1;
}

u128 captures(const u128 friendBoard, const u128 foeBoard, const int dest) {
  int modDest = dest % 11;

  u128 captures = {0, 0};
  int target;

  //northCapture
  target = dest + 11;
  if (dest < 99 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest + 22))
    {
      u128_set_bit(&captures, target);
  }

  //southCapture
  target = dest - 11;
  if (dest > 23 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest - 22))
    {
      u128_set_bit(&captures, target);
  }

  //eastCapture
  target = dest + 1;
  if (modDest < 8 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest + 2))
    {
      u128_set_bit(&captures, target);
  }

  //westCapture
  target = dest - 1;
  if (modDest > 2 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest - 2))
    {
      u128_set_bit(&captures, target);
  }

  return captures;
}

int *captures_array(const u128 friendBoard, const u128 foeBoard, const int dest,
                    int *output, size_t *count) {

  *count = 0;
  int modDest = dest % 11;

  int target;

  //northCapture
  target = dest + 11;
  if (dest < 99 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest + 22))
    {
      output[*count] = target;
      (*count)++;
  }

  //southCapture
  target = dest - 11;
  if (dest > 23 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest - 22))
    {
      output[*count] = target;
      (*count)++;
  }

  //eastCapture
  target = dest + 1;
  if (modDest < 8 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest + 2))
    {
      output[*count] = target;
      (*count)++;
  }

  //westCapture
  target = dest - 1;
  if (modDest > 2 &&
      u128_check_bit(foeBoard, target) &&
      u128_check_bit(friendBoard, dest - 2))
    {
      output[*count] = target;
      (*count)++;
  }

  return output;
}

// ----------------------------------------------------------------------
// Port of Haskell stuff to C


void board_to_string(board board, char *string) {
  // initialize empty string
  memset(string, ' ', 373);
  string[373] = '\0';

  int i;

  // insert newlines
  for (i = 33; i < 373; i+=34) {
    string[i] = '\n';
  }

  // set board positions with the appropriate char
  for (i = 0; i < 121; i++) {
    int newline_offset = i / 11;
    int index = ((i * 3) + 1) + newline_offset;
    if (u128_check_bit(board.white_pawns, i)) {
      string[index] = 'O';
    } else if (u128_check_bit(board.king, i)) {
      string[index] = '#';
    } else if (u128_check_bit(board.black_pawns, i)) {
      string[index] = 'X';
    } else {
      string[index] = '.';
    }
  }
}

move_board_zobrist *next_move_board_zobrists_black(board board, size_t *count) {
  u128 occ = u128_or(
      PAWN_ILLEGAL_DESTINATIONS,
      u128_or(board.king, u128_or(board.white_pawns, board.black_pawns)));

  int pawn_move_count;
  move pawn_moves[400]; // consider 400 the max possible movecount for a board
  team_moves_ptr(board.black_pawns.hi, board.black_pawns.lo, occ.hi, occ.lo,
                 &pawn_move_count, pawn_moves);

  int i;

  for (i = 0; i < pawn_move_count; i++) {
    move m = pawn_moves[i];
    /*
    printf("%d\n", m.orig);
    printf("%d\n", m.dest);
    printf("\n");
    */
  };

  move_board_zobrist *output = malloc(sizeof(move_board_zobrist) * pawn_move_count);
  for (i = 0; i < pawn_move_count; i++) {
    move move = pawn_moves[i];
    struct board new_board = board;

    // clear piece from origin square and set at destination
    u128_clear_bit(&new_board.black_pawns, move.orig);
    u128_set_bit(&new_board.black_pawns, move.dest);

    // get array of capture indexes
    u128 allies = u128_or(new_board.black_pawns, u128_or(new_board.king, CORNERS));
    int captures[4];
    size_t capture_count;
    captures_array(allies, new_board.white_pawns, move.dest, captures, &capture_count);

    //remove captures from the board
    int capture;
    for (capture = 0; capture < capture_count; capture++) {
      u128_clear_bit(&new_board.white_pawns, captures[capture]);
    }

    /*
    char new_board_string[374];
    board_to_string(new_board, new_board_string);
    puts(new_board_string);
    printf("\n");
    */

    struct move_board_zobrist entry = {move, new_board, 0};
    output[i] = entry;
  };

  // handle king

  (*count) = pawn_move_count;
  return output;
}

move_board_zobrist *next_move_board_zobrists_white(board board, size_t *count) {
  // handle pawns
  int pawn_move_count;
  move pawn_moves[400]; // consider 400 the max possible movecount for a board
  u128 occ = u128_or(
      PAWN_ILLEGAL_DESTINATIONS,
      u128_or(board.king, u128_or(board.white_pawns, board.black_pawns)));

  team_moves_ptr(board.white_pawns.hi, board.white_pawns.lo, occ.hi, occ.lo,
                 &pawn_move_count, pawn_moves);

  int i;

  for (i = 0; i < pawn_move_count; i++) {
    move m = pawn_moves[i];
    /*
    printf("%d\n", m.orig);
    printf("%d\n", m.dest);
    printf("\n");
    */
  };

  move_board_zobrist *output = malloc(sizeof(move_board_zobrist) * pawn_move_count);
  for (i = 0; i < pawn_move_count; i++) {
    move move = pawn_moves[i];
    struct board new_board = board;
    // clear piece from origin square and set at destination
    u128_clear_bit(&new_board.white_pawns, move.orig);
    u128_set_bit(&new_board.white_pawns, move.dest);

    // get array of capture indexes
    u128 allies = u128_or(new_board.white_pawns, u128_or(new_board.king, WHITE_ALLIED_SQUARES));
    int captures[4];
    size_t capture_count;
    captures_array(allies, new_board.black_pawns, move.dest, captures, &capture_count);

    //remove captures from the board
    int capture;
    for (capture = 0; capture < capture_count; capture++) {
      u128_clear_bit(&new_board.black_pawns, captures[capture]);
    }

    /*
    char new_board_string[374];
    board_to_string(new_board, new_board_string);
    puts(new_board_string);
    printf("\n");
    */

    struct move_board_zobrist entry = {move, new_board, 0};
    output[i] = entry;
  };

  // handle king

  (*count) = pawn_move_count;
  return output;
}

// ----------------------------------------------------------------------

// score

int white_pawn_count(const board *board) {
  return u128_popcount(board->white_pawns);
}

int white_pawn_move_count(const board *board) {
  u128 occ = u128_or(
      PAWN_ILLEGAL_DESTINATIONS,
      u128_or(board->white_pawns, u128_or(board->king, board->black_pawns)));
  return team_move_count_2(board->white_pawns, occ);
}

int king_move_count(const board *board) {
  u128 occ = u128_or(PAWN_ILLEGAL_DESTINATIONS,
                     u128_or(board->white_pawns, board->black_pawns));
  return pieceMoveCount2(occ, u128_ctz(&(board->king)));
}

int black_pawn_count(const board *board) {
  return u128_popcount(board->black_pawns);
}

int black_pawn_move_count(const board *board) {
  u128 occ = u128_or(
      PAWN_ILLEGAL_DESTINATIONS,
      u128_or(board->white_pawns, u128_or(board->king, board->black_pawns)));
  return team_move_count_2(board->black_pawns, occ);
}

bool king_escaped(const board *board) {
  return u128_popcount(u128_and(board->king, CORNERS)) == 1;
}

bool king_captured(const board *board) {
  int king_index = u128_ctz(&(board->king));
  if (u128_popcount(u128_and(board->king, INSIDE))) {
    u128 surround_mask = u128_zero;
    u128_set_bit(&surround_mask, king_index + 1);
    u128_set_bit(&surround_mask, king_index - 1);
    u128_set_bit(&surround_mask, king_index + 11);
    u128_set_bit(&surround_mask, king_index - 11);
    return u128_popcount(u128_and(surround_mask, board->black_pawns)) == 4;
  } else {
    return false;
  }
}

int corner_protection(const board *board) {
  int nw = u128_popcount(u128_and(board->black_pawns, NORTH_WEST_GUARD));
  int ne = u128_popcount(u128_and(board->black_pawns, NORTH_EAST_GUARD));
  int sw = u128_popcount(u128_and(board->black_pawns, SOUTH_WEST_GUARD));
  int se = u128_popcount(u128_and(board->black_pawns, SOUTH_EAST_GUARD));
  return ne*ne + nw*nw + se*se + sw*sw;
}

int score_board(const board *board, const bool is_black_turn) {
  int white_score;
  if (king_escaped(board)) {
    white_score = 1000000;
  } else {
    white_score = (white_pawn_count(board) * 1000) + white_pawn_move_count(board) +
      (king_move_count(board) * 100);
  }
  int black_score;
  if (king_captured(board)) {
    black_score = 1000000;
  } else {
    black_score = (black_pawn_count(board) * 1000) +
                  black_pawn_move_count(board) +
                  corner_protection(board) * 10000;
  }
  if (is_black_turn) {
    return black_score - white_score;
  } else {
    return white_score - black_score;
  }
}

// ----------------------------------------------------------------------

u128 read_layer(const char *string) {
  u128 output = u128_zero;
  int len = strlen(string);
  int i;
  char c;
  int index = 0;
  for (i = 0; i < len; i++) {
    char c = string[i];
    if (c == '.') {
      printf("hit . index %d\n", index);
      index++;
    } else if (c == 'X') {
      printf("hit X index %d\n", index);
      u128_set_bit(&output, index);
      index++;
    } else {

      // do nothing
    }
  }
  return output;
}

#define READ_LAYER(string) ({u128 output; output = read_layer(string); output;})

const char* corners_string =
  "X  .  .  .  .  .  .  .  .  .  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  .  .  .  .  .  .  .  .  .  X";

#define CORNERS_2 READ_LAYER(corners_string)

// ----------------------------------------------------------------------

typedef int score;

typedef struct negamax_result {
  move move;
  board board;
  score score;
} negamax_result;

typedef struct scored_mbz {
  score score;
  move_board_zobrist board;
} scored_mbz;

int cmp_score(const void *a, const void *b) {
  return ((move_board_zobrist *)b)->score - ((move_board_zobrist *)a)->score;
}

negamax_result negamax_ab(move move, board board, bool is_black_turn, int depth,
                          int alpha, int beta, int *tally) {
  //printf("depth: %d\n", depth);
  if (depth == 0) {
    return (negamax_result) { move, board, score_board(&board, is_black_turn) };
  } else {
    //printf("non-zero case\n");
    size_t next_boards_count;
    move_board_zobrist *next_boards;
    //printf("getting boards\n");
    if (is_black_turn) {
      next_boards = next_move_board_zobrists_black(board, &next_boards_count);
    } else {
      next_boards = next_move_board_zobrists_white(board, &next_boards_count);
    }
    //printf("got boards\n");

    int i;
    for (i = 0; i < next_boards_count; i++) {
      next_boards[i].score = score_board(&(next_boards[i].board), is_black_turn);
    }

    qsort(next_boards, next_boards_count, sizeof(move_board_zobrist), cmp_score);

    // start with a bogus best
    negamax_result best = {move, board, INT_MIN};
    for (i = 0; i < next_boards_count; i++) {

      (*tally)++;
      move_board_zobrist next = next_boards[i];

      // calcualte result and negate score
      negamax_result next_result = negamax_ab(next.move, next.board, !is_black_turn, depth - 1, -beta, -alpha, tally);
      next_result.score = -next_result.score;

      if (next_result.score > best.score) {
	best = next_result;
      }
      if (best.score > alpha) {
	alpha = best.score;
      }
      if (alpha > beta) {
	break;
      }
    }

    //printf("free");
    free(next_boards);
    return best;
  }
}
