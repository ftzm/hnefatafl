#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "move.h"

uint64_t checkBoardBit(const u128 board, const int bit) {
  ////printf("checkBoardBit: %d \n", bit);
  if (bit<64) {
    return board.lo & ((uint64_t) 1 << bit);
  } else {
    return board.hi & ((uint64_t) 1 << (bit - 64));
  }
}

void setBoardBit(u128 *board, const int bit) {
  if (bit<64) {
    board->lo = board->lo | ((uint64_t) 1 << bit);
  } else {
    board->hi = board->hi | ((uint64_t)1 << (bit - 64));
  }
}

//int northMoveCount(const uint64_t hi, const uint64_t lo, int pos) {
int northMoveCount(const u128 occ, const int pos) {
  int count = 0;
  int i;
  for (i = pos + 11; i < 121; i = i + 11) {
    ////printf("C: %d \n",i);
    if (checkBoardBit(occ, i)) {
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
    if (checkBoardBit(occ, i)) {
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
    if (checkBoardBit(occ, pos)) {
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
    if (checkBoardBit(occ, pos)) {
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
    if (checkBoardBit(team, i)) {
      result += pieceMoveCount2(occ, i);
    };
  };
  return result;
}

moves_t *team_moves(
	      const uint64_t teamHi,
	      const uint64_t teamLo,
	      const uint64_t occHi,
	      const uint64_t occLo
	      ) {

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
    if (checkBoardBit(team, board_index)) {

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
	if (checkBoardBit(occ, i)) {
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
	if (checkBoardBit(occ, i)) {
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
	if (checkBoardBit(occ, pos)) {
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
	if (checkBoardBit(occ, pos)) {
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

moves_t *team_moves_ptr(
	      const uint64_t teamHi,
	      const uint64_t teamLo,
	      const uint64_t occHi,
	      const uint64_t occLo,
	      int *move_index,
	      move *moves
	      ) {

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
    if (checkBoardBit(team, board_index)) {

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
	if (checkBoardBit(occ, i)) {
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
	if (checkBoardBit(occ, i)) {
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

	if (checkBoardBit(occ, pos)) {
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

	if (checkBoardBit(occ, pos)) {
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

void *free_team_moves(moves_t *moves) {
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
      checkBoardBit(foeBoard, target) &&
      checkBoardBit(friendBoard, dest + 22))
    {
      setBoardBit(&captures, target);
  }

  //southCapture
  target = dest - 11;
  if (dest > 23 &&
      checkBoardBit(foeBoard, target) &&
      checkBoardBit(friendBoard, dest - 22))
    {
      setBoardBit(&captures, target);
  }

  //eastCapture
  target = dest + 1;
  if (modDest < 8 &&
      checkBoardBit(foeBoard, target) &&
      checkBoardBit(friendBoard, dest + 2))
    {
      setBoardBit(&captures, target);
  }

  //westCapture
  target = dest - 1;
  if (modDest > 2 &&
      checkBoardBit(foeBoard, target) &&
      checkBoardBit(friendBoard, dest - 2))
    {
      setBoardBit(&captures, target);
  }

  return captures;
}
