#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

typedef struct u128 {
  uint64_t hi;
  uint64_t lo;
} u128;

int checkBoardBit(const u128 board, const int bit) {
  if (bit<64) {
    return board.lo & (1 << bit);
  } else {
    return board.hi & (1 << (bit - 64));
  }
}

//int northMoveCount(const uint64_t hi, const uint64_t lo, int pos) {
int northMoveCount(const u128 occ, int pos) {
  int count = 0;
  int i;
  for (i = pos + 11; i < 121; i = i + 11) {
    //printf("C: %d \n",i);
    if (checkBoardBit(occ, i)) {
      break;
    };
    count++;
  }
  //printf("north: %d \n",count);
  return count;
}

int southMoveCount(const u128 occ, int pos) {
  int count = 0;
  int i;
  for (i = pos - 11; i >= 0; i = i - 11) {
    //printf("C: %d \n",i);
    if (checkBoardBit(occ, i)) {
      break;
    };
    count++;
  }
  //printf("south: %d \n",count);
  return count;
}

int eastMoveCount(const u128 occ, int pos) {
  int count = 0;
  int limit = 10 - (pos % 11);
  while (count < limit) {
    pos++;
    //printf("east pos: %d \n",pos);
    if (checkBoardBit(occ, pos)) {
      break;
    };
    count++;
  }
  //printf("east: %d \n",count);
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
  //printf("west: %d \n",count);
  return count;
}

int pieceMoveCount(const uint64_t hi, const uint64_t lo, int pos) {
  u128 occ = {hi, lo};
  return northMoveCount(occ, pos) + southMoveCount(occ, pos) + eastMoveCount(occ, pos) + westMoveCount(occ, pos);
}

int pieceMoveCount2(const u128 occ, int pos) {
  return northMoveCount(occ, pos) + southMoveCount(occ, pos) + eastMoveCount(occ, pos) + westMoveCount(occ, pos);
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

/*
int northMoveCount(const uint64_t hi, const uint64_t lo, int pos) {
  u128 occ = {hi, lo};
  int check = checkBoardBit(occ, pos);
  printf("C: %d \n",check);
  return pos;
}
*/

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

/*
uint64_t captures(const uint64_t friendBoard, const uint64_t friendBoard, const int dest) {
  int modDest = dest % 11;

  //northCapture
  //southCapture
  //eastCapture
  //westCapture
}
*/
