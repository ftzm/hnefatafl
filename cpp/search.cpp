#include "board.cpp"
#include "move.cpp"
#include <climits>
#include <stdlib.h>

int white_pawn_count(const board b) {
  return __builtin_popcountll(b.black[0]) + __builtin_popcountll(b.black[1]);
}

int white_pawn_move_count(const board b) {
  return get_team_move_count(b.get_occ(), b.white, b.get_occ_r(), b.white_r);
}

int black_pawn_count(const board b) {
  return __builtin_popcountll(b.white[0]) + __builtin_popcountll(b.white[1]);
}

int black_pawn_move_count(const board b) {
  return get_team_move_count(b.get_occ(), b.black, b.get_occ_r(), b.black_r);
}

bool king_escaped(const board b) {
  return b.king[0] & corners[0] || b.king[1] & corners[1];
}

bool king_captured(const board b) {
  uint8_t king_index =
      b.king[0] ? _tzcnt_u64(b.king[0]) : _tzcnt_u64(b.king[1]) + 64;
  layer attackers = foe_masks[king_index] & b.black;
  uint8_t attacker_count =
      __builtin_popcountll(attackers[0]) + __builtin_popcountll(attackers[1]);
  return attacker_count > 3;
}

constexpr layer corner_guard = read_layer(".  .  X  .  .  .  .  .  X  .  ."
                                          ".  X  .  .  .  .  .  .  .  X  ."
                                          "X  .  .  .  .  .  .  .  .  .  X"
                                          ".  .  .  .  .  .  .  .  .  .  ."
                                          ".  .  .  .  .  .  .  .  .  .  ."
                                          ".  .  .  .  .  .  .  .  .  .  ."
                                          ".  .  .  .  .  .  .  .  .  .  ."
                                          ".  .  .  .  .  .  .  .  .  .  ."
                                          "X  .  .  .  .  .  .  .  .  .  X"
                                          ".  X  .  .  .  .  .  .  .  X  ."
                                          ".  .  X  .  .  .  .  .  X  .  .",
                                          'X');

int corner_protection(const board b) {
  layer guards = corner_guard & b.black;
  return __builtin_popcountll(guards[0]) + __builtin_popcountll(guards[1]);
}

int score_board(const board *board, const bool is_black_turn) {
  int white_score;
  if (king_escaped(*board)) {
    white_score = 1000000;
  } else {
    white_score = (white_pawn_count(*board) * 1000) +
                  white_pawn_move_count(*board) +
                  (get_king_move_count(*board) * 100);
  }

  int black_score;
  if (king_captured(*board)) {
    black_score = 1000000;
  } else {
    black_score = (black_pawn_count(*board) * 1000) +
                  black_pawn_move_count(*board) +
                  corner_protection(*board) * 10000;
  }

  if (is_black_turn) {
    return black_score - white_score;
  } else {
    return white_score - black_score;
  }
}

typedef int score;

typedef struct negamax_ab_result {
  move _move;
  board _board;
  score _score;
} negamax_ab_result;

// move moves_table[11][240];
// board boards_table[11][240];
// board scores_table[11][240];

negamax_ab_result negamax_ab(move m, board b, bool is_black_turn, int depth,
                             int alpha, int beta, int *tally) {
  if (depth == 0) {
    return (negamax_ab_result){m, b, score_board(&b, is_black_turn)};
  } else {
    int total = 0;
    move moves_table[240];
    board boards_table[240];
    if (is_black_turn) {
      get_team_moves<true>(b, &total, moves_table, boards_table);
    } else {
      get_team_moves<false>(b, &total, moves_table, boards_table);
    }

    /*
    negamax_ab_result combi[240];
    for (int i = total; i < total; i++) {
      combi[i] =
          (negamax_ab_result){moves_table[i], boards_table[i],
                              score_board(&boards_table[i], is_black_turn)};
    }
    std::sort(combi, combi+240, [](const auto& lhs, const auto& rhs) {
      return lhs.score < rhs.score;
    });
    */

    // start with a bogus best
    negamax_ab_result best = {m, b, INT_MIN};
    for (int i = 0; i < total; i++) {
      (*tally)++;

      // calcualte result and negate score
      negamax_ab_result next_result =
          negamax_ab(moves_table[i], boards_table[i], !is_black_turn, depth - 1,
                     -beta, -alpha, tally);
      next_result._score = -next_result._score;

      if (next_result._score > best._score) {
        best = next_result;
      }
      if (best._score > alpha) {
        alpha = best._score;
      }
      if (alpha > beta) {
        break;
      }
    }
    return best;
  }
}

negamax_ab_result negamax_ab_sorted(move m, board b, bool is_black_turn, int depth,
                             int alpha, int beta, int *tally) {
  if (depth == 0) {
    return (negamax_ab_result){m, b, score_board(&b, is_black_turn)};
  } else {
    int total = 0;
    move moves_table[240];
    board boards_table[240];
    if (is_black_turn) {
      get_team_moves<true>(b, &total, moves_table, boards_table);
    } else {
      get_team_moves<false>(b, &total, moves_table, boards_table);
    }

    negamax_ab_result combi[240];
    for (int i = total; i < total; i++) {
      combi[i] =
          (negamax_ab_result){moves_table[i], boards_table[i],
                              score_board(&boards_table[i], is_black_turn)};
    }
    std::sort(combi, combi+240, [](const auto& lhs, const auto& rhs) {
      return lhs._score < rhs._score;
    });

    // start with a bogus best
    negamax_ab_result best = {m, b, INT_MIN};
    for (int i = 0; i < total; i++) {
      (*tally)++;

      // calcualte result and negate score
      negamax_ab_result next_result =
          negamax_ab(combi[i]._move, combi[i]._board, !is_black_turn, depth - 1,
                     -beta, -alpha, tally);
      next_result._score = -next_result._score;

      if (next_result._score > best._score) {
        best = next_result;
      }
      if (best._score > alpha) {
        alpha = best._score;
      }
      if (alpha > beta) {
        break;
      }
    }
    return best;
  }
}

negamax_ab_result negamax_ab_runner(board b, bool is_black, int depth) {
  int tally = 0;
  auto res = negamax_ab((move) {0, 0}, b, is_black, depth, INT_MIN, INT_MAX, &tally);
  //printf("tally: %d\n", tally);
  return res;
}

negamax_ab_result negamax_ab_sorted_runner(board b, bool is_black, int depth) {
  int tally = 0;
  auto res = negamax_ab_sorted((move) {0, 0}, b, is_black, depth, INT_MIN, INT_MAX, &tally);
  //printf("tally: %d\n", tally);
  return res;
}

// ------------------------------------------------------------------
// prev:
/*

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
*/

/*
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
      next_boards[i].score = score_board(&(next_boards[i].board),
is_black_turn);
    }

    qsort(next_boards, next_boards_count, sizeof(move_board_zobrist),
cmp_score);

    // start with a bogus best
    negamax_result best = {move, board, INT_MIN};
    for (i = 0; i < next_boards_count; i++) {

      (*tally)++;
      move_board_zobrist next = next_boards[i];

      // calcualte result and negate score
      negamax_result next_result = negamax_ab(next.move, next.board,
!is_black_turn, depth - 1, -beta, -alpha, tally); next_result.score =
-next_result.score;

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
*/
