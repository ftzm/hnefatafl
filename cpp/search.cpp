#include "board.cpp"
#include "move.cpp"
#include <climits>
#include <climits>

enum PieceType : uint8_t {
  black_type = 1,
  white_type = 2,
  king_type = 3,
};

int32_t BLACK_PAWN_VALUE = 10000;
int32_t WHITE_PAWN_VALUE = 10000;

struct score_state {
  int32_t get_score(bool is_black_turn) {
    int32_t score_as_black = guard_score +
                             (BLACK_PAWN_VALUE * black_pawn_count) -
                             (WHITE_PAWN_VALUE * white_pawn_count);
    return is_black_turn ? score_as_black : -score_as_black;
  };
  uint8_t nw_guard_count;
  uint8_t ne_guard_count;
  uint8_t sw_guard_count;
  uint8_t se_guard_count;
  int32_t guard_score;
  uint8_t black_pawn_count;
  uint8_t white_pawn_count;
};

constexpr layer corner_guard_nw = read_layer(".  .  X  .  .  .  .  .  .  .  ."
                                             ".  X  .  .  .  .  .  .  .  .  ."
                                             "X  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  .",
                                             'X');

constexpr layer corner_guard_ne = read_layer(".  .  .  .  .  .  .  .  X  .  ."
                                             ".  .  .  .  .  .  .  .  .  X  ."
                                             ".  .  .  .  .  .  .  .  .  .  X"
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  .",
                                             'X');

constexpr layer corner_guard_sw = read_layer(".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             "X  .  .  .  .  .  .  .  .  .  ."
                                             ".  X  .  .  .  .  .  .  .  .  ."
                                             ".  .  X  .  .  .  .  .  .  .  .",
                                             'X');

constexpr layer corner_guard_se = read_layer(".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  ."
                                             ".  .  .  .  .  .  .  .  .  .  X"
                                             ".  .  .  .  .  .  .  .  .  X  ."
                                             ".  .  .  .  .  .  .  .  X  .  .",
                                             'X');

uint8_t nw_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_nw[1] & b.black[1]);
}

uint8_t ne_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_ne[1] & b.black[1]);
}

uint8_t sw_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_sw[0] & b.black[0]);
}

uint8_t se_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_se[0] & b.black[0]);
}

score_state init_score_state(const board b) {
  return {
    nw_corner_protection(b),    
    ne_corner_protection(b),    
    sw_corner_protection(b),    
    se_corner_protection(b),    
    0,
    static_cast<uint8_t>(black_pawn_count(b)),
    static_cast<uint8_t>(white_pawn_count(b)),
  };
}

int32_t FIRST_GUARD_BONUS = 100;
int32_t SECOND_GUARD_BONUS = 200;
int32_t THIRD_GUARD_BONUS = 600;

void update_guard_score_state(score_state &s, move m) {
  int32_t count_bonuses[] = {100, 200, 600};
  switch (m.orig) {
  // nw
  case 118:
  case 108:
  case 98:
    s.guard_score -= count_bonuses[--s.nw_guard_count];
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s.guard_score -= count_bonuses[--s.ne_guard_count];
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s.guard_score -= count_bonuses[--s.sw_guard_count];
    break;
  // se
  case 22:
  case 12:
  case 2:
    s.guard_score -= count_bonuses[--s.se_guard_count];
    break;
  }
  switch (m.dest) {
  // nw
  case 118:
  case 108:
  case 98:
    s.guard_score += count_bonuses[s.nw_guard_count++];
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s.guard_score += count_bonuses[s.ne_guard_count++];
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s.guard_score += count_bonuses[s.sw_guard_count++];
    break;
  // se
  case 22:
  case 12:
  case 2:
    s.guard_score += count_bonuses[s.se_guard_count++];
    break;
  }
};

score_state update_score_state(score_state old_s, move m, PieceType t) {
  score_state s = old_s;
  if (t == black_type) {
    update_guard_score_state(s, m);
  }
  return s;
}

int white_pawn_move_count(const board b) {
  return get_team_move_count(b.get_occ(), b.white, b.get_occ_r(), b.white_r);
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

uint32_t CORNER_PROTECTION_BONUS = 250;

int corner_protection(const board b) {
  return (nw_corner_protection(b) * CORNER_PROTECTION_BONUS) +
         (ne_corner_protection(b) * CORNER_PROTECTION_BONUS) +
         (sw_corner_protection(b) * CORNER_PROTECTION_BONUS) +
         (se_corner_protection(b) * CORNER_PROTECTION_BONUS);
}

// typedef int32_t score;
static const int32_t MIN_SCORE = -INT_MAX;
static const int32_t MAX_SCORE = INT_MAX;

int32_t score_board(const board *board, const bool is_black_turn) {
  int32_t white_score = //(white_pawn_count(*board) * 10000) +
                        white_pawn_move_count(*board) +
                        (get_king_move_count(*board) * 100);

  int32_t black_score = //(black_pawn_count(*board) * 10000) +
                        black_pawn_move_count(*board);// +
                        //corner_protection(*board);

  return is_black_turn ? black_score - white_score : white_score - black_score;
}

bool game_over_check(const board &b, bool is_black_turn, int32_t &score) {
  if (king_escaped(b)) {
    score = is_black_turn ? MIN_SCORE : MAX_SCORE;
    return true;
  } else if (king_captured(b)) {
    score = is_black_turn ? MAX_SCORE : MIN_SCORE;
    return true;
  }
  return false;
}

int32_t score_board_for_order(const board *board, const bool is_black_turn) {
  int32_t white_score = white_pawn_count(*board) * 1000;

  int32_t black_score =
      (black_pawn_count(*board) * 1000) + corner_protection(*board) * 100;

  return is_black_turn ? black_score - white_score : white_score - black_score;
}

typedef struct negamax_ab_result {
  move _move;
  board _board;
  int32_t _score;
  uint64_t zobrist;
  score_state ss;
} negamax_ab_result;

// move moves_table[11][240];
// board boards_table[11][240];
// board scores_table[11][240];
const int MAX_DEPTH = 32;
int PV_LENGTH[MAX_DEPTH];
move PV_TABLE[MAX_DEPTH][MAX_DEPTH];
board PV_TABLE_BOARDS[MAX_DEPTH][MAX_DEPTH];
move PREV_PV[MAX_DEPTH];
int PREV_PV_LENGTH;
move KILLER_MOVES[MAX_DEPTH][2];

int32_t negamax_ab_sorted_pv(const move m, const board b, bool is_black_turn,
                                       const int depth, const int ply, int32_t alpha, int32_t beta,
                             int *tally, bool is_pv, score_state ss, bool allow_null_move) {
  PV_LENGTH[ply] = ply;

  int32_t game_over_score = 0;
  bool game_over = game_over_check(b, is_black_turn, game_over_score);
  if (game_over) {
    return game_over_score;
  }

  int total = 0;
  move moves_table[235];
  board boards_table[235];
  uint8_t cap_counts[235] = {0};

  if (depth > 3 && ply > 0 && allow_null_move && !is_pv) {
    // Null move heuristic
    // TODO: figure out the most suitable depth cutoff (if any)
    // TODO: add check to see if the static evaluation of the board is above
    // beta
    // TODO: ensure king is not in check/on verge of escape, otherwise this
    // will be unsound
    int null_shortening = 2;
    int32_t null_result = -negamax_ab_sorted_pv(
        m, b, !is_black_turn, depth - 1 - null_shortening,
        ply + 1 + null_shortening, -beta, -beta + 1, tally, false, ss, false);
    if (null_result >= beta) {
      return beta;
    }
  }
  /*
  */

  if (depth <= 0) {
    if (ply < MAX_DEPTH) {
      if (is_black_turn) {
        get_capture_move_boards<true>(boards_table, b, &total, moves_table,
                                      cap_counts);
      } else {
        get_capture_move_boards<false>(boards_table, b, &total, moves_table,
                                       cap_counts);
      }
    }
    if (total == 0) {
      return score_board(&b, is_black_turn) + ss.get_score(is_black_turn);
    }
  } else {

    if (is_black_turn) {
      get_team_moves<true>(b, &total, moves_table, cap_counts, boards_table);
    } else {
      get_king_moves(b, &total, moves_table, cap_counts, boards_table);
      get_team_moves<false>(b, &total, moves_table, cap_counts, boards_table);
    }
  }

  negamax_ab_result combi[235];
  for (int i = 0; i < total; i++) {
    combi[i] = (negamax_ab_result){moves_table[i], boards_table[i]};
    // update score state
    combi[i].ss = update_score_state(ss, combi[i]._move, is_black_turn ? black_type : white_type);
    if (is_black_turn) {
      combi[i].ss.white_pawn_count -= cap_counts[i];
    } else {
      combi[i].ss.black_pawn_count -= cap_counts[i];
    }
    combi[i]._score = depth > 1 ? combi[i].ss.get_score(is_black_turn) : 0;
    // Add bonus if killer move
    if (combi[i]._move == KILLER_MOVES[ply][0] ||
        combi[i]._move == KILLER_MOVES[ply][1])
      combi[i]._score += 10000000;
    // Add bonus if we're following the 
    if (is_pv && (combi[i]._move == PREV_PV[ply])) {
      combi[i]._score += 100000000;
    }
  }

  // niave sort
  /*
  if (depth > 1) {
    std::sort(combi, combi + total, [](const auto &lhs, const auto &rhs) {
      return lhs._score > rhs._score;
    });
  }
  */

  // start with a bogus best
  int32_t best = MIN_SCORE;
  negamax_ab_result tmp;
  for (int i = 0; i < total; i++) {
    if (depth > 1) {
      int best_index = i;
      for (int j = i+1; j < total; j++) {
	if (combi[j]._score > combi[best_index]._score) {
          best_index = j;
	}
      }
      if (best_index != i) {
	tmp = combi[i];
	combi[i] = combi[best_index];
	combi[best_index] = tmp;
      }
    }
    /*
    */
    // if (depth == 1 && (m.orig == 115 && m.dest == 114)) printf("----------------------\n");
    // if (depth == 1) printf("----------------------\n");
    // if (depth == 1 && (m.orig == 115 && m.dest == 114)) {std::cout << combi[i]._move << "\n";};
    // if (depth == 1 && (m.orig == 115 && m.dest == 114)) print_board(combi[i]._board);
    // if (depth == 1) print_board(combi[i]._board);
    // if (depth == 1) printf("nw_guard_count: %d\n", combi[i].ss.nw_guard_count);
    // if (depth == 1) printf("ne_guard_count: %d\n", combi[i].ss.ne_guard_count);
    // if (depth == 1) printf("sw_guard_count: %d\n", combi[i].ss.sw_guard_count);
    // if (depth == 1) printf("se_guard_count: %d\n", combi[i].ss.se_guard_count);
    // if (depth == 1) printf("guard score: %d\n", combi[i].ss.guard_score);
    // if (depth == 1) printf("black pawn count: %d\n", combi[i].ss.black_pawn_count);
    // if (depth == 1) printf("white pawn count: %d\n", combi[i].ss.white_pawn_count);
    // if (depth == 1) printf("inc score: %d\n", combi[i].ss.get_score(is_black_turn));
    // if (depth == 1) printf("static score: %d\n", score_board(&combi[i]._board, is_black_turn));

    // TODO: move this to leaf node
    (*tally)++;

    // Late Move Reduction
    if (depth > 2 && i > 25) {
      int32_t lmr_eval =
        -negamax_ab_sorted_pv(combi[i]._move, combi[i]._board, !is_black_turn,
                              depth == 0 ? 0 : depth - 2, ply + 2, -alpha-1, -alpha, tally, (is_pv && !i), combi[i].ss, true);
      if (lmr_eval <= alpha) {
        continue;
      }
    }
    /*
    */

    // calcualte result and negate score
    // the is_pv parameter is (is_pv && !i) because if this _is_ the pv then the
    // first entry must be the next PV node due to the bonus applied above,
    // unless the next PV move wasn't found, which would be a real bug.

    int32_t eval = -negamax_ab_sorted_pv(
					 combi[i]._move, combi[i]._board, !is_black_turn,
					 depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
					 (is_pv && !i), combi[i].ss, true);

    /*
    int32_t eval;
    if (i == 0) {
      int32_t eval = -negamax_ab_sorted_pv(
          combi[i]._move, combi[i]._board, !is_black_turn,
          depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
          (is_pv && !i), combi[i].ss, true);
    } else {
      // search with null window
      int32_t eval = -negamax_ab_sorted_pv(
          combi[i]._move, combi[i]._board, !is_black_turn,
          depth == 0 ? 0 : depth - 1, ply + 1, -alpha-1, -alpha, tally,
          (is_pv && !i), combi[i].ss, true);
      if (alpha < eval && eval < beta) {
	// if failed high, research with full window
        int32_t eval = -negamax_ab_sorted_pv(
            combi[i]._move, combi[i]._board, !is_black_turn,
            depth == 0 ? 0 : depth - 1, ply + 1, -beta, -alpha, tally,
            (is_pv && !i), combi[i].ss, true);
      }
    }
    */

    // if (depth == 1 && (m.orig == 115 && m.dest == 114)) printf("score: %d\n",
    // next_result); if (depth == 1) printf("score: %d\n", next_result);

    if (eval > best) {
      best = eval;
    }
    if (best > alpha) {
      // ALPHA BETA HANDLING ---------------------------------------------------------------
      alpha = best;

      // PV HANDLING ---------------------------------------------------------------
      // assign move discovered here
      PV_TABLE[ply][ply] = combi[i]._move;
      PV_TABLE_BOARDS[ply][ply] = combi[i]._board; // me only
      // copy up moves discovered at lower depths
      for (int next_ply = ply + 1; next_ply < PV_LENGTH[ply + 1]; next_ply++) {
	PV_TABLE[ply][next_ply] = PV_TABLE[ply + 1][next_ply];
	PV_TABLE_BOARDS[ply][next_ply] = PV_TABLE_BOARDS[ply + 1][next_ply]; // me only
      }
      // adjust pv length
      PV_LENGTH[ply] = PV_LENGTH[ply + 1];

      // KILLER HANDLING ---------------------------------------------------------------
      // TODO: exclude captures
      // NOTE: this should typically be handled at a beta cutoff, but
      // benchmarking shows better performance if killers are set
      // every time alpha is raised
      KILLER_MOVES[ply][1] = KILLER_MOVES[ply][0];
      KILLER_MOVES[ply][0] = combi[i]._move;

    }
    if (alpha > beta) {
      break;
    }
  }
  // if (depth == 1 && (m.orig == 115 && m.dest == 114)) printf("----------------------");
  // if (depth == 1) printf("----------------------");

  return best;
}

enum Flag : uint8_t {
  lower_bound = 1,
  exact = 2,
  upper_bound = 3,
};

struct tt_entry {
  uint64_t hash;
  uint8_t depth;
  int32_t score;
  Flag flag;
  move best_move;
};

static const int tt_size_main = 1048576; // 2^20
static const int tt_size_offset = 7;
static const int tt_size = tt_size_main + tt_size_offset;
struct tt_entry tt[tt_size] = {};

int z_usage = 0;

int32_t negamax_ab_sorted_pv_runner(board b, bool is_black, int depth) {
  int tally = 0;
  score_state s = init_score_state(b);

  memset(KILLER_MOVES, 0, MAX_DEPTH * sizeof(move) * 2);
  memset(PREV_PV, 0, MAX_DEPTH * sizeof(move));
  for (int i = 0; i < depth; i++) {
    negamax_ab_sorted_pv((move){0, 0}, b, is_black, i, 0, INT_MIN,
			 INT_MAX, &tally, true, s, false);
    for (int j = 0; j < PV_LENGTH[0]; j++) {
      PREV_PV[j] = PV_TABLE[0][j];
    }
    PREV_PV_LENGTH = PV_LENGTH[0];
    
  }
  /*
  */
  auto res = negamax_ab_sorted_pv((move){0, 0}, b, is_black, depth, 0, MIN_SCORE,
				  MAX_SCORE, &tally, true, s, false);
  // printf("tally: %d\n", tally);
  return res;
}
