#include "layer.h"
#include "board.h"
#include "constants.h"
#include "stdbool.h"
#include "stdint.h"
#include "limits.h"
#include "move.h"
#include "capture.h"
#include "x86intrin.h"

typedef enum piece_type : uint8_t {
  black_type = 1,
  white_type = 2,
  king_type = 3,
} piece_type;

typedef struct score_state {
  uint8_t nw_guard_count;
  uint8_t ne_guard_count;
  uint8_t sw_guard_count;
  uint8_t se_guard_count;
  int32_t guard_score;
  uint8_t black_pawn_count;
  uint8_t white_pawn_count;
  int32_t pst_black_score;
  int32_t pst_white_score;
  int32_t pst_king_score;
} score_state;

int32_t BLACK_PAWN_VALUE = 10000;
int32_t WHITE_PAWN_VALUE = 10000;

int32_t get_score(score_state *state, bool is_black_turn) {
  int32_t score_as_black =
      state->guard_score + (BLACK_PAWN_VALUE * state->black_pawn_count) -
      (WHITE_PAWN_VALUE * state->white_pawn_count) - state->pst_white_score;
  return is_black_turn ? score_as_black : -score_as_black;
};

uint8_t nw_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_nw._[1] & b.black._[1]);
}

uint8_t ne_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_ne._[1] & b.black._[1]);
}

uint8_t sw_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_sw._[0] & b.black._[0]);
}

uint8_t se_corner_protection(const board b) {
  return __builtin_popcountll(corner_guard_se._[0] & b.black._[0]);
}

typedef struct piece_square_table {
  u32 _[121];
} piece_square_table;

piece_square_table quarter_to_pst(u32 quarter[29]) {
  static int indices[29] = {1,  2,  3,  4,  5,  11, 12, 13, 14, 15,
                            16, 22, 23, 24, 25, 26, 27, 33, 34, 35,
                            36, 37, 38, 44, 45, 46, 47, 48, 49};

  piece_square_table pst = {0};

  for (int i = 0; i < 29; i++) {
    int32_t val = quarter[i];
    int index = indices[i];
    pst._[index] = val;
    index = rotate_right[index];
    pst._[index] = val;
    index = rotate_right[index];
    pst._[index] = val;
    index = rotate_right[index];
    pst._[index] = val;
  }
  return pst;
}

u32 black_niave_pst_quarter[29] = {
    0, // 1
    0, // 2
    0, // 3
    0, // 4
    0, // 5
    0, // 11
    0, // 12
    0, // 13
    0, // 14
    0, // 15
    0, // 16
    0, // 22
    0, // 23
    0, // 24
    0, // 25
    0, // 26
    0, // 27
    0, // 33
    0, // 34
    0, // 35
    0, // 36
    0, // 37
    0, // 38
    0, // 44
    0, // 45
    0, // 46
    0, // 47
    0, // 48
    0  // 49
};

u32 white_niave_pst_quarter[29] = {
    0, // 1
    0, // 2
    0, // 3
    0, // 4
    0, // 5
    0, // 11
    0, // 12
    0, // 13
    0, // 14
    0, // 15
    0, // 16
    0, // 22
    0, // 23
    0, // 24
    0, // 25
    0, // 26
    0, // 27
    0, // 33
    0, // 34
    0, // 35
    0, // 36
    0, // 37
    0, // 38
    0, // 44
    0, // 45
    0, // 46
    0, // 47
    0, // 48
    0  // 49
};

u32 king_niave_pst_quarter[29] = {
    0,   // 1
    500, // 2
    0,   // 3
    0,   // 4
    0,   // 5
    0,   // 11
    500, // 12
    500, // 13
    0,   // 14
    0,   // 15
    0,   // 16
    500, // 22
    500, // 23
    500, // 24
    0,   // 25
    0,   // 26
    0,   // 27
    0,   // 33
    0,   // 34
    0,   // 35
    0,   // 36
    0,   // 37
    -5,  // 38
    0,   // 44
    0,   // 45
    0,   // 46
    0,   // 47
    -10, // 48
    -15  // 49
};


typedef struct ai_settings {
  piece_square_table black_pst;
  piece_square_table white_pst;
  piece_square_table king_pst;
  int32_t king_throne_position_score;
} ai_settings;

ai_settings init_ai_settings() {
  return (ai_settings){
      quarter_to_pst(black_niave_pst_quarter),
      quarter_to_pst(white_niave_pst_quarter),
      quarter_to_pst(king_niave_pst_quarter),
      -100};
}

score_state init_score_state(const board b) {
  return (score_state){
      nw_corner_protection(b),
      ne_corner_protection(b),
      sw_corner_protection(b),
      se_corner_protection(b),
      0,
      black_pawn_count(b),
      white_pawn_count(b),
      0,
      0,
      0};
}

int32_t guard_count_bonuses[] = {0, 300, 600, 1000};

void update_guard_score_state_move(score_state *s, move m) {

  switch (m.orig) {
  // nw
  case 118:
  case 108:
  case 98:
    s->guard_score -= guard_count_bonuses[s->nw_guard_count];
    s->nw_guard_count--;
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s->guard_score -= guard_count_bonuses[s->ne_guard_count];
    s->ne_guard_count--;
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s->guard_score -= guard_count_bonuses[s->sw_guard_count];
    s->sw_guard_count--;
    break;
  // se
  case 22:
  case 12:
  case 2:
    s->guard_score -= guard_count_bonuses[s->se_guard_count];
    s->se_guard_count -= 1;
    break;
  }
  switch (m.dest) {
  // nw
  case 118:
  case 108:
  case 98:
    s->nw_guard_count++;
    /*
    if (s.nw_guard_count > 3) {
      printf("guard count nw: %d\n", s.nw_guard_count);
      std::cout << "that's illegal: " << m << "\n";
    }
    */
    s->guard_score += guard_count_bonuses[s->nw_guard_count];
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s->ne_guard_count++;
    if (s->ne_guard_count > 3) {
      printf("guard coun ne: %d", s->ne_guard_count);
      // std::cout << "that's illegal: " << m << "\n";
    }
    s->guard_score += guard_count_bonuses[s->ne_guard_count];
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s->sw_guard_count++;
    // if (s.sw_guard_count > 3) {
    //   printf("guard count sw: %d", s.sw_guard_count);
    //   std::cout << "that's illegal: " << m << "\n";
    // }
    s->guard_score += guard_count_bonuses[s->sw_guard_count];
    break;
  // se
  case 22:
  case 12:
  case 2:
    s->se_guard_count++;
    // if (s.se_guard_count > 3) {
    //   printf("guard count se: %d", s.se_guard_count);
    //   std::cout << "that's illegal: " << m << "\n";
    // }
    s->guard_score += guard_count_bonuses[s->se_guard_count];
    break;
  }
};

void update_guard_score_state_capture(score_state *s, int i) {
  //  printf("fire cap up\n");

  switch (i) {
  // nw
  case 118:
  case 108:
  case 98:
    s->guard_score -= guard_count_bonuses[s->nw_guard_count];
    s->nw_guard_count--;
    break;
  // ne
  case 112:
  case 100:
  case 88:
    s->guard_score -= guard_count_bonuses[s->ne_guard_count];
    s->ne_guard_count--;
    break;
  // sw
  case 32:
  case 20:
  case 8:
    s->guard_score -= guard_count_bonuses[s->sw_guard_count];
    s->sw_guard_count--;
    break;
  // se
  case 22:
  case 12:
  case 2:
    s->guard_score -= guard_count_bonuses[s->se_guard_count];
    s->se_guard_count -= 1;
    break;
  }
};

score_state update_score_state(score_state old_s, move m, piece_type t) {
  score_state s = old_s;
  if (t == black_type) {
    update_guard_score_state_move(&s, m);
  }
  return s;
}

bool king_escaped(const board b) {
  return b.king._[0] & corners._[0] || b.king._[1] & corners._[1];
}

bool king_captured(const board b) {
  uint8_t king_index =
      b.king._[0] ? _tzcnt_u64(b.king._[0]) : _tzcnt_u64(b.king._[1]) + 64;
  layer attackers = LAYER_OR(surround_masks[king_index], b.black);
  uint8_t attacker_count =
      __builtin_popcountll(attackers._[0]) + __builtin_popcountll(attackers._[1]);
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


bool king_escape_ensured(const board b) {
  return b.king._[0] & ADJACENTS._[0] || b.king._[1] & ADJACENTS._[1];
}

int32_t score_board(const board board, const bool is_black_turn) {
  // TODO: div and mod are expensive; store king pos in board or the other way around
  uint king_pos = board.king._[0] ? _tzcnt_u64(board.king._[0])
                                : _tzcnt_u64(board.king._[1]) + 64;
  uint king_rank = king_pos / 11;
  uint king_file = king_pos % 11;

  int32_t white_score = //(white_pawn_count(*board) * 10000) +
      white_moves_count(&board) + (king_moves_count(&board) * 100) +
      (king_escape_ensured(board) * 9000000);

  int32_t black_score =             //(black_pawn_count(*board) * 10000) +
      black_moves_count(&board); // +
                                    // corner_protection(*board);

  return is_black_turn ? black_score - white_score : white_score - black_score;
}

int32_t score_board_for_order(const board *board, const bool is_black_turn) {
  int32_t white_score = white_pawn_count(*board) * 1000;

  int32_t black_score =
      (black_pawn_count(*board) * 1000) + corner_protection(*board) * 100;

  return is_black_turn ? black_score - white_score : white_score - black_score;
}
