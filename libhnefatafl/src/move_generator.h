#pragma once

#include "board.h"
#include "layer.h"
#include "move.h"
#include "stdbool.h"

// Generator state constants
enum generator_state {
  GEN_INIT = 0,
  GEN_LEFTWARD_0 = 1,
  GEN_LEFTWARD_0_EXTRACT = 2,
  GEN_LEFTWARD_1 = 3,
  GEN_LEFTWARD_1_EXTRACT = 4,
  GEN_LEFTWARD_CENTER = 5,
  GEN_LEFTWARD_CENTER_EXTRACT = 6,
  GEN_LEFTWARD_0_R = 7,
  GEN_LEFTWARD_0_R_EXTRACT = 8,
  GEN_LEFTWARD_1_R = 9,
  GEN_LEFTWARD_1_R_EXTRACT = 10,
  GEN_LEFTWARD_CENTER_R = 11,
  GEN_LEFTWARD_CENTER_R_EXTRACT = 12,
  GEN_RIGHTWARD_0 = 13,
  GEN_RIGHTWARD_0_EXTRACT = 14,
  GEN_RIGHTWARD_1 = 15,
  GEN_RIGHTWARD_1_EXTRACT = 16,
  GEN_RIGHTWARD_CENTER = 17,
  GEN_RIGHTWARD_CENTER_EXTRACT = 18,
  GEN_RIGHTWARD_0_R = 19,
  GEN_RIGHTWARD_0_R_EXTRACT = 20,
  GEN_RIGHTWARD_1_R = 21,
  GEN_RIGHTWARD_1_R_EXTRACT = 22,
  GEN_RIGHTWARD_CENTER_R = 23,
  GEN_RIGHTWARD_CENTER_R_EXTRACT = 24,
  GEN_DONE = 25
};

// King generator state constants
enum king_generator_state {
  GEN_KING_INIT = 0,
  GEN_KING_LEFTWARD_0 = 1,
  GEN_KING_LEFTWARD_0_EXTRACT = 2,
  GEN_KING_LEFTWARD_1 = 3,
  GEN_KING_LEFTWARD_1_EXTRACT = 4,
  GEN_KING_LEFTWARD_CENTER = 5,
  GEN_KING_LEFTWARD_CENTER_EXTRACT = 6,
  GEN_KING_LEFTWARD_0_R = 7,
  GEN_KING_LEFTWARD_0_R_EXTRACT = 8,
  GEN_KING_LEFTWARD_1_R = 9,
  GEN_KING_LEFTWARD_1_R_EXTRACT = 10,
  GEN_KING_LEFTWARD_CENTER_R = 11,
  GEN_KING_LEFTWARD_CENTER_R_EXTRACT = 12,
  GEN_KING_RIGHTWARD_0 = 13,
  GEN_KING_RIGHTWARD_0_EXTRACT = 14,
  GEN_KING_RIGHTWARD_1 = 15,
  GEN_KING_RIGHTWARD_1_EXTRACT = 16,
  GEN_KING_RIGHTWARD_CENTER = 17,
  GEN_KING_RIGHTWARD_CENTER_EXTRACT = 18,
  GEN_KING_RIGHTWARD_0_R = 19,
  GEN_KING_RIGHTWARD_0_R_EXTRACT = 20,
  GEN_KING_RIGHTWARD_1_R = 21,
  GEN_KING_RIGHTWARD_1_R_EXTRACT = 22,
  GEN_KING_RIGHTWARD_CENTER_R = 23,
  GEN_KING_RIGHTWARD_CENTER_R_EXTRACT = 24,
  GEN_KING_DONE = 25
};

// Move generator structure definition
typedef struct move_generator {
  // Input parameters
  layer targets;
  layer targets_r;
  layer movers;
  layer movers_r;
  layer occ;
  layer occ_r;
  
  // Derived values calculated once during initialization
  u16 center_occ;
  u16 center_occ_r;
  u16 center_movers;
  u16 center_movers_r;
  layer leftward_occ;
  layer leftward_occ_r;
  layer rightward_occ;
  layer rightward_occ_r;
  
  // Generator state
  int state;
  u64 dests;
  u64 origs;
  u64 current_orig_bit;
  u8 current_orig;
  
  // For rightward moves
  u64 move_mask;
} move_generator;

// Initialize a move generator
void init_move_generator(
    move_generator *gen,
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r);

// Initialize the king move generator
void init_move_generator_king(
    move_generator *gen,
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r);

// Get the next move from the generator
// Returns true if a move was generated, false if no more moves
bool next_move(move_generator *gen, move *result);

// Get the next move from the king generator
// Returns true if a move was generated, false if no more moves
bool next_move_king(move_generator *gen, move *result);