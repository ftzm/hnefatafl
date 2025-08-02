#include "move.h"
#include "move_legacy.h"
#include "assert.h"
#include "board.h"
#include "io.h"
#include "layer.h"
#include "string.h"
#include "ubench.h"
#include "x86intrin.h"

// Test board for benchmarking
const char *test_board_string = " .  .  X  .  X  .  .  O  .  .  . "
                                " .  X  .  X  .  .  .  .  .  .  . "
                                " X  .  .  O  O  X  .  .  X  .  . "
                                " .  .  .  .  .  #  X  .  .  X  . "
                                " .  X  .  .  .  .  O  .  .  .  X "
                                " X  .  .  O  O  .  O  .  .  X  X "
                                " X  .  .  .  O  O  O  .  .  .  X "
                                " X  .  .  .  .  O  .  .  .  .  X "
                                " .  .  .  .  .  .  .  .  O  .  . "
                                " .  .  .  .  .  X  .  .  .  .  . "
                                " .  .  X  .  X  X  X  X  .  .  . ";

// Benchmark moves_to function for black pieces
UBENCH_EX(comparison, moves_to_black) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move ms[335];
    layer ls[335];
    layer ls_r[335];
    int total = 0;
    moves_to(
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
    UBENCH_DO_NOTHING(&total);
  }
}

// Benchmark generator for black pieces
UBENCH_EX(comparison, generator_black) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move_generator gen;
    init_move_generator(
        &gen,
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board));
    
    move m;
    int total = 0;
    while (next_move(&gen, &m)) {
      total++;
      UBENCH_DO_NOTHING(&m);
    }
    UBENCH_DO_NOTHING(&total);
  }
}

// Benchmark moves_to function for white pieces
UBENCH_EX(comparison, moves_to_white) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move ms[335];
    layer ls[335];
    layer ls_r[335];
    int total = 0;
    moves_to(
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.white,
        test_board.white_r,
        board_occ(test_board),
        board_occ_r(test_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
    UBENCH_DO_NOTHING(&total);
  }
}

// Benchmark generator for white pieces
UBENCH_EX(comparison, generator_white) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move_generator gen;
    init_move_generator(
        &gen,
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.white,
        test_board.white_r,
        board_occ(test_board),
        board_occ_r(test_board));
    
    move m;
    int total = 0;
    while (next_move(&gen, &m)) {
      total++;
      UBENCH_DO_NOTHING(&m);
    }
    UBENCH_DO_NOTHING(&total);
  }
}

// Benchmark moves_to function for king pieces
UBENCH_EX(comparison, moves_to_king) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move ms[335];
    layer ls[335];
    layer ls_r[335];
    int total = 0;
    moves_to_king_impl(
        LAYER_NEG(king_board_occ(test_board)),
        LAYER_NEG(king_board_occ_r(test_board)),
        test_board.king,
        test_board.king_r,
        king_board_occ(test_board),
        king_board_occ_r(test_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
    UBENCH_DO_NOTHING(&total);
  }
}

// Benchmark generator for king pieces
UBENCH_EX(comparison, generator_king) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move_generator gen;
    init_move_generator(
        &gen,
        LAYER_NEG(king_board_occ(test_board)),
        LAYER_NEG(king_board_occ_r(test_board)),
        test_board.king,
        test_board.king_r,
        king_board_occ(test_board),
        king_board_occ_r(test_board));
    
    move m;
    int total = 0;
    while (next_move(&gen, &m)) {
      total++;
      UBENCH_DO_NOTHING(&m);
    }
    UBENCH_DO_NOTHING(&total);
  }
}

// Benchmark generating only the first few moves with generator
UBENCH_EX(early_termination, generator_first_5_black) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move_generator gen;
    init_move_generator(
        &gen,
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board));
    
    move m;
    int count = 0;
    while (next_move(&gen, &m) && count < 5) {
      count++;
      UBENCH_DO_NOTHING(&m);
    }
    UBENCH_DO_NOTHING(&count);
  }
}

// For comparison, generating first 5 moves with moves_to (has to generate all)
UBENCH_EX(early_termination, moves_to_first_5_black) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    move ms[335];
    layer ls[335];
    layer ls_r[335];
    int total = 0;
    moves_to(
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board),
        ms,
        ls,
        ls_r,
        &total);
    
    // Use only first 5 moves
    int count = total > 5 ? 5 : total;
    for (int i = 0; i < count; i++) {
      UBENCH_DO_NOTHING(&ms[i]);
    }
    UBENCH_DO_NOTHING(&count);
  }
}

// Memory usage test - measure allocations
UBENCH_EX(memory, moves_to_allocation) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    // This simulates the memory allocation pattern of moves_to
    move ms[335];           // 670 bytes
    layer ls[335];          // 5360 bytes  
    layer ls_r[335];        // 5360 bytes
    int total = 0;
    
    // Total: ~11.4KB of stack allocation
    moves_to(
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
    UBENCH_DO_NOTHING(&total);
  }
}

UBENCH_EX(memory, generator_allocation) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    // Generator only needs ~200 bytes for the struct
    move_generator gen;     // ~200 bytes
    init_move_generator(
        &gen,
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board));
    
    move m;                 // 2 bytes
    int total = 0;
    while (next_move(&gen, &m)) {
      total++;
      UBENCH_DO_NOTHING(&m);
    }
    UBENCH_DO_NOTHING(&total);
  }
}

// Test correctness by comparing outputs
UBENCH_EX(correctness, verify_same_moves) {
  const board test_board = read_board(test_board_string);
  UBENCH_DO_BENCHMARK() {
    // Generate moves with moves_to
    move ms_array[335];
    layer ls[335];
    layer ls_r[335];
    int total_array = 0;
    moves_to(
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board),
        ms_array,
        ls,
        ls_r,
        &total_array);
    
    // Generate moves with generator
    move_generator gen;
    init_move_generator(
        &gen,
        LAYER_NEG(board_occ(test_board)),
        LAYER_NEG(board_occ_r(test_board)),
        test_board.black,
        test_board.black_r,
        board_occ(test_board),
        board_occ_r(test_board));
    
    move ms_gen[335];
    int total_gen = 0;
    move m;
    while (next_move(&gen, &m) && total_gen < 335) {
      ms_gen[total_gen] = m;
      total_gen++;
    }
    
    // Counts should match
    assert(total_array == total_gen);
    
    UBENCH_DO_NOTHING(&total_array);
    UBENCH_DO_NOTHING(&total_gen);
  }
}

// needs to be at top level
UBENCH_STATE();

int main() {
  init_move_globals();
  return ubench_main(0, NULL);
}