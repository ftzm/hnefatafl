#include "move.h"
#include "assert.h"
#include "board.h"
#include "io.h"
#include "king_mobility.h"
#include "layer.h"
#include "move_legacy.h"
#include "move_legacy_mm.h"
#include "score.h"
#include "string.h"
#include "ubench.h"
#include "victory.h"
#include "x86intrin.h" // IWYU pragma: export

const char *sanity_capture_king_string = " .  .  X  .  X  .  .  O  .  .  . "
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

/*
UBENCH_EX(foo, gen) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

UBENCH_EX(foo, orig_black) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    get_team_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo, orig_white) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    get_team_moves_white(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

/*
UBENCH_EX(foo2, gen) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

/*
UBENCH_EX(foo2, gen2) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_black2(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

UBENCH_EX(foo3, gen3_black) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_black3(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo3, gen3_white) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_white3(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(move, mm_black) {
  const board b = read_board(sanity_capture_king_string);
  board bs[MAX_MOVES];
  move ms[MAX_MOVES];
  dir ds[MAX_MOVES];
  int total = 0;
  move_map mm;
  memset(mm, 0, sizeof(mm));
  move_map mm2;
  memset(mm2, 0, sizeof(mm));
  move_map mm3;
  memset(mm3, 0, sizeof(mm));
  build_mm(b.white, board_occ(b), mm);
  build_mm(b.black, board_occ(b), mm2);
  build_mm(b.king, board_occ(b), mm3);
  layer throne_mask = EMPTY_LAYER;
  OP_LAYER_BIT(throne_mask, 60, |=);
  layer free = pawn_destinations(b);
  free._[1] &= 144115188075855871;
  UBENCH_DO_BENCHMARK() {
    apply_southward_move(66, 11, mm2, mm2, mm3);
    UBENCH_DO_NOTHING(mm2);
    UBENCH_DO_NOTHING(mm3);
    gen_moves_from_mm_black(b, free, mm2, ms, ds, bs, &total);
    UBENCH_DO_NOTHING(ms);
    UBENCH_DO_NOTHING(ds);
  }
}

UBENCH_EX(move, mm_white) {
  const board b = read_board(sanity_capture_king_string);
  board bs[MAX_MOVES];
  move ms[MAX_MOVES];
  dir ds[MAX_MOVES];
  int total = 0;
  move_map mm;
  memset(mm, 0, sizeof(mm));
  move_map mm2;
  memset(mm2, 0, sizeof(mm));
  move_map mm3;
  memset(mm3, 0, sizeof(mm));
  build_mm(b.white, board_occ(b), mm);
  build_mm(b.black, board_occ(b), mm2);
  build_mm(b.king, board_occ(b), mm3);
  layer throne_mask = EMPTY_LAYER;
  OP_LAYER_BIT(throne_mask, 60, |=);
  layer free = pawn_destinations(b);
  free._[1] &= 144115188075855871;
  UBENCH_DO_BENCHMARK() {
    apply_southward_move(66, 11, mm2, mm2, mm3);
    UBENCH_DO_NOTHING(mm2);
    UBENCH_DO_NOTHING(mm3);
    gen_moves_from_mm_white(b, free, mm, ms, ds, bs, &total);
    UBENCH_DO_NOTHING(ms);
    UBENCH_DO_NOTHING(ds);
  }
}

UBENCH_EX(foo3, moves_to_black) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    move ms[MAX_MOVES];
    layer ls[MAX_MOVES];
    layer ls_r[MAX_MOVES];
    int total = 0;
    moves_to(
        LAYER_NEG(board_occ(start_board)),
        LAYER_NEG(board_occ_r(start_board)),
        start_board.black,
        start_board.black_r,
        board_occ(start_board),
        board_occ_r(start_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo3, moves_to_white) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    move ms[MAX_MOVES];
    layer ls[MAX_MOVES];
    layer ls_r[MAX_MOVES];
    int total = 0;
    moves_to(
        LAYER_NEG(board_occ(start_board)),
        LAYER_NEG(board_occ_r(start_board)),
        start_board.white,
        start_board.white_r,
        board_occ(start_board),
        board_occ_r(start_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo3, moves_to2_black) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    move ms[MAX_MOVES];
    layer ls[MAX_MOVES];
    layer ls_r[MAX_MOVES];
    int total = 0;
    moves_to2(
        LAYER_NEG(board_occ(start_board)),
        LAYER_NEG(board_occ_r(start_board)),
        start_board.black,
        start_board.black_r,
        board_occ(start_board),
        board_occ_r(start_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo3, moves_to2_white) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    move ms[MAX_MOVES];
    layer ls[MAX_MOVES];
    layer ls_r[MAX_MOVES];
    int total = 0;
    moves_to2(
        LAYER_NEG(board_occ(start_board)),
        LAYER_NEG(board_occ_r(start_board)),
        start_board.white,
        start_board.white_r,
        board_occ(start_board),
        board_occ_r(start_board),
        ms,
        ls,
        ls_r,
        &total);
    UBENCH_DO_NOTHING(ms);
  }
}

/*
UBENCH_EX(triple_nested, gen3_white) {
  int total_total = 0;
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_white3(start_board, &total, ms, bs);
    total_total += total;
    for (int i = 0; i < total; i++) {
      board bs2[MAX_MOVES];
      move ms2[MAX_MOVES];
      int total2 = 0;
      gen_reference_moves_white3(bs[i], &total2, ms2, bs2);
      total_total += total2;
      for (int j = 0; j < total; j++) {
        board bs3[MAX_MOVES];
        move ms3[MAX_MOVES];
        int total3 = 0;
        gen_reference_moves_white3(bs2[j], &total3, ms3, bs3);
        total_total += total3;
        UBENCH_DO_NOTHING(ms3);
        UBENCH_DO_NOTHING(bs3);
      }
    }
  }
  printf("%d\n", total_total);
}

UBENCH_EX(triple_nested, mm_white) {
  int total_total = 0;
  const board b = read_board(sanity_capture_king_string);
  board bs[MAX_MOVES];
  move ms[MAX_MOVES];
  dir ds[MAX_MOVES];
  int total = 0;
  move_map mm_white;
  memset(mm_white, 0, sizeof(mm_white));
  move_map mm_black;
  memset(mm_black, 0, sizeof(mm_black));
  move_map mm_king;
  memset(mm_king, 0, sizeof(mm_king));
  UBENCH_DO_BENCHMARK() {
    build_mm(b.white, board_occ(b), mm_white);
    build_mm(b.black, board_occ(b), mm_black);
    build_mm(b.king, board_occ(b), mm_king);

    layer throne_mask = EMPTY_LAYER;
    OP_LAYER_BIT(throne_mask, 60, |=);
    layer free = pawn_destinations(b);
    free._[1] &= 144115188075855871;

    gen_moves_from_mm_white(b, free, mm_white, ms, ds, bs, &total);
    total_total += total;
    for (int i = 0; i < total; i++) {
      move_map mm_white2;
      memcpy(mm_white2, mm_white, sizeof(mm_white));
      move_map mm_black2;
      memcpy(mm_black2, mm_black, sizeof(mm_black));
      move_map mm_king2;
      memcpy(mm_king2, mm_king, sizeof(mm_king));

      move m = ms[i];
      enum dir d = ds[i];
      if (d == north) {
        apply_northward_move(m.orig, m.dest, mm_white2, mm_black2, mm_king2);
      } else if (d == south) {
        apply_southward_move(m.orig, m.dest, mm_white2, mm_black2, mm_king2);
      } else if (d == east) {
        apply_eastward_move(m.orig, m.dest, mm_white2, mm_black2, mm_king2);
      } else if (d == west) {
        apply_westward_move(m.orig, m.dest, mm_white2, mm_black2, mm_king2);
      }

      board bs2[MAX_MOVES];
      move ms2[MAX_MOVES];
      dir ds2[MAX_MOVES];
      int total2 = 0;

      layer throne_mask = EMPTY_LAYER;
      OP_LAYER_BIT(throne_mask, 60, |=);
      layer free = pawn_destinations(bs[i]);
      free._[1] &= 144115188075855871;

      gen_moves_from_mm_white(bs[i], free, mm_white2, ms2, ds2, bs2, &total2);
      total_total += total2;

      for (int j = 0; j < total; j++) {
        move_map mm_white3;
        memcpy(mm_white3, mm_white2, sizeof(mm_white));
        move_map mm_black3;
        memcpy(mm_black3, mm_black2, sizeof(mm_black));
        move_map mm_king3;
        memcpy(mm_king3, mm_king2, sizeof(mm_king));

        move m = ms2[j];
        enum dir d = ds2[j];
        if (d == north) {
          apply_northward_move(m.orig, m.dest, mm_white3, mm_black3, mm_king3);
        } else if (d == south) {
          apply_southward_move(m.orig, m.dest, mm_white3, mm_black3, mm_king3);
        } else if (d == east) {
          apply_eastward_move(m.orig, m.dest, mm_white3, mm_black3, mm_king3);
        } else if (d == west) {
          apply_westward_move(m.orig, m.dest, mm_white3, mm_black3, mm_king3);
        }

        board bs3[MAX_MOVES];
        move ms3[MAX_MOVES];
        dir ds3[MAX_MOVES];
        int total3 = 0;

        layer throne_mask = EMPTY_LAYER;
        OP_LAYER_BIT(throne_mask, 60, |=);
        layer free = pawn_destinations(bs2[j]);
        free._[1] &= 144115188075855871;

        gen_moves_from_mm_white(
            bs2[j], free, mm_white3, ms3, ds3, bs3, &total3);
        total_total += total3;
        UBENCH_DO_NOTHING(ms3);
        UBENCH_DO_NOTHING(bs3);
      }
    }
  }
  printf("%d\n", total_total);
}

UBENCH_EX(triple_nested, gen3_black) {
  int total_total = 0;
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[MAX_MOVES];
    move ms[MAX_MOVES];
    int total = 0;
    gen_reference_moves_black3(start_board, &total, ms, bs);
    total_total += total;
    for (int i = 0; i < total; i++) {
      board bs2[MAX_MOVES];
      move ms2[MAX_MOVES];
      int total2 = 0;
      gen_reference_moves_black3(bs[i], &total2, ms2, bs2);
      total_total += total2;
      for (int j = 0; j < total; j++) {
        board bs3[MAX_MOVES];
        move ms3[MAX_MOVES];
        int total3 = 0;
        gen_reference_moves_black3(bs2[j], &total3, ms3, bs3);
        total_total += total3;
        UBENCH_DO_NOTHING(ms3);
        UBENCH_DO_NOTHING(bs3);
      }
    }
  }
  printf("%d\n", total_total);
}

UBENCH_EX(triple_nested, mm_black) {
  int total_total = 0;
  const board b = read_board(sanity_capture_king_string);
  board bs[MAX_MOVES];
  move ms[MAX_MOVES];
  dir ds[MAX_MOVES];
  int total = 0;
  move_map mm_white;
  memset(mm_white, 0, sizeof(mm_white));
  move_map mm_black;
  memset(mm_black, 0, sizeof(mm_black));
  move_map mm_king;
  memset(mm_king, 0, sizeof(mm_king));
  UBENCH_DO_BENCHMARK() {
    build_mm(b.white, board_occ(b), mm_white);
    build_mm(b.black, board_occ(b), mm_black);
    build_mm(b.king, board_occ(b), mm_king);

    layer throne_mask = EMPTY_LAYER;
    OP_LAYER_BIT(throne_mask, 60, |=);
    layer free = pawn_destinations(b);
    free._[1] &= 144115188075855871;

    gen_moves_from_mm_black(b, free, mm_black, ms, ds, bs, &total);
    total_total += total;
    for (int i = 0; i < total; i++) {
      move_map mm_white2;
      memcpy(mm_white2, mm_white, sizeof(mm_white));
      move_map mm_black2;
      memcpy(mm_black2, mm_black, sizeof(mm_black));
      move_map mm_king2;
      memcpy(mm_king2, mm_king, sizeof(mm_king));

      move m = ms[i];
      enum dir d = ds[i];
      if (d == north) {
        apply_northward_move(m.orig, m.dest, mm_black2, mm_white2, mm_king2);
      } else if (d == south) {
        apply_southward_move(m.orig, m.dest, mm_black2, mm_white2, mm_king2);
      } else if (d == east) {
        apply_eastward_move(m.orig, m.dest, mm_black2, mm_white2, mm_king2);
      } else if (d == west) {
        apply_westward_move(m.orig, m.dest, mm_black2, mm_white2, mm_king2);
      }

      board bs2[MAX_MOVES];
      move ms2[MAX_MOVES];
      dir ds2[MAX_MOVES];
      int total2 = 0;

      layer throne_mask = EMPTY_LAYER;
      OP_LAYER_BIT(throne_mask, 60, |=);
      layer free = pawn_destinations(bs[i]);
      free._[1] &= 144115188075855871;

      gen_moves_from_mm_black(bs[i], free, mm_black2, ms2, ds2, bs2, &total2);
      total_total += total2;

      for (int j = 0; j < total; j++) {
        move_map mm_white3;
        memcpy(mm_white3, mm_white2, sizeof(mm_white));
        move_map mm_black3;
        memcpy(mm_black3, mm_black2, sizeof(mm_black));
        move_map mm_king3;
        memcpy(mm_king3, mm_king2, sizeof(mm_king));

        move m = ms2[j];
        enum dir d = ds2[j];
        if (d == north) {
          apply_northward_move(m.orig, m.dest, mm_black3, mm_white3, mm_king3);
        } else if (d == south) {
          apply_southward_move(m.orig, m.dest, mm_black3, mm_white3, mm_king3);
        } else if (d == east) {
          apply_eastward_move(m.orig, m.dest, mm_black3, mm_white3, mm_king3);
        } else if (d == west) {
          apply_westward_move(m.orig, m.dest, mm_black3, mm_white3, mm_king3);
        }

        board bs3[MAX_MOVES];
        move ms3[MAX_MOVES];
        dir ds3[MAX_MOVES];
        int total3 = 0;

        layer throne_mask = EMPTY_LAYER;
        OP_LAYER_BIT(throne_mask, 60, |=);
        layer free = pawn_destinations(bs2[j]);
        free._[1] &= 144115188075855871;

        gen_moves_from_mm_black(
            bs2[j], free, mm_black3, ms3, ds3, bs3, &total3);
        total_total += total3;
        UBENCH_DO_NOTHING(ms3);
        UBENCH_DO_NOTHING(bs3);
      }
    }
  }
  printf("%d\n", total_total);
}
  */

/*
UBENCH_EX(move_count, white_orig) {
  const board b = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    u16 c =
        get_team_move_count(board_occ(b), b.white, board_occ_r(b), b.white_r);
    UBENCH_DO_NOTHING(&c);
  }
}
  */

UBENCH_EX(move_count, white_new) {
  board b = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    int c = white_moves_count(&b);
    UBENCH_DO_NOTHING(&c);
  }
}

UBENCH_EX(move_count, king_moves_count) {
  board b = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    int c = king_moves_count(&b);
    UBENCH_DO_NOTHING(&c);
  }
}

UBENCH_EX(move_count, king_moves_count2) {
  board b = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    int c = king_moves_count2(&b);
    UBENCH_DO_NOTHING(&c);
  }
}

UBENCH_EX(move_count, get_king_move_count) {
  board b = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    int c = get_king_move_count(b);
    UBENCH_DO_NOTHING(&c);
  }
}

const char *corner_access_double = " .  .  X  .  X  .  .  O  .  .  . "
                                   " .  X  .  X  .  .  .  .  .  .  . "
                                   " X  .  .  O  O  X  .  .  X  .  . "
                                   " .  .  .  .  .  .  X  .  .  X  . "
                                   " .  X  .  .  .  .  O  .  .  .  X "
                                   " X  .  .  O  O  .  O  .  .  X  X "
                                   " X  .  .  .  O  O  O  .  .  .  X "
                                   " X  .  .  .  .  O  .  .  .  .  X "
                                   " .  .  .  .  .  .  .  .  O  .  . "
                                   " .  #  .  .  .  X  .  .  .  .  . "
                                   " .  .  X  .  X  X  X  X  .  .  . ";

UBENCH_EX(king_mobility, corner_paths_1) {
  board b = read_board(corner_access_double);

  // setup to generate layers
  layer occ = board_occ(b);
  layer occ_r = board_occ_r(b);

  int king_pos = LOWEST_INDEX(b.king);
  int king_rank = RANK(king_pos);
  int king_file = FILE(king_pos);

  UBENCH_DO_BENCHMARK() {

    layer paths = EMPTY_LAYER;
    layer paths_r = EMPTY_LAYER;

    corner_paths_1(occ, occ_r, king_rank, king_file, &paths, &paths_r);
    UBENCH_DO_NOTHING(&paths);
    UBENCH_DO_NOTHING(&paths_r);
  }
}

static void set_occ(layer *occ, layer *occ_r, int rank, int file) {
  int idx = rank * 11 + file;
  SET_INDEX_PTR(occ, idx);
  SET_INDEX_PTR(occ_r, rotate_right[idx]);
}

// corner_paths_2 benchmarks: 13 cases x 2 (empty / blocked)
// Cases correspond to the branches in corner_paths_2:
//   Edge:     file==0, file==10, rank==0, rank==10
//   Interior: file x rank where file in {1, 9, default} and rank in {1, 9,
//   default}

#define CP2_BENCH_EMPTY(_name, _rank, _file)                                   \
  UBENCH_EX(cp2, _name##_empty) {                                              \
    layer occ = EMPTY_LAYER;                                                   \
    layer occ_r = EMPTY_LAYER;                                                 \
    UBENCH_DO_BENCHMARK() {                                                    \
      layer paths = EMPTY_LAYER;                                               \
      layer paths_r = EMPTY_LAYER;                                             \
      corner_paths_2(occ, occ_r, _rank, _file, &paths, &paths_r);              \
      UBENCH_DO_NOTHING(&paths);                                               \
      UBENCH_DO_NOTHING(&paths_r);                                             \
    }                                                                          \
  }

#define CP2_BENCH_BLOCKED(_name, _rank, _file)                                 \
  UBENCH_EX(cp2, _name##_blocked) {                                            \
    layer occ = EMPTY_LAYER;                                                   \
    layer occ_r = EMPTY_LAYER;                                                 \
    if (_rank > 0)                                                             \
      set_occ(&occ, &occ_r, _rank - 1, _file);                                 \
    if (_rank < 10)                                                            \
      set_occ(&occ, &occ_r, _rank + 1, _file);                                 \
    if (_file > 0)                                                             \
      set_occ(&occ, &occ_r, _rank, _file - 1);                                 \
    if (_file < 10)                                                            \
      set_occ(&occ, &occ_r, _rank, _file + 1);                                 \
    UBENCH_DO_BENCHMARK() {                                                    \
      layer paths = EMPTY_LAYER;                                               \
      layer paths_r = EMPTY_LAYER;                                             \
      corner_paths_2(occ, occ_r, _rank, _file, &paths, &paths_r);              \
      UBENCH_DO_NOTHING(&paths);                                               \
      UBENCH_DO_NOTHING(&paths_r);                                             \
    }                                                                          \
  }

#define CP2_BENCH(_name, _rank, _file)                                         \
  CP2_BENCH_EMPTY(_name, _rank, _file)                                         \
  CP2_BENCH_BLOCKED(_name, _rank, _file)

// Edge cases
CP2_BENCH(edge_f0, 5, 0)
CP2_BENCH(edge_f10, 5, 10)
CP2_BENCH(edge_r0, 0, 5)
CP2_BENCH(edge_r10, 10, 5)

// Interior: file==1
CP2_BENCH(f1_r1, 1, 1)
CP2_BENCH(f1_r9, 9, 1)
CP2_BENCH(f1_mid, 5, 1)

// Interior: file==9
CP2_BENCH(f9_r1, 1, 9)
CP2_BENCH(f9_r9, 9, 9)
CP2_BENCH(f9_mid, 5, 9)

// Interior: file==default (5)
CP2_BENCH(mid_r1, 1, 5)
CP2_BENCH(mid_r9, 9, 5)
CP2_BENCH(mid_mid, 5, 5)

#define CP2_NEW_BENCH_EMPTY(_name, _rank, _file)                               \
  UBENCH_EX(cp2_new, _name##_empty) {                                          \
    layer occ = EMPTY_LAYER;                                                   \
    layer occ_r = EMPTY_LAYER;                                                 \
    int king_pos = _rank * 11 + _file;                                         \
    UBENCH_DO_BENCHMARK() {                                                    \
      layer paths = EMPTY_LAYER;                                               \
      layer paths_r = EMPTY_LAYER;                                             \
      corner_paths_2_new(                                                      \
          occ,                                                                 \
          occ_r,                                                               \
          _rank,                                                               \
          _file,                                                               \
          king_pos,                                                            \
          &paths,                                                              \
          &paths_r);                                                           \
      UBENCH_DO_NOTHING(&paths);                                               \
      UBENCH_DO_NOTHING(&paths_r);                                             \
    }                                                                          \
  }

#define CP2_NEW_BENCH_BLOCKED(_name, _rank, _file)                             \
  UBENCH_EX(cp2_new, _name##_blocked) {                                        \
    layer occ = EMPTY_LAYER;                                                   \
    layer occ_r = EMPTY_LAYER;                                                 \
    int king_pos = _rank * 11 + _file;                                         \
    if (_rank > 0)                                                             \
      set_occ(&occ, &occ_r, _rank - 1, _file);                                 \
    if (_rank < 10)                                                            \
      set_occ(&occ, &occ_r, _rank + 1, _file);                                 \
    if (_file > 0)                                                             \
      set_occ(&occ, &occ_r, _rank, _file - 1);                                 \
    if (_file < 10)                                                            \
      set_occ(&occ, &occ_r, _rank, _file + 1);                                 \
    UBENCH_DO_BENCHMARK() {                                                    \
      layer paths = EMPTY_LAYER;                                               \
      layer paths_r = EMPTY_LAYER;                                             \
      corner_paths_2_new(                                                      \
          occ,                                                                 \
          occ_r,                                                               \
          _rank,                                                               \
          _file,                                                               \
          king_pos,                                                            \
          &paths,                                                              \
          &paths_r);                                                           \
      UBENCH_DO_NOTHING(&paths);                                               \
      UBENCH_DO_NOTHING(&paths_r);                                             \
    }                                                                          \
  }

#define CP2_NEW_BENCH(_name, _rank, _file)                                     \
  CP2_NEW_BENCH_EMPTY(_name, _rank, _file)                                     \
  CP2_NEW_BENCH_BLOCKED(_name, _rank, _file)

// Edge cases
CP2_NEW_BENCH(edge_f0, 5, 0)
CP2_NEW_BENCH(edge_f10, 5, 10)
CP2_NEW_BENCH(edge_r0, 0, 5)
CP2_NEW_BENCH(edge_r10, 10, 5)

// Interior: file==1
CP2_NEW_BENCH(f1_r1, 1, 1)
CP2_NEW_BENCH(f1_r9, 9, 1)
CP2_NEW_BENCH(f1_mid, 5, 1)

// Interior: file==9
CP2_NEW_BENCH(f9_r1, 1, 9)
CP2_NEW_BENCH(f9_r9, 9, 9)
CP2_NEW_BENCH(f9_mid, 5, 9)

// Interior: file==default (5)
CP2_NEW_BENCH(mid_r1, 1, 5)
CP2_NEW_BENCH(mid_r9, 9, 5)
CP2_NEW_BENCH(mid_mid, 5, 5)
UBENCH_EX(extract, from_layers_black) {
  const board b = read_board(sanity_capture_king_string);
  move_layers layers = generate_black_move_layers(&b);
  UBENCH_DO_BENCHMARK() {
    move ms[MAX_MOVES];
    layer ls[MAX_MOVES] = {0};
    layer ls_r[MAX_MOVES] = {0};
    int total = 0;
    moves_from_layers(&layers, b.black, b.black_r, ms, ls, ls_r, &total);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(extract, from_layers_white) {
  const board b = read_board(sanity_capture_king_string);
  move_layers layers = generate_white_move_layers(&b);
  UBENCH_DO_BENCHMARK() {
    move ms[MAX_MOVES];
    layer ls[MAX_MOVES] = {0};
    layer ls_r[MAX_MOVES] = {0};
    int total = 0;
    moves_from_layers(&layers, b.white, b.white_r, ms, ls, ls_r, &total);
    UBENCH_DO_NOTHING(ms);
  }
}

int bench_king_capture_check(bool (*check)(const board *b)) {
  // this total is just to ensure that the code is not optimized away.
  int total = 0;
  for (int i = 0; i < 121; i++) {
    for (u8 attackers = 0; attackers < 16; attackers++) {
      board b = {.king = EMPTY_LAYER, .black = EMPTY_LAYER};
      SET_INDEX(b.king, i);
      // north
      if (attackers & 1 && RANK(i) != 10) {
        int index = i + 11;
        SET_INDEX(b.black, index);
      }
      // south
      if (attackers & 0b10 && RANK(i) != 0) {
        int index = i - 11;
        SET_INDEX(b.black, index);
      }
      // east
      if (attackers & 0b100 && FILE(i) != 0) {
        int index = i - 1;
        SET_INDEX(b.black, index);
      }
      // west
      if (attackers & 0b1000 && FILE(i) != 10) {
        int index = i + 1;
        SET_INDEX(b.black, index);
      }
      total += check(&b);
    }
  }
  return total;
}

UBENCH_EX(king_capture, bit_checks) {
  UBENCH_DO_BENCHMARK() {
    int res = bench_king_capture_check(king_capture_check_ref);
    UBENCH_DO_NOTHING(&res);
  }
}

UBENCH_EX(king_capture, surround_mask) {
  UBENCH_DO_BENCHMARK() {
    int res = bench_king_capture_check(king_captured);
    UBENCH_DO_NOTHING(&res);
  }
}

UBENCH_EX(king_capture, surround_mask2) {
  UBENCH_DO_BENCHMARK() {
    int res = bench_king_capture_check(king_capture_check);
    UBENCH_DO_NOTHING(&res);
  }
}

// needs to be at top level
UBENCH_STATE();

int main() {
  init_move_globals();
  return ubench_main(0, NULL);
}
