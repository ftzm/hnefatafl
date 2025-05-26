#include "move.h"
#include "move_legacy.h"
#include "move_legacy_mm.h"
#include "assert.h"
#include "board.h"
#include "io.h"
#include "layer.h"
#include "string.h"
#include "ubench.h"
#include "x86intrin.h"

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
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

UBENCH_EX(foo, orig_black) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves_black(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo, orig_white) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    get_team_moves_white(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

/*
UBENCH_EX(foo2, gen) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
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
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black2(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}
*/

UBENCH_EX(foo3, gen3_black) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black3(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(foo3, gen3_white) {
  const board start_board = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_white3(start_board, &total, ms, bs);
    UBENCH_DO_NOTHING(ms);
  }
}

UBENCH_EX(move, mm_black) {
  const board b = read_board(sanity_capture_king_string);
  board bs[235];
  move ms[235];
  dir ds[235];
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
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(board_occ(b), throne_mask));
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
  board bs[235];
  move ms[235];
  dir ds[235];
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
  op_layer_bit(throne_mask, 60, |=);
  layer free = layer_neg(layer_or(board_occ(b), throne_mask));
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
    move ms[235];
    layer ls[235];
    layer ls_r[235];
    int total = 0;
    moves_to(
        layer_neg(board_occ(start_board)),
        layer_neg(board_occ_r(start_board)),
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
    move ms[235];
    layer ls[235];
    layer ls_r[235];
    int total = 0;
    moves_to(
        layer_neg(board_occ(start_board)),
        layer_neg(board_occ_r(start_board)),
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
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_white3(start_board, &total, ms, bs);
    total_total += total;
    for (int i = 0; i < total; i++) {
      board bs2[235];
      move ms2[235];
      int total2 = 0;
      gen_reference_moves_white3(bs[i], &total2, ms2, bs2);
      total_total += total2;
      for (int j = 0; j < total; j++) {
        board bs3[235];
        move ms3[235];
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
  board bs[235];
  move ms[235];
  dir ds[235];
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
    op_layer_bit(throne_mask, 60, |=);
    layer free = layer_neg(layer_or(board_occ(b), throne_mask));
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

      board bs2[235];
      move ms2[235];
      dir ds2[235];
      int total2 = 0;

      layer throne_mask = EMPTY_LAYER;
      op_layer_bit(throne_mask, 60, |=);
      layer free = layer_neg(layer_or(board_occ(bs[i]), throne_mask));
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

        board bs3[235];
        move ms3[235];
        dir ds3[235];
        int total3 = 0;

        layer throne_mask = EMPTY_LAYER;
        op_layer_bit(throne_mask, 60, |=);
        layer free = layer_neg(layer_or(board_occ(bs2[j]), throne_mask));
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
    board bs[235];
    move ms[235];
    int total = 0;
    gen_reference_moves_black3(start_board, &total, ms, bs);
    total_total += total;
    for (int i = 0; i < total; i++) {
      board bs2[235];
      move ms2[235];
      int total2 = 0;
      gen_reference_moves_black3(bs[i], &total2, ms2, bs2);
      total_total += total2;
      for (int j = 0; j < total; j++) {
        board bs3[235];
        move ms3[235];
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
  board bs[235];
  move ms[235];
  dir ds[235];
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
    op_layer_bit(throne_mask, 60, |=);
    layer free = layer_neg(layer_or(board_occ(b), throne_mask));
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

      board bs2[235];
      move ms2[235];
      dir ds2[235];
      int total2 = 0;

      layer throne_mask = EMPTY_LAYER;
      op_layer_bit(throne_mask, 60, |=);
      layer free = layer_neg(layer_or(board_occ(bs[i]), throne_mask));
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

        board bs3[235];
        move ms3[235];
        dir ds3[235];
        int total3 = 0;

        layer throne_mask = EMPTY_LAYER;
        op_layer_bit(throne_mask, 60, |=);
        layer free = layer_neg(layer_or(board_occ(bs2[j]), throne_mask));
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

UBENCH_EX(move_count, white_orig) {
  const board b = read_board(sanity_capture_king_string);
  UBENCH_DO_BENCHMARK() {
    uint16_t c =
        get_team_move_count(board_occ(b), b.white, board_occ_r(b), b.white_r);
    UBENCH_DO_NOTHING(&c);
  }
}

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

  int king_pos = lowest_index(b.king);
  int king_rank = rank(king_pos);
  int king_file = file(king_pos);

  UBENCH_DO_BENCHMARK() {

    layer paths = EMPTY_LAYER;
    layer paths_r = EMPTY_LAYER;

    corner_paths_1(occ, occ_r, king_rank, king_file, &paths, &paths_r);
    UBENCH_DO_NOTHING(&paths);
    UBENCH_DO_NOTHING(&paths_r);
  }
}

const char *corner_access_double2 = " .  .  X  .  X  .  .  O  .  .  . "
                                   " .  X  .  X  .  .  .  .  .  .  . "
                                   " X  .  .  O  O  X  .  .  X  .  . "
                                   " .  .  .  .  .  .  X  .  .  X  . "
                                   " .  X  .  .  .  #  O  .  .  .  X "
                                   " X  .  .  O  O  .  O  .  .  X  X "
                                   " X  .  .  .  O  O  O  .  .  .  X "
                                   " X  .  .  .  .  O  .  .  .  .  X "
                                   " .  .  .  .  .  .  .  .  O  .  . "
                                   " .  .  .  .  .  X  .  .  .  .  . "
                                   " .  .  X  .  X  X  X  X  .  .  . ";


UBENCH_EX(king_mobility, corner_paths_2) {
  board b = read_board(corner_access_double2);

  // setup to generate layers
  layer occ = layer_or(b.black, b.white);
  layer occ_r = layer_or(b.black_r, b.white_r);

  int king_pos = lowest_index(b.king);
  int king_rank = rank(king_pos);
  int king_file = file(king_pos);

  UBENCH_DO_BENCHMARK() {

    layer paths = EMPTY_LAYER;
    layer paths_r = EMPTY_LAYER;

    corner_paths_2(occ, occ_r, king_rank, king_file, &paths, &paths_r);
    UBENCH_DO_NOTHING(&paths);
    UBENCH_DO_NOTHING(&paths_r);
  }
}


// needs to be at top level
UBENCH_STATE();

int main() {
  init_move_globals();
  return ubench_main(0, NULL);
}
