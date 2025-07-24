#include "constants.h"
#include "io.h"
#include "layer.h"
#include "limits.h"
#include "stdbool.h"
#include <stdio.h>

// -----------------------------------------------------------------------------
// Generic macros

#define _STR(_s) #_s
#define STR(_s) _STR(_s)

/* join tokens with an understore */
#define JOIN(_a, _b) _a##_##_b
#define JOIN3(_a, _b, _c) _a##_##_b##_##_c

/* Inserts a macro expension phase between a function and its
arguments, allowing those arguments to expand before being used inside
of the body of the function. */
#define APPLY(_func, _a) _func(_a)
#define APPLY2(_func, _a, _b) _func(_a, _b)
#define APPLY3(_func, _a, _b, _c) _func(_a, _b, _c)
#define APPLY4(_func, _a, _b, _c, _d) _func(_a, _b, _c, _e)
#define APPLY5(_func, _a, _b, _c, _d, _e) _func(_a, _b, _c, _d, _e)
#define APPLY6(_func, _a, _b, _c, _d, _e, _f) _func(_a, _b, _c, _d, _e, _f)

// -----------------------------------------------------------------------------
// Semi generic macros

// when converting bits left/right of a position in a row mask to a
// perpendicular layer representation, are those bits above or below
// the position
#define left_file below_n
#define right_file above_n
#define left_rank above_n
#define right_rank below_n
#define ROT_SIDE(_side, _axis) JOIN(_side, _axis)

// same as above but inclusive
#define left_file_inc below_n_inc
#define right_file_inc above_n_inc
#define left_rank_inc above_n_inc
#define right_rank_inc below_n_inc
#define ROT_SIDE_INC(_side, _axis) JOIN3(_side, _axis, inc)

#define INTO_ROW_0(_layer, _row) (_layer->_[0] |= _row)
#define INTO_ROW_1(_layer, _row) (_layer->_[0] |= ((u64)_row << 11))
#define INTO_ROW_2(_layer, _row) (_layer->_[0] |= ((u64)_row << 22))
#define INTO_ROW_3(_layer, _row) (_layer->_[0] |= ((u64)_row << 33))
#define INTO_ROW_4(_layer, _row) (_layer->_[0] |= ((u64)_row << 44))
#define INTO_ROW_5(_layer, _row)                                               \
  (_layer->_[0] |= ((u64)_row << 55), _layer->_[1] |= (_row >> 9))
#define INTO_ROW_6(_layer, _row) (_layer->_[1] |= ((u64)_row << 2))
#define INTO_ROW_7(_layer, _row) (_layer->_[1] |= ((u64)_row << 13))
#define INTO_ROW_8(_layer, _row) (_layer->_[1] |= ((u64)_row << 24))
#define INTO_ROW_9(_layer, _row) (_layer->_[1] |= ((u64)_row << 35))
#define INTO_ROW_10(_layer, _row) (_layer->_[1] |= ((u64)_row << 46))
#define INTO_ROW(_i, _layer, _row) JOIN(INTO_ROW, _i)(_layer, _row)

inline void into_row(layer *l, u16 row, int n) {
  switch (n) {
  case 0:
    INTO_ROW_0(l, row);
    break;
  case 1:
    INTO_ROW_1(l, row);
    break;
  case 2:
    INTO_ROW_2(l, row);
    break;
  case 3:
    INTO_ROW_3(l, row);
    break;
  case 4:
    INTO_ROW_4(l, row);
    break;
  case 5:
    INTO_ROW_5(l, row);
    break;
  case 6:
    INTO_ROW_6(l, row);
    break;
  case 7:
    INTO_ROW_7(l, row);
    break;
  case 8:
    INTO_ROW_8(l, row);
    break;
  case 9:
    INTO_ROW_9(l, row);
    break;
  case 10:
    INTO_ROW_10(l, row);
    break;
  }
}

#define DIRTY_GET_ROW_0(l) (u64) l._[0]
#define DIRTY_GET_ROW_1(l) ((u64)l._[0] >> 11)
#define DIRTY_GET_ROW_2(l) ((u64)l._[0] >> 22)
#define DIRTY_GET_ROW_3(l) ((u64)l._[0] >> 33)
#define DIRTY_GET_ROW_4(l) ((u64)l._[0] >> 44)
#define DIRTY_GET_ROW_5(l) ((u64)l._[0] >> 55) | ((((u64)l._[1] & 0x3) << 9))
#define DIRTY_GET_ROW_6(l) ((u64)l._[1] >> 2)
#define DIRTY_GET_ROW_7(l) ((u64)l._[1] >> 13)
#define DIRTY_GET_ROW_8(l) ((u64)l._[1] >> 24)
#define DIRTY_GET_ROW_9(l) ((u64)l._[1] >> 35)
#define DIRTY_GET_ROW_10(l) ((u64)l._[1] >> 46)
#define DIRTY_GET_ROW(_i, _l) JOIN(DIRTY_GET_ROW, _i)(_l)

#define VERT_INDEX_rank(_i) _i
#define VERT_INDEX_file_0 10
#define VERT_INDEX_file_1 9
#define VERT_INDEX_file_9 1
#define VERT_INDEX_file_10 0
#define VERT_INDEX_file(_i) JOIN(VERT_INDEX_file, _i)
#define VERT_INDEX(_axis, _i) JOIN(VERT_INDEX, _axis)(_i)

// inclusive of start position, non edge
#define CHOOSE_MASKER_1_left MASK_LEFTWARD
#define CHOOSE_MASKER_1_right MASK_RIGHTWARD
#define CHOOSE_MASKER_9_left MASK_LEFTWARD
#define CHOOSE_MASKER_9_right MASK_RIGHTWARD
// inclusive of start position, edge so excludes corners
#define CHOOSE_MASKER_0_left MASK_LEFTWARD_EDGE
#define CHOOSE_MASKER_0_right MASK_RIGHTWARD_EDGE
#define CHOOSE_MASKER_10_left MASK_LEFTWARD_EDGE
#define CHOOSE_MASKER_10_right MASK_RIGHTWARD_EDGE
// dispatch
#define CHOOSE_MASKER(_i, _side) JOIN3(CHOOSE_MASKER, _i, _side)

#define LEGAL_FILE_MASK_HALF(_i, _half) JOIN3(LEGAL_FILE_MASK, _i, _half)

#define LEGAL_FILE_MASK_0_0 36046397799139328ULL
#define LEGAL_FILE_MASK_0_1 34376523780ULL
#define LEGAL_FILE_MASK_0 ((layer){36046397799139328ULL, 34376523780ULL})
#define LEGAL_FILE_MASK_1_0 72092795598278658ULL
#define LEGAL_FILE_MASK_1_1 140806241402888ULL
#define LEGAL_FILE_MASK_1 ((layer){72092795598278658ULL, 140806241402888ULL})
#define LEGAL_FILE_MASK_5_0 1153484729572458528ULL
#define LEGAL_FILE_MASK_5_1 2252899862446208ULL
#define LEGAL_FILE_MASK_9_0 9011599449784832ULL
#define LEGAL_FILE_MASK_9_1 36046397799139329ULL
#define LEGAL_FILE_MASK_9 ((layer){9011599449784832ULL, 36046397799139329ULL})
#define LEGAL_FILE_MASK_10_0 18023198899568640ULL
#define LEGAL_FILE_MASK_10_1 35201560350722ULL
#define LEGAL_FILE_MASK_10 ((layer){18023198899568640ULL, 35201560350722ULL})

#define FILE_MASK_2 ((layer){144185591196557316ULL, 281612482805776ULL})
#define FILE_MASK_3 ((layer){288371182393114632ULL, 563224965611552ULL})
#define FILE_MASK_4 ((layer){576742364786229264ULL, 1126449931223104ULL})
#define FILE_MASK_5 ((layer){1153484729572458528ULL, 2252899862446208ULL})
#define FILE_MASK_6 ((layer){2306969459144917056ULL, 4505799724892416ULL})
#define FILE_MASK_7 ((layer){4613938918289834112ULL, 9011599449784832ULL})
#define FILE_MASK_8 ((layer){9227877836579668224ULL, 18023198899569664ULL})

#define ADJUST_AXIS_VAL_file(_i) _i
#define ADJUST_AXIS_VAL_rank(_i) (10 - _i)
#define ADJUST_AXIS_VAL(_axis) JOIN(ADJUST_AXIS_VAL, _axis)(_axis)
#define ADJUST_AXIS_VAL_I(_axis, _i) JOIN(ADJUST_AXIS_VAL, _axis)(_i)

// excludes origin, includes edges (corners)
#define MASK_RIGHTWARD(x) (((u16)1 << x) - 1)
#define MASK_LEFTWARD(x) ((0x7fe << x) & 0x7fe)

// excludes origin, excludes edges (corners)
#define MASK_RIGHTWARD_EDGE(x) (((u16)1 << x) - 2)
#define MASK_LEFTWARD_EDGE(x) ((0x3fe << x) & 0x3fe)

// includes x
#define MASK_RIGHTWARD_INC(x) (((u16)2 << x) - 1)
#define MASK_LEFTWARD_INC(x) ((0x7ff << x) & 0x7ff)

// includes x; only applicable at the edge; excludes corners
#define MASK_RIGHTWARD_INC_EDGE(x) (((u16)2 << x) - 2)
#define MASK_LEFTWARD_INC_EDGE(x) ((0x3ff << x) & 0x3ff)

#define LEGAL_EDGE_MASK 0b01111111110

#define APPLY_LEGAL_EDGE_MASK_0(_row) (_row & LEGAL_EDGE_MASK)
#define APPLY_LEGAL_EDGE_MASK_1(_row) (_row)
#define APPLY_LEGAL_EDGE_MASK_5(_row) (_row)
#define APPLY_LEGAL_EDGE_MASK_9(_row) (_row)
#define APPLY_LEGAL_EDGE_MASK_10(_row) (_row & LEGAL_EDGE_MASK)

/*
const layer above_0 = {18446744073709549568ULL, 18446744073709551615ULL};
const layer above_1 = {18446744073705357312ULL, 18446744073709551615ULL};
const layer above_2 = {18446744065119617024ULL, 18446744073709551615ULL};
const layer above_3 = {18446726481523507200ULL, 18446744073709551615ULL};
const layer above_4 = {18410715276690587648ULL, 18446744073709551615ULL};
const layer above_5 = {0ULL, 18446744073709551612ULL};
const layer above_6 = {0ULL, 18446744073709543424ULL};
const layer above_7 = {0ULL, 18446744073692774400ULL};
const layer above_8 = {0ULL, 18446744039349813248ULL};
const layer above_9 = {0ULL, 18446673704965373952ULL};
const layer above_10 = {0ULL, 18302628885633695744ULL};
*/

// excludes index
layer above_n[11] = {
    above_0,
    above_1,
    above_2,
    above_3,
    above_4,
    above_5,
    above_6,
    above_7,
    above_8,
    above_9,
    above_10,
};

// includes index
layer above_n_inc[11] = {
    (layer){ULLONG_MAX, ULLONG_MAX},
    above_0,
    above_1,
    above_2,
    above_3,
    above_4,
    above_5,
    above_6,
    above_7,
    above_8,
    above_9,
};

/*
const layer below_0 = {0ULL, 0ULL};
const layer below_1 = {2047ULL, 0ULL};
const layer below_2 = {4194303ULL, 0ULL};
const layer below_3 = {8589934591ULL, 0ULL};
const layer below_4 = {17592186044415ULL, 0ULL};
const layer below_5 = {36028797018963967ULL, 0ULL};
const layer below_6 = {18446744073709551615ULL, 3ULL};
const layer below_7 = {18446744073709551615ULL, 8191ULL};
const layer below_8 = {18446744073709551615ULL, 16777215ULL};
const layer below_9 = {18446744073709551615ULL, 34359738367ULL};
const layer below_10 = {18446744073709551615ULL, 70368744177663ULL};
*/

// excludes index
layer below_n[11] = {
    below_0,
    below_1,
    below_2,
    below_3,
    below_4,
    below_5,
    below_6,
    below_7,
    below_8,
    below_9,
    below_10,
};

// includes index
layer below_n_inc[11] = {
    below_1,
    below_2,
    below_3,
    below_4,
    below_5,
    below_6,
    below_7,
    below_8,
    below_9,
    below_10,
    (layer){ULLONG_MAX, ULLONG_MAX}};

// TODO: try implemting with lookup table and see if it's faster
// Mask (_from, _to]
#define MASK_LEFT_FROM_TO(_from, _to) (((u16)2 << _to) - ((u16)2 << _from))
#define MASK_RIGHT_FROM_TO(_from, _to) (((u16)1 << _from) - (1 << _to))

const layer legal_file_masks[] = {
    LEGAL_FILE_MASK_0,
    LEGAL_FILE_MASK_1,
    FILE_MASK_2,
    FILE_MASK_3,
    FILE_MASK_4,
    FILE_MASK_5,
    FILE_MASK_6,
    FILE_MASK_7,
    FILE_MASK_8,
    LEGAL_FILE_MASK_9,
    LEGAL_FILE_MASK_10};

#define LEGAL_FILE_MASK_DYN(_i) legal_file_masks[_i]

#define FILE_MASK_ADJACENT_0 ((layer){36046397799139328ULL, 34376523780ULL})
#define FILE_MASK_ADJACENT_1 ((layer){72092795598278656ULL, 68753047560ULL})
#define FILE_MASK_ADJACENT_2 ((layer){144185591196557312ULL, 137506095120ULL})
#define FILE_MASK_ADJACENT_3 ((layer){288371182393114624ULL, 275012190240ULL})
#define FILE_MASK_ADJACENT_4 ((layer){576742364786229248ULL, 550024380480ULL})
#define FILE_MASK_ADJACENT_5 ((layer){1153484729572458496ULL, 1100048760960ULL})
#define FILE_MASK_ADJACENT_6 ((layer){2306969459144916992ULL, 2200097521920ULL})
#define FILE_MASK_ADJACENT_7 ((layer){4613938918289833984ULL, 4400195043840ULL})
#define FILE_MASK_ADJACENT_8 ((layer){9227877836579667968ULL, 8800390087680ULL})
#define FILE_MASK_ADJACENT_9 ((layer){9011599449784320ULL, 17600780175361ULL})
#define FILE_MASK_ADJACENT_10 ((layer){18023198899568640ULL, 35201560350722ULL})

const layer file_mask_adjacent[] = {
    FILE_MASK_ADJACENT_0,
    FILE_MASK_ADJACENT_1,
    FILE_MASK_ADJACENT_2,
    FILE_MASK_ADJACENT_3,
    FILE_MASK_ADJACENT_4,
    FILE_MASK_ADJACENT_5,
    FILE_MASK_ADJACENT_6,
    FILE_MASK_ADJACENT_7,
    FILE_MASK_ADJACENT_8,
    FILE_MASK_ADJACENT_9,
    FILE_MASK_ADJACENT_10};

#define FILE_MASK_ADJACENT_DYN(_i) file_mask_adjacent[_i]

// -----------------------------------------------------------------------------
// corner moves components

#define PERP_STEM_BASE_0 LEGAL_FILE_MASK_DYN
#define PERP_STEM_BASE_1 FILE_MASK_ADJACENT_DYN
#define PERP_STEM_BASE_9 FILE_MASK_ADJACENT_DYN
#define PERP_STEM_BASE_10 LEGAL_FILE_MASK_DYN
#define PERP_STEM_BASE(_i) JOIN(PERP_STEM_BASE, _i)
#define STEM_BASE_HALF(_axis, _target, _half)                                  \
  APPLY(PERP_STEM_BASE(_target), _axis)._[_half]

#define TARGET_0 right
#define TARGET_1 right
#define TARGET_9 left
#define TARGET_10 left
#define TARGET_SIDE(_target) JOIN(TARGET, _target)

#define AXIS_PATHS_file paths_r
#define AXIS_PATHS_rank paths
#define AXIS_PATHS(_axis) JOIN(AXIS_PATHS, _axis)

#define AXIS_OCC_file occ_r
#define AXIS_OCC_rank occ
#define AXIS_OCC(_axis) JOIN(AXIS_OCC, _axis)

#define NOT_rank file
#define NOT_file rank
#define NOT_AXIS(_axis) JOIN(NOT, _axis)

#define TARGET_MASKER_10 MASK_LEFT_FROM_TO
#define TARGET_MASKER_9 MASK_LEFT_FROM_TO
#define TARGET_MASKER_1 MASK_RIGHT_FROM_TO
#define TARGET_MASKER_0 MASK_RIGHT_FROM_TO
#define TARGET_MASKER(_target) JOIN(TARGET_MASKER, _target)

#define CAN_REACH_POS_0 (pos & (row - 1))
#define CAN_REACH_POS_1 (pos & (row - 2))
#define CAN_REACH_POS_9 (0b01000000000 & ~row & (row - pos))
#define CAN_REACH_POS_10 (0b10000000000 & ~row & (row - pos))
#define CAN_REACH_POS(_i) JOIN(CAN_REACH_POS, _i)

#define BOTH_SIDES_INC(                                                        \
    _target,                                                                   \
    _axis,                                                                     \
    _occ,                                                                      \
    _parallel_paths,                                                           \
    _perpendicular_paths)                                                      \
  {                                                                            \
    u16 row = DIRTY_GET_ROW(_target, AXIS_OCC(NOT_AXIS(_axis)));               \
    {                                                                          \
      const u16 mask = MASK_LEFTWARD_INC(ADJUST_AXIS_VAL(_axis));              \
      if (!(row & mask)) {                                                     \
        ADD_PATHS_INC(                                                         \
            _target,                                                           \
            _axis,                                                             \
            left,                                                              \
            AXIS_PATHS(NOT_AXIS(_axis)),                                       \
            AXIS_PATHS(_axis));                                                \
      }                                                                        \
    }                                                                          \
    {                                                                          \
      const u16 mask = MASK_RIGHTWARD_INC(ADJUST_AXIS_VAL(_axis));             \
      if (!(row & mask)) {                                                     \
        ADD_PATHS_INC(                                                         \
            _target,                                                           \
            _axis,                                                             \
            right,                                                             \
            AXIS_PATHS(NOT_AXIS(_axis)),                                       \
            AXIS_PATHS(_axis));                                                \
      }                                                                        \
    }                                                                          \
  }

#define ADD_PATHS(_i, _axis, _side, _parallel_paths, _perpendicular_paths)     \
  {                                                                            \
    INTO_ROW(_i, _parallel_paths, JOIN(APPLY_LEGAL_EDGE_MASK, _i)(mask));      \
    _perpendicular_paths->_[0] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 0) &                      \
         ROT_SIDE(_side, NOT_AXIS(_axis))[_axis]._[0]);                        \
    _perpendicular_paths->_[1] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 1) &                      \
         ROT_SIDE(_side, NOT_AXIS(_axis))[_axis]._[1]);                        \
  }

#define ADD_PATHS_INC(_i, _axis, _side, _parallel_paths, _perpendicular_paths) \
  {                                                                            \
    INTO_ROW(_i, _parallel_paths, JOIN(APPLY_LEGAL_EDGE_MASK, _i)(mask));      \
    _perpendicular_paths->_[0] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 0) &                      \
         ROT_SIDE_INC(_side, NOT_AXIS(_axis))[_axis]._[0]);                    \
    _perpendicular_paths->_[1] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 1) &                      \
         ROT_SIDE_INC(_side, NOT_AXIS(_axis))[_axis]._[1]);                    \
  }

#define BOTH_SIDES(_i, _variable, _occ, _parallel_paths, _perpendicular_paths) \
  {                                                                            \
    u16 row = DIRTY_GET_ROW(_i, _occ);                                         \
    {                                                                          \
      const u16 mask = CHOOSE_MASKER(_i, left)(ADJUST_AXIS_VAL(_variable));    \
      if (!(row & mask)) {                                                     \
        ADD_PATHS(_i, _variable, left, _parallel_paths, _perpendicular_paths); \
      }                                                                        \
    }                                                                          \
    {                                                                          \
      const u16 mask = CHOOSE_MASKER(_i, right)(ADJUST_AXIS_VAL(_variable));   \
      if (!(row & mask)) {                                                     \
        ADD_PATHS(                                                             \
            _i,                                                                \
            _variable,                                                         \
            right,                                                             \
            _parallel_paths,                                                   \
            _perpendicular_paths);                                             \
      }                                                                        \
    }                                                                          \
  }

// TODO: even though the checks for adjacent squares will overlap, it doesn't
// appear to help performance to gate them behind each other. Might be a matter
// of inaccurate microbenchmarks. An alternative version to try would double
// this macro up: take two targets, the inner and outer, do and exclusive origin
// check at the inner target, an inclusive at the outer targer, and then add a
// stem to the adjacent row in the event of any escape.
#define BOTH_SIDES_STEM_GEN(_axis, _target)                                    \
  if (CAN_REACH_POS(_target)) {                                                \
    bool escape = false;                                                       \
    u16 row =                                                                  \
        DIRTY_GET_ROW(VERT_INDEX(_axis, _target), AXIS_OCC(NOT_AXIS(_axis)));  \
    {                                                                          \
      const u16 mask = MASK_LEFTWARD(ADJUST_AXIS_VAL(_axis));                  \
      if (!(row & mask)) {                                                     \
        ADD_PATHS(                                                             \
            VERT_INDEX(_axis, _target),                                        \
            _axis,                                                             \
            left,                                                              \
            AXIS_PATHS(NOT_AXIS(_axis)),                                       \
            AXIS_PATHS(_axis));                                                \
        /*                                                                     \
         */                                                                    \
        escape = true;                                                         \
      }                                                                        \
    }                                                                          \
    {                                                                          \
      const u16 mask = MASK_RIGHTWARD(ADJUST_AXIS_VAL(_axis));                 \
      if (!(row & mask)) {                                                     \
        ADD_PATHS(                                                             \
            VERT_INDEX(_axis, _target),                                        \
            _axis,                                                             \
            right,                                                             \
            AXIS_PATHS(NOT_AXIS(_axis)),                                       \
            AXIS_PATHS(_axis));                                                \
        /*                                                                     \
         */                                                                    \
        escape = true;                                                         \
      }                                                                        \
    }                                                                          \
    if (escape) {                                                              \
      u16 stem =                                                               \
          TARGET_MASKER(_target)(ADJUST_AXIS_VAL(NOT_AXIS(_axis)), _target);   \
      into_row(AXIS_PATHS(_axis), stem, _axis);                                \
      AXIS_PATHS(NOT_AXIS(_axis))->_[0] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _target, 0) &                 \
          ROT_SIDE(TARGET_SIDE(_target), _axis)[NOT_AXIS(_axis)]._[0];         \
      AXIS_PATHS(NOT_AXIS(_axis))->_[1] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _target, 1) &                 \
          ROT_SIDE(TARGET_SIDE(_target), _axis)[NOT_AXIS(_axis)]._[1];         \
    }                                                                          \
  }

#define CASE(_x, _body)                                                        \
  case _x: {                                                                   \
    _body                                                                      \
  }; break;

/**
 * Get layer mask for all open 1-move routes to a corner.
 */
void corner_paths_1(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r) {

  switch (rank) {
    CASE(0, BOTH_SIDES(0, file, occ, paths, paths_r));
    CASE(1, BOTH_SIDES(1, file, occ, paths, paths_r));
    CASE(9, BOTH_SIDES(9, file, occ, paths, paths_r));
    CASE(10, BOTH_SIDES(10, file, occ, paths, paths_r));
  }

  switch (file) {
    CASE(0, BOTH_SIDES(0, rank, occ_r, paths_r, paths));
    CASE(1, BOTH_SIDES(1, rank, occ_r, paths_r, paths));
    CASE(9, BOTH_SIDES(9, rank, occ_r, paths_r, paths));
    CASE(10, BOTH_SIDES(10, rank, occ_r, paths_r, paths));
  }
}

void corner_paths_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r) {

  // on file 0 or 10 we should only do the file methods
  if (file == 0) {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;

    BOTH_SIDES(0, rank, occ_r, paths_r, paths);
    BOTH_SIDES_INC(1, rank, occ_r, paths_r, paths);
    BOTH_SIDES_STEM_GEN(rank, 9);

    // TODO: I think I can actually get rid of these fallback checks;
    if (rank == 9) {
      BOTH_SIDES(9, file, occ, paths, paths_r);
    } else if (rank == 1) {
      BOTH_SIDES(1, file, occ, paths, paths_r);
    } else {
      BOTH_SIDES_STEM_GEN(rank, 10);
    }

    return;
  }

  if (file == 10) {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;

    if (rank == 9) {
      BOTH_SIDES(9, file, occ, paths, paths_r);
    } else if (rank == 1) {
      BOTH_SIDES(1, file, occ, paths, paths_r);
    } else {
      BOTH_SIDES_STEM_GEN(rank, 0);
    }

    BOTH_SIDES_STEM_GEN(rank, 1);
    BOTH_SIDES_INC(9, rank, occ_r, paths_r, paths);
    BOTH_SIDES(10, rank, occ_r, paths_r, paths);

    return;
  }

  // on rank 0 or 10 we should only do the rank methods
  if (rank == 0) {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    BOTH_SIDES(0, file, occ, paths, paths_r);
    BOTH_SIDES_INC(1, file, occ, paths, paths_r);
    BOTH_SIDES_STEM_GEN(file, 1);
    if (file == 9) {
      BOTH_SIDES(9, rank, occ_r, paths_r, paths);
    } else if (file == 1) {
      BOTH_SIDES(1, rank, occ_r, paths_r, paths);
    } else {
      BOTH_SIDES_STEM_GEN(file, 0);
    }
    return;
  }

  if (rank == 10) {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    BOTH_SIDES_INC(9, file, occ, paths, paths_r);
    BOTH_SIDES(10, file, occ, paths, paths_r);
    BOTH_SIDES_STEM_GEN(file, 9);
    if (file == 9) {
      BOTH_SIDES(9, rank, occ_r, paths_r, paths);
    } else if (file == 1) {
      BOTH_SIDES(1, rank, occ_r, paths_r, paths);
    } else {
      BOTH_SIDES_STEM_GEN(file, 10);
    }
    return;
  }

  // NOTE: on adjacent rank/file the BOTH_SIDES_STEM_GEN to the edge is redunant
  // (for example, file 9 going all the way up to rank 10 and then across to
  // file 1). It's enough to reach rank 10 from file 9, it's already a corner
  // adjacent square with a guaranteed exit. Might be worth bench an
  // implementation that just does BOTH_SIDES to the edge.

  {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;

    switch (file) {
    case 1: {
      // inner
      BOTH_SIDES(1, rank, occ_r, paths_r, paths);
      BOTH_SIDES_STEM_GEN(rank, 9);

      // outer
      // An idea to avoid this check: add artifical blockers so that the stem
      // check itself just fails
      if (rank != 1 && rank != 9) {
        BOTH_SIDES_INC(0, rank, occ_r, paths_r, paths);
        BOTH_SIDES_STEM_GEN(rank, 10);
      }
    }; break;
    case 9: {
      // inner
      BOTH_SIDES_STEM_GEN(rank, 1);
      BOTH_SIDES(9, rank, occ_r, paths_r, paths);

      // outer
      if (rank != 1 && rank != 9) {
        BOTH_SIDES_STEM_GEN(rank, 0);
        BOTH_SIDES_INC(10, rank, occ_r, paths_r, paths);
      }
    }; break;
    default: {
      // inner
      BOTH_SIDES_STEM_GEN(rank, 1);
      BOTH_SIDES_STEM_GEN(rank, 9);

      // outer
      if (rank != 1 && rank != 9) {
        BOTH_SIDES_STEM_GEN(rank, 0);
        BOTH_SIDES_STEM_GEN(rank, 10);
      }
    }; break;
    }
  }

  {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    switch (rank) {
    case 1: {
      // inner
      BOTH_SIDES(1, file, occ, paths, paths_r);
      BOTH_SIDES_STEM_GEN(file, 1);

      // outer
      if (file != 1 && file != 9) {
        BOTH_SIDES_INC(0, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_GEN(file, 0);
      }
    }; break;
    case 9: {
      // inner
      BOTH_SIDES(9, file, occ, paths, paths_r);
      BOTH_SIDES_STEM_GEN(file, 9);

      // outer
      if (file != 1 && file != 9) {
        BOTH_SIDES_INC(10, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_GEN(file, 10);
      }
    }; break;
    default: {
      // inner
      BOTH_SIDES_STEM_GEN(file, 1);
      BOTH_SIDES_STEM_GEN(file, 9);

      // outer
      if (file != 1 && file != 9) {
        BOTH_SIDES_STEM_GEN(file, 0);
        BOTH_SIDES_STEM_GEN(file, 10);
      }
    }; break;
    }
  }
}

/* _axis is a token named either rank or file, which determines the
axis _i should be interpreted as. _i is an int constant. */
#define GET_POS(_axis, _i)                                                     \
  (GET_AXIS_VAL(_axis, _axis) +                                                \
   GET_AXIS_VAL(NOT_AXIS(_axis), ADJUST_AXIS_VAL_I(NOT_AXIS(_axis), _i)))
#define GET_AXIS_VAL_rank(_i) (_i * 11)
#define GET_AXIS_VAL_file(_i) _i
#define GET_AXIS_VAL(_axis, _i) JOIN(GET_AXIS_VAL, _axis)(_i)

#define ADD_MOVE(_pos)                                                         \
  dests[(*total)] = _pos;                                                      \
  (*total)++;

// this is used directly from the king, so there's no intermediate
// move; we therefore return true as the king has excaped.
#define BOTH_SIDES_MOVE(_i, _variable, _occ)                                   \
  {                                                                            \
    u16 row = DIRTY_GET_ROW(_i, _occ);                                         \
    {                                                                          \
      const u16 left_mask =                                                    \
          CHOOSE_MASKER(_i, left)(ADJUST_AXIS_VAL(_variable));                 \
      const u16 right_mask =                                                   \
          CHOOSE_MASKER(_i, right)(ADJUST_AXIS_VAL(_variable));                \
      if (!(row & left_mask) || !(row & right_mask)) {                         \
        return true;                                                           \
      }                                                                        \
    }                                                                          \
  }

// TODO: is the vert indexing I'm doing here actually necessary, here or
// anywhere?
#define BOTH_SIDES_MOVE_INC(_target, _axis, _occ)                              \
  {                                                                            \
    u16 row = DIRTY_GET_ROW(_target, AXIS_OCC(NOT_AXIS(_axis)));               \
    {                                                                          \
      const u16 left_mask = MASK_LEFTWARD_INC(ADJUST_AXIS_VAL(_axis));         \
      const u16 right_mask = MASK_RIGHTWARD_INC(ADJUST_AXIS_VAL(_axis));       \
      if (!(row & left_mask) || !(row & right_mask)) {                         \
        ADD_MOVE(GET_POS(_axis, VERT_INDEX(_axis, _target)))                   \
      }                                                                        \
    }                                                                          \
  }

#define BOTH_SIDES_MOVE_STEM_GEN(_axis, _target)                               \
  if (CAN_REACH_POS(_target)) {                                                \
    u16 row =                                                                  \
        DIRTY_GET_ROW(VERT_INDEX(_axis, _target), AXIS_OCC(NOT_AXIS(_axis)));  \
    {                                                                          \
      const u16 left_mask = MASK_LEFTWARD(ADJUST_AXIS_VAL(_axis));             \
      const u16 right_mask = MASK_RIGHTWARD(ADJUST_AXIS_VAL(_axis));           \
      if (!(row & left_mask) || !(row & right_mask)) {                         \
        ADD_MOVE(GET_POS(_axis, _target))                                      \
      }                                                                        \
    }                                                                          \
  }

// TODO: consider removing the single move checks from this function and using
// it in conjuntion with the other function.
// IMPORTANT: occ must not include the king
bool corner_moves_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    int *dests,
    int *total) {

  // on file 0 or 10 we should only do the file methods
  if (file == 0) {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;
    BOTH_SIDES_MOVE(0, rank, occ_r);
    BOTH_SIDES_MOVE_INC(1, rank, occ_r);
    BOTH_SIDES_MOVE_STEM_GEN(rank, 10);
    BOTH_SIDES_MOVE_STEM_GEN(rank, 9);
    return false;
  }
  if (file == 10) {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;
    BOTH_SIDES_MOVE_STEM_GEN(rank, 0);
    BOTH_SIDES_MOVE_STEM_GEN(rank, 1);
    BOTH_SIDES_MOVE_INC(9, rank, occ_r);
    BOTH_SIDES_MOVE(10, rank, occ_r);
    return false;
  }

  // on rank 0 or 10 we should only do the rank methods
  if (rank == 0) {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    BOTH_SIDES_MOVE(0, file, occ);
    BOTH_SIDES_MOVE_INC(1, file, occ);
    BOTH_SIDES_MOVE_STEM_GEN(file, 0);
    BOTH_SIDES_MOVE_STEM_GEN(file, 1);
    return false;
  }

  if (rank == 10) {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    BOTH_SIDES_MOVE_INC(9, file, occ);
    BOTH_SIDES_MOVE(10, file, occ);
    BOTH_SIDES_MOVE_STEM_GEN(file, 9);
    BOTH_SIDES_MOVE_STEM_GEN(file, 10);
    return false;
  }

  // NOTE: on adjacent rank/file the BOTH_SIDES_MOVE_STEM_GEN to the edge is
  // redunant (for example, file 9 going all the way up to rank 10 and then
  // across to file 1). It's enough to reach rank 10 from file 9, it's already a
  // corner adjacent square with a guaranteed exit. Might be worth bench an
  // implementation that just does BOTH_SIDES_MOVE to the edge.

  {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;

    switch (file) {
    case 1: {
      BOTH_SIDES_MOVE_INC(0, rank, occ_r);
      BOTH_SIDES_MOVE(1, rank, occ_r);
      BOTH_SIDES_MOVE_STEM_GEN(rank, 9);
      BOTH_SIDES_MOVE_STEM_GEN(rank, 10);
    }; break;
    case 9: {
      BOTH_SIDES_MOVE_STEM_GEN(rank, 0);
      BOTH_SIDES_MOVE_STEM_GEN(rank, 1);
      BOTH_SIDES_MOVE(9, rank, occ_r);
      BOTH_SIDES_MOVE_INC(10, rank, occ_r);
    }; break;
    default: {
      BOTH_SIDES_MOVE_STEM_GEN(rank, 0);
      BOTH_SIDES_MOVE_STEM_GEN(rank, 1);
      BOTH_SIDES_MOVE_STEM_GEN(rank, 9);
      BOTH_SIDES_MOVE_STEM_GEN(rank, 10);
    }; break;
    }
  }

  {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    switch (rank) {
    case 1: {
      BOTH_SIDES_MOVE_INC(0, file, occ);
      BOTH_SIDES_MOVE(1, file, occ);
      BOTH_SIDES_MOVE_STEM_GEN(file, 0);
      BOTH_SIDES_MOVE_STEM_GEN(file, 1);
    }; break;
    case 9: {
      BOTH_SIDES_MOVE_INC(10, file, occ);
      BOTH_SIDES_MOVE(9, file, occ);
      BOTH_SIDES_MOVE_STEM_GEN(file, 9);
      BOTH_SIDES_MOVE_STEM_GEN(file, 10);
    }; break;
    default: {
      BOTH_SIDES_MOVE_STEM_GEN(file, 0);
      BOTH_SIDES_MOVE_STEM_GEN(file, 1);
      BOTH_SIDES_MOVE_STEM_GEN(file, 9);
      BOTH_SIDES_MOVE_STEM_GEN(file, 10);
    }; break;
    }
  }
  return false;
}

/**
 * Check if the king can reach a corner in 1 move
 *
 * because this is used on white's turn we can skip actually
 * generating moves--it's enough to know that the king can
 * reach the corner. Instead it should be used as a static heuristic.
 */
bool corner_moves_1(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file) {

  u16 row;

  if (rank == 0) {
    row = DIRTY_GET_ROW_0(occ);
    if (!(row & MASK_LEFTWARD(file)) || !(row & MASK_RIGHTWARD(file))) {
      return true;
    }
  } else if (rank == 1) {
    row = DIRTY_GET_ROW_1(occ);
    if (!(row & MASK_LEFTWARD(file)) || !(row & MASK_RIGHTWARD(file))) {
      return true;
    }
  } else if (rank == 9) {
    row = DIRTY_GET_ROW_9(occ);
    if (!(row & MASK_LEFTWARD(file)) || !(row & MASK_RIGHTWARD(file))) {
      return true;
    }
  } else if (rank == 10) {
    row = DIRTY_GET_ROW_10(occ);
    if (!(row & MASK_LEFTWARD(file)) || !(row & MASK_RIGHTWARD(file))) {
      return true;
    }
  }

  if (file == 0) {
    row = DIRTY_GET_ROW_0(occ_r);
    if (!(row & MASK_LEFTWARD(rank)) || !(row & MASK_RIGHTWARD(rank))) {
      return true;
    }
  } else if (file == 1) {
    row = DIRTY_GET_ROW_1(occ_r);
    if (!(row & MASK_LEFTWARD(rank)) || !(row & MASK_RIGHTWARD(rank))) {
      return true;
    }
  } else if (file == 9) {
    row = DIRTY_GET_ROW_9(occ_r);
    if (!(row & MASK_LEFTWARD(rank)) || !(row & MASK_RIGHTWARD(rank))) {
      return true;
    }
  } else if (file == 10) {
    row = DIRTY_GET_ROW_10(occ_r);
    if (!(row & MASK_LEFTWARD(rank)) || !(row & MASK_RIGHTWARD(rank))) {
      return true;
    }
  }

  return false;
}
