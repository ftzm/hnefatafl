#include "board.h"
#include "constants.h"
#include "layer.h"
#include "limits.h"
#include "macro_util.h"
#include "stdbool.h"
#include "x86intrin.h" // IWYU pragma: export

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

inline void __attribute__((always_inline)) into_row(layer *l, u16 row, int n) {
  if (n <= 5) {
    l->_[0] |= (u64)row << (n * 11);
    if (__builtin_expect(n == 5, 0))
      l->_[1] |= (u64)row >> 9;
  } else {
    l->_[1] |= (u64)row << (n * 11 - 64);
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

// inclusive of start position, non edge
#define CHOOSE_MASKER_INC_1_left MASK_LEFTWARD_INC
#define CHOOSE_MASKER_INC_1_right MASK_RIGHTWARD_INC
#define CHOOSE_MASKER_INC_9_left MASK_LEFTWARD_INC
#define CHOOSE_MASKER_INC_9_right MASK_RIGHTWARD_INC
// inclusive of start position, edge so excludes corners
#define CHOOSE_MASKER_INC_0_left MASK_LEFTWARD_INC_EDGE
#define CHOOSE_MASKER_INC_0_right MASK_RIGHTWARD_INC_EDGE
#define CHOOSE_MASKER_INC_10_left MASK_LEFTWARD_INC_EDGE
#define CHOOSE_MASKER_INC_10_right MASK_RIGHTWARD_INC_EDGE
// dispatch
#define CHOOSE_MASKER_INC(_i, _side) JOIN3(CHOOSE_MASKER_INC, _i, _side)

#define LEGAL_FILE_MASK_HALF(_i, _half) JOIN3(LEGAL_FILE_MASK, _i, _half)

#define LEGAL_FILE_MASK_0_0 36046397799139328ULL
#define LEGAL_FILE_MASK_0_1 34376523780ULL
#define LEGAL_FILE_MASK_0 ((layer){{36046397799139328ULL, 34376523780ULL}})
#define LEGAL_FILE_MASK_1_0 72092795598278658ULL
#define LEGAL_FILE_MASK_1_1 140806241402888ULL
#define LEGAL_FILE_MASK_1 ((layer){{72092795598278658ULL, 140806241402888ULL}})
#define LEGAL_FILE_MASK_5_0 1153484729572458528ULL
#define LEGAL_FILE_MASK_5_1 2252899862446208ULL
#define LEGAL_FILE_MASK_9_0 9011599449784832ULL
#define LEGAL_FILE_MASK_9_1 36046397799139329ULL
#define LEGAL_FILE_MASK_9 ((layer){{9011599449784832ULL, 36046397799139329ULL}})
#define LEGAL_FILE_MASK_10_0 18023198899568640ULL
#define LEGAL_FILE_MASK_10_1 35201560350722ULL
#define LEGAL_FILE_MASK_10 ((layer){{18023198899568640ULL, 35201560350722ULL}})

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
const layer above_n[11] = {
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
const layer above_n_inc[11] = {
    (layer){{ULLONG_MAX, ULLONG_MAX}},
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

// excludes index
const layer below_n[11] = {
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
const layer below_n_inc[11] = {
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
    (layer){{ULLONG_MAX, ULLONG_MAX}}};

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

const layer file_mask_adjacent[] = {
    FILE_MASK_EDGE_ADJACENT_0,
    FILE_MASK_EDGE_ADJACENT_1,
    FILE_MASK_EDGE_ADJACENT_2,
    FILE_MASK_EDGE_ADJACENT_3,
    FILE_MASK_EDGE_ADJACENT_4,
    FILE_MASK_EDGE_ADJACENT_5,
    FILE_MASK_EDGE_ADJACENT_6,
    FILE_MASK_EDGE_ADJACENT_7,
    FILE_MASK_EDGE_ADJACENT_8,
    FILE_MASK_EDGE_ADJACENT_9,
    FILE_MASK_EDGE_ADJACENT_10};

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
      const u16 mask =                                                         \
          CHOOSE_MASKER_INC(_target, left)(ADJUST_AXIS_VAL(_axis));            \
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
      const u16 mask =                                                         \
          CHOOSE_MASKER_INC(_target, right)(ADJUST_AXIS_VAL(_axis));           \
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
    INTO_ROW(_i, _parallel_paths, mask);                                       \
    _perpendicular_paths->_[0] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 0)                        \
         & ROT_SIDE(_side, NOT_AXIS(_axis))[_axis]._[0]);                      \
    _perpendicular_paths->_[1] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 1)                        \
         & ROT_SIDE(_side, NOT_AXIS(_axis))[_axis]._[1]);                      \
  }

#define ADD_PATHS_INC(_i, _axis, _side, _parallel_paths, _perpendicular_paths) \
  {                                                                            \
    INTO_ROW(_i, _parallel_paths, mask);                                       \
    _perpendicular_paths->_[0] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 0)                        \
         & ROT_SIDE_INC(_side, NOT_AXIS(_axis))[_axis]._[0]);                  \
    _perpendicular_paths->_[1] |=                                              \
        (LEGAL_FILE_MASK_HALF(VERT_INDEX(_axis, _i), 1)                        \
         & ROT_SIDE_INC(_side, NOT_AXIS(_axis))[_axis]._[1]);                  \
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
      const u16 mask = CHOOSE_MASKER(VERT_INDEX(_axis, _target), left)(        \
          ADJUST_AXIS_VAL(_axis));                                             \
      if (!(row & mask)) {                                                     \
        ADD_PATHS(                                                             \
            VERT_INDEX(_axis, _target),                                        \
            _axis,                                                             \
            left,                                                              \
            AXIS_PATHS(NOT_AXIS(_axis)),                                       \
            AXIS_PATHS(_axis));                                                \
        escape = true;                                                         \
      }                                                                        \
    }                                                                          \
    {                                                                          \
      const u16 mask = CHOOSE_MASKER(VERT_INDEX(_axis, _target), right)(       \
          ADJUST_AXIS_VAL(_axis));                                             \
      if (!(row & mask)) {                                                     \
        ADD_PATHS(                                                             \
            VERT_INDEX(_axis, _target),                                        \
            _axis,                                                             \
            right,                                                             \
            AXIS_PATHS(NOT_AXIS(_axis)),                                       \
            AXIS_PATHS(_axis));                                                \
        escape = true;                                                         \
      }                                                                        \
    }                                                                          \
    if (escape) {                                                              \
      u16 stem =                                                               \
          TARGET_MASKER(_target)(ADJUST_AXIS_VAL(NOT_AXIS(_axis)), _target);   \
      into_row(AXIS_PATHS(_axis), stem, _axis);                                \
      AXIS_PATHS(NOT_AXIS(_axis))->_[0] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _target, 0)                   \
          & ROT_SIDE(TARGET_SIDE(_target), _axis)[NOT_AXIS(_axis)]._[0];       \
      AXIS_PATHS(NOT_AXIS(_axis))->_[1] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _target, 1)                   \
          & ROT_SIDE(TARGET_SIDE(_target), _axis)[NOT_AXIS(_axis)]._[1];       \
    }                                                                          \
  }

// Paired stem macro: handles both near-edge (1/9) and edge (0/10) targets
// with a single reach check. If the king can't reach the near target, it
// can't reach the edge either (same direction, more squares to cross).
// The edge landing square's reachability is handled implicitly: a piece
// blocking the stem at the edge position also appears on the edge branch
// row at branch_pos, causing exit_check to fail.
#define BOTH_SIDES_STEM_PAIRED(_axis, _near, _edge, _check_edge)               \
  if (CAN_REACH_POS(_near)) {                                                  \
    bool near_escape = false;                                                  \
    bool edge_escape = false;                                                  \
    {                                                                          \
      u16 row =                                                                \
          DIRTY_GET_ROW(VERT_INDEX(_axis, _near), AXIS_OCC(NOT_AXIS(_axis)));  \
      {                                                                        \
        const u16 mask = CHOOSE_MASKER(VERT_INDEX(_axis, _near), left)(        \
            ADJUST_AXIS_VAL(_axis));                                           \
        if (!(row & mask)) {                                                   \
          ADD_PATHS(                                                           \
              VERT_INDEX(_axis, _near),                                        \
              _axis,                                                           \
              left,                                                            \
              AXIS_PATHS(NOT_AXIS(_axis)),                                     \
              AXIS_PATHS(_axis));                                              \
          near_escape = true;                                                  \
        }                                                                      \
      }                                                                        \
      {                                                                        \
        const u16 mask = CHOOSE_MASKER(VERT_INDEX(_axis, _near), right)(       \
            ADJUST_AXIS_VAL(_axis));                                           \
        if (!(row & mask)) {                                                   \
          ADD_PATHS(                                                           \
              VERT_INDEX(_axis, _near),                                        \
              _axis,                                                           \
              right,                                                           \
              AXIS_PATHS(NOT_AXIS(_axis)),                                     \
              AXIS_PATHS(_axis));                                              \
          near_escape = true;                                                  \
        }                                                                      \
      }                                                                        \
    }                                                                          \
    if (_check_edge) {                                                         \
      u16 row =                                                                \
          DIRTY_GET_ROW(VERT_INDEX(_axis, _edge), AXIS_OCC(NOT_AXIS(_axis)));  \
      {                                                                        \
        const u16 mask = CHOOSE_MASKER(VERT_INDEX(_axis, _edge), left)(        \
            ADJUST_AXIS_VAL(_axis));                                           \
        if (!(row & mask)) {                                                   \
          ADD_PATHS(                                                           \
              VERT_INDEX(_axis, _edge),                                        \
              _axis,                                                           \
              left,                                                            \
              AXIS_PATHS(NOT_AXIS(_axis)),                                     \
              AXIS_PATHS(_axis));                                              \
          edge_escape = true;                                                  \
        }                                                                      \
      }                                                                        \
      {                                                                        \
        const u16 mask = CHOOSE_MASKER(VERT_INDEX(_axis, _edge), right)(       \
            ADJUST_AXIS_VAL(_axis));                                           \
        if (!(row & mask)) {                                                   \
          ADD_PATHS(                                                           \
              VERT_INDEX(_axis, _edge),                                        \
              _axis,                                                           \
              right,                                                           \
              AXIS_PATHS(NOT_AXIS(_axis)),                                     \
              AXIS_PATHS(_axis));                                              \
          edge_escape = true;                                                  \
        }                                                                      \
      }                                                                        \
    }                                                                          \
    if (near_escape) {                                                         \
      u16 stem =                                                               \
          TARGET_MASKER(_near)(ADJUST_AXIS_VAL(NOT_AXIS(_axis)), _near);       \
      into_row(AXIS_PATHS(_axis), stem, _axis);                                \
      AXIS_PATHS(NOT_AXIS(_axis))->_[0] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _near, 0)                     \
          & ROT_SIDE(TARGET_SIDE(_near), _axis)[NOT_AXIS(_axis)]._[0];         \
      AXIS_PATHS(NOT_AXIS(_axis))->_[1] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _near, 1)                     \
          & ROT_SIDE(TARGET_SIDE(_near), _axis)[NOT_AXIS(_axis)]._[1];         \
    }                                                                          \
    if (edge_escape) {                                                         \
      u16 stem =                                                               \
          TARGET_MASKER(_edge)(ADJUST_AXIS_VAL(NOT_AXIS(_axis)), _edge);       \
      into_row(AXIS_PATHS(_axis), stem, _axis);                                \
      AXIS_PATHS(NOT_AXIS(_axis))->_[0] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _edge, 0)                     \
          & ROT_SIDE(TARGET_SIDE(_edge), _axis)[NOT_AXIS(_axis)]._[0];         \
      AXIS_PATHS(NOT_AXIS(_axis))->_[1] |=                                     \
          STEM_BASE_HALF(ADJUST_AXIS_VAL(_axis), _edge, 1)                     \
          & ROT_SIDE(TARGET_SIDE(_edge), _axis)[NOT_AXIS(_axis)]._[1];         \
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

void corner_paths_2_2(
    const layer king,
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r) {

  if (LAYERS_OVERLAP(middle, king)) {
    {
      u16 row = dirty_get_row(occ, rank);
      u16 pos = 1 << file;
      BOTH_SIDES_STEM_GEN(rank, 1);
      BOTH_SIDES_STEM_GEN(rank, 9);
      BOTH_SIDES_STEM_GEN(rank, 0);
      BOTH_SIDES_STEM_GEN(rank, 10);
    }

    {
      u16 row = dirty_get_row(occ_r, file);
      u16 pos = 1 << (10 - rank);
      BOTH_SIDES_STEM_GEN(file, 1);
      BOTH_SIDES_STEM_GEN(file, 9);
      BOTH_SIDES_STEM_GEN(file, 0);
      BOTH_SIDES_STEM_GEN(file, 10);
    }
    return;
  }

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
    }
  }
}

// Compute escape path layers for the king. The king escapes by reaching a
// corner square. Since a king cardinally adjacent to a corner can never be
// prevented from entering it, corner-adjacent squares are also effective
// escapes. We call these "exit squares":
//
//   Let E = {0, 10}, A = {1, 9}, M = {2..8}, B = E ∪ A.
//   Exit squares: X = B² \ A²
//
// This function maps all 1- and 2-move paths from the king to an exit
// square. All exit squares lie on rows in B, so a 1-move escape is only
// possible when the king is already on a row in B — it slides along that
// row directly to an exit. A 2-move path consists of a stem (first move
// to an intermediate row in B) and a branch (second move along that row
// to an exit). Two mirrored paths to opposite exits share a stem but
// diverge on the branch.
//
// The function handles 13 cases organized as 1 middle + 3 edge types × 4
// rotations:
//
//   Middle (1):         file ∈ M, rank ∈ M
//   On-edge (4):        file ∈ E, rank ∈ M
//                       rank ∈ E, file ∈ M
//   Edge-adjacent (4):  file ∈ A, rank ∈ M
//                       rank ∈ A, file ∈ M
//   Inner-corner (4):   file ∈ A, rank ∈ A
//
// Exit paths per case:
//
//   Middle (0 one-move, 8 two-move):
//     King is not on any row in B, so no 1-move escapes. Must move to an
//     intermediate row in B, then branch to an exit. All 8 intermediate
//     rows checked.
//
//   On-edge (1 one-move, 3 two-move):
//     1-move: Slide along the king's edge row to an exit.
//     2-move: Move along the perpendicular axis to each of the other 3
//       rows in B, branch to exit.
//     No parallel-axis 2-move routes: reaching any intermediate row along
//       the king's edge row lands on an exit square, already covered by
//       the 1-move escape.
//
//   Edge-adjacent (1 one-move, 5 two-move):
//     1-move: Slide along the king's near-edge row to an exit.
//     2-move perpendicular: Move along the perpendicular axis to each of
//       the other 3 rows in B on that axis, branch to exit (3 routes).
//     2-move parallel: Move along the king's own row to each of the 2
//       near-edge rows in A on the other axis, branch to exit (2 routes).
//     No edge row 2-move routes on the parallel axis: reaching them along
//       the king's near-edge row lands on an exit square, already covered
//       by the 1-move escape.
//
//   Inner-corner (2 one-move, 2 two-move):
//     1-move: Slide along each of the king's two near-edge rows to an
//       exit (2 routes). Note: the king is by definition directly
//       adjacent to the two exit squares on its near edges, so we only
//       need to check that those exit squares themselves are empty, not
//       that a path to them is clear.
//     2-move: Move along each row to the opposite near-edge row on the
//       other axis, branch to exit (2 routes).
//     No edge row 2-move routes: reaching any edge row along the king's
//       near-edge rows lands on an exit square, already covered by the
//       1-move escapes.
// restrict on paths/paths_r: without it, the compiler assumes writes through
// these pointers may alias the global arrays (above_n, below_n, etc.), forcing
// redundant reloads after every |= write. With restrict, the compiler knows
// these pointers don't alias anything else and can keep array values in
// registers across writes. This alone is worth ~2x on the hot path.
void corner_paths_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *restrict paths,
    layer *restrict paths_r) {

  // on file 0 or 10 we should only do the file methods
  if (file == 0) {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;

    BOTH_SIDES_INC(0, rank, occ_r, paths_r, paths);
    BOTH_SIDES_INC(1, rank, occ_r, paths_r, paths);
    BOTH_SIDES_STEM_PAIRED(rank, 9, 10, true);

    CLEAR_INDEX_PTR(paths, rank * 11 + file);
    CLEAR_INDEX_PTR(paths_r, rotate_right[rank * 11 + file]);
    return;
  }

  if (file == 10) {
    u16 row = dirty_get_row(occ, rank);
    u16 pos = 1 << file;

    BOTH_SIDES_STEM_PAIRED(rank, 1, 0, true);
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
    BOTH_SIDES_STEM_PAIRED(file, 1, 0, true);
    return;
  }

  if (rank == 10) {
    u16 row = dirty_get_row(occ_r, file);
    u16 pos = 1 << (10 - rank);
    BOTH_SIDES_INC(9, file, occ, paths, paths_r);
    BOTH_SIDES(10, file, occ, paths, paths_r);
    BOTH_SIDES_STEM_PAIRED(file, 9, 10, true);
    return;
  }

  u16 rank_occ = dirty_get_row(occ, rank);
  u16 file_occ = dirty_get_row(occ_r, file);

  if (file == 1) {
    if (rank == 1) {
      // IC_11
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES(1, rank, occ_r, paths_r, paths);
        BOTH_SIDES_STEM_PAIRED(rank, 9, 10, false);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES(1, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_PAIRED(file, 1, 0, false);
      }
    } else if (rank == 9) {
      // IC_91
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES(1, rank, occ_r, paths_r, paths);
        BOTH_SIDES_STEM_PAIRED(rank, 9, 10, false);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES(9, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_PAIRED(file, 9, 10, false);
      }
    } else {
      // ADJ_F1
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES(1, rank, occ_r, paths_r, paths);
        BOTH_SIDES_INC(0, rank, occ_r, paths_r, paths);
        BOTH_SIDES_STEM_PAIRED(rank, 9, 10, true);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES_STEM_PAIRED(file, 1, 0, false);
        BOTH_SIDES_STEM_PAIRED(file, 9, 10, false);
      }
    }
  } else if (file == 9) {
    if (rank == 1) {
      // IC_19
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES_STEM_PAIRED(rank, 1, 0, false);
        BOTH_SIDES(9, rank, occ_r, paths_r, paths);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES(1, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_PAIRED(file, 1, 0, false);
      }
    } else if (rank == 9) {
      // IC_99
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES_STEM_PAIRED(rank, 1, 0, false);
        BOTH_SIDES(9, rank, occ_r, paths_r, paths);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES(9, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_PAIRED(file, 9, 10, false);
      }
    } else {
      // ADJ_F9
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES_STEM_PAIRED(rank, 1, 0, true);
        BOTH_SIDES(9, rank, occ_r, paths_r, paths);
        BOTH_SIDES_INC(10, rank, occ_r, paths_r, paths);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES_STEM_PAIRED(file, 1, 0, false);
        BOTH_SIDES_STEM_PAIRED(file, 9, 10, false);
      }
    }
  } else {
    if (rank == 1) {
      // ADJ_R1
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES_STEM_PAIRED(rank, 1, 0, false);
        BOTH_SIDES_STEM_PAIRED(rank, 9, 10, false);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES(1, file, occ, paths, paths_r);
        BOTH_SIDES_INC(0, file, occ, paths, paths_r);
        BOTH_SIDES_STEM_PAIRED(file, 1, 0, true);
      }
    } else if (rank == 9) {
      // ADJ_R9
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES_STEM_PAIRED(rank, 1, 0, false);
        BOTH_SIDES_STEM_PAIRED(rank, 9, 10, false);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES_STEM_PAIRED(file, 9, 10, true);
        BOTH_SIDES(9, file, occ, paths, paths_r);
        BOTH_SIDES_INC(10, file, occ, paths, paths_r);
      }
    } else {
      // MIDDLE
      {
        u16 row = rank_occ;
        u16 pos = 1 << file;
        BOTH_SIDES_STEM_PAIRED(rank, 1, 0, true);
        BOTH_SIDES_STEM_PAIRED(rank, 9, 10, true);
      }
      {
        u16 row = file_occ;
        u16 pos = 1 << (10 - rank);
        BOTH_SIDES_STEM_PAIRED(file, 1, 0, true);
        BOTH_SIDES_STEM_PAIRED(file, 9, 10, true);
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Branchless into_row: avoids the 11-case switch in into_row() by using
// lookup tables for sub-layer index and shift amount. Row 5 spans both
// sub-layers, so we write to both (the shift naturally zeros out the
// irrelevant bits for rows != 5).
static const u8 row_sub_layer[11] = {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1};
static const u8 row_shift[11] = {0, 11, 22, 33, 44, 55, 2, 13, 24, 35, 46};
// For row 5: overflow bits 9-10 go to _[1] via row >> 9.
// For all other rows: row >> 63 == 0 (row is 11 bits), making the write a
// no-op.
static const u8 row_overflow_shift[11] =
    {63, 63, 63, 63, 63, 9, 63, 63, 63, 63, 63};
// LEGAL_EDGE_MASK for rows 0 and 10 (clear corner positions); identity for
// others.
static const u16 edge_row_mask[11] = {
    0x3fe,
    0x7ff,
    0x7ff,
    0x7ff,
    0x7ff,
    0x7ff,
    0x7ff,
    0x7ff,
    0x7ff,
    0x7ff,
    0x3fe};

// For reading row 5: bits 0-8 come from _[0] >> 55, bits 9-10 from _[1] << 9.
// For all other rows the secondary read produces 0 (shift of 63 on a u16 cast).
static const u8 row_overflow_read_shift[11] =
    {63, 63, 63, 63, 63, 9, 63, 63, 63, 63, 63};

static inline u16 __attribute__((always_inline))
dirty_get_row_branchless(layer l, int n) {
  u16 row = (u16)(l._[row_sub_layer[n]] >> row_shift[n]);
  row |= (u16)((u64)l._[1 - row_sub_layer[n]] << row_overflow_read_shift[n]);
  return row;
}

static inline void __attribute__((always_inline))
into_row_branchless(layer *l, u16 row, int n) {
  row &= edge_row_mask[n];
  l->_[row_sub_layer[n]] |= (u64)row << row_shift[n];
  l->_[1 - row_sub_layer[n]] |= (u64)row >> row_overflow_shift[n];
}

// Simplified row write for exit rows (0, 1, 9, 10) where row 5 straddling
// is impossible. With a compile-time constant row_idx, this folds to a
// single shift+or — identical to the INTO_ROW_N macros.
static inline void __attribute__((always_inline))
into_row_exit(layer *l, u16 row, int row_idx) {
  int bit_offset = row_idx * 11;
  l->_[bit_offset >> 6] |= (u64)row << (bit_offset & 63);
}

// ---------------------------------------------------------------------------
// New inline primitives for corner_paths_2_new

// Mask of bits [right, left] inclusive.
static inline u16 range_mask(int left, int right) {
  return (2 << left) - (1 << right);
}

// Are all bits in [right, left] clear in row_occ?
static inline bool range_clear(u16 row_occ, int left, int right) {
  return !(row_occ & range_mask(left, right));
}

// Can the king slide rightward (toward 0) from king_pos to target_pos?
// target_pos < king_pos.
// Uses a borrow trick (4 ops) that is faster than range_clear (5 ops)
// but requires king_pos to be clear in row_occ. This holds because
// row_occ is black | white, excluding the king. Not usable in exit_check
// where the checked position (a landing square) may be occupied.
static inline bool can_reach_right(u16 row_occ, int king_pos, int target_pos) {
  u16 king_bit = 1 << king_pos;
  u16 target_bit = 1 << target_pos;
  // Subtracting target_bit borrows upward through clear bits until hitting
  // a blocker. If no blocker exists between target and king, the borrow
  // flips king_bit on; otherwise king_bit stays off.
  return king_bit & (row_occ - target_bit);
}

// Can the king slide leftward (toward 10) from king_pos to target_pos?
// target_pos > king_pos.
static inline bool can_reach_left(u16 row_occ, int king_pos, int target_pos) {
  // Mask of bits from king_pos+1 to target_pos inclusive.
  // True iff none of those bits are set in row_occ.
  return range_clear(row_occ, target_pos, king_pos + 1);
}

// Check both directions along a row from pos for clear paths to an exit.
// Inclusive of pos: on a branch row, if the landing square is occupied
// the check fails naturally. On the king's own row, pos is the king's
// position which is clear in row_occ (king excluded from occ).
//
//   row_occ         - occupancy of the row being checked
//   pos             - starting position on the row (king's coordinate on this
//                     axis)
//   row_idx         - index of this row, for into_row
//   col_idx         - index into legal_file_masks for perpendicular mark.
//                     File-axis: col_idx = row_idx (identity).
//                     Rank-axis: col_idx = 10 - row_idx (rotated mapping).
//   perp_idx        - king's coordinate on the perpendicular axis, indexes
//                     into left_perp_arr / right_perp_arr
//   par             - parallel path layer: receives the row mask of traversed
//                     cells
//   perp            - perpendicular path layer: receives column masks for
//                     exit files
//   left_perp_arr   - half-board array for leftward exits
//   right_perp_arr  - half-board array for rightward exits
//
// Returns true if at least one direction found an exit.
static inline bool __attribute__((always_inline)) exit_check(
    u16 row_occ,
    int pos,
    int row_idx,
    int col_idx,
    int perp_idx,
    layer *par,
    layer *perp,
    const layer *left_perp_arr,
    const layer *right_perp_arr) {
  bool found = false;
  int left_end = (row_idx == 0 || row_idx == 10) ? 9 : 10;
  int right_end = (row_idx == 0 || row_idx == 10) ? 1 : 0;

  // Leftward: [pos, left_end]
  u16 left_mask = range_mask(left_end, pos);
  if (!(row_occ & left_mask)) {
    into_row_exit(par, left_mask, row_idx);
    perp->_[0] |= legal_file_masks[col_idx]._[0] & left_perp_arr[perp_idx]._[0];
    perp->_[1] |= legal_file_masks[col_idx]._[1] & left_perp_arr[perp_idx]._[1];
    found = true;
  }

  // Rightward: [right_end, pos]
  u16 right_mask = range_mask(pos, right_end);
  if (!(row_occ & right_mask)) {
    into_row_exit(par, right_mask, row_idx);
    perp->_[0] |=
        legal_file_masks[col_idx]._[0] & right_perp_arr[perp_idx]._[0];
    perp->_[1] |=
        legal_file_masks[col_idx]._[1] & right_perp_arr[perp_idx]._[1];
    found = true;
  }

  return found;
}

// Identical to exit_check except the range masks exclude the king's
// position (pos+1/pos-1 instead of pos). Used for the king's own row
// where the king's bit is clear in row_occ and must not appear in the
// path mask.
static inline bool __attribute__((always_inline)) exit_check_exc(
    u16 row_occ,
    int pos,
    int row_idx,
    int col_idx,
    int perp_idx,
    layer *par,
    layer *perp,
    const layer *left_perp_arr,
    const layer *right_perp_arr) {
  bool found = false;
  int left_end = (row_idx == 0 || row_idx == 10) ? 9 : 10;
  int right_end = (row_idx == 0 || row_idx == 10) ? 1 : 0;

  // Leftward: (pos, left_end] — excludes king
  u16 left_mask = range_mask(left_end, pos + 1);
  if (!(row_occ & left_mask)) {
    into_row_exit(par, left_mask, row_idx);
    perp->_[0] |= legal_file_masks[col_idx]._[0] & left_perp_arr[perp_idx]._[0];
    perp->_[1] |= legal_file_masks[col_idx]._[1] & left_perp_arr[perp_idx]._[1];
    found = true;
  }

  // Rightward: [right_end, pos) — excludes king
  u16 right_mask = range_mask(pos - 1, right_end);
  if (!(row_occ & right_mask)) {
    into_row_exit(par, right_mask, row_idx);
    perp->_[0] |=
        legal_file_masks[col_idx]._[0] & right_perp_arr[perp_idx]._[0];
    perp->_[1] |=
        legal_file_masks[col_idx]._[1] & right_perp_arr[perp_idx]._[1];
    found = true;
  }

  return found;
}

enum escape_dir { DIR_RIGHT, DIR_LEFT };

// Axis context for stem_and_branch and exit_check: captures all values
// that depend on which axis the king stems along (rank vs file).
//
// "Below" and "above" are relative to the stem axis, defined as toward
// decreasing (pos 0) and increasing (pos 10) stem position respectively.
// For rank-axis stems, stem position = file, so below = toward file 0
// and below_masks = below_n. For file-axis stems, stem position =
// rot_row_pos = 10 - rank, so below = toward rot_row_pos 0 = toward
// rank 10, and below_masks = above_n.
typedef struct {
  u16 stem_occ;     // occupancy of king's row on this axis
  int stem_pos;     // king's position on stem row
  layer branch_src; // layer to extract branch rows from (occ or occ_r)
  int branch_pos;   // king's coordinate on the branch axis
  bool col_flip;    // true for file-axis stem (col_idx = 10 - row_idx)
  int perp_idx;     // king's perpendicular coordinate
  layer *par; // output layer where branch rows are rows (paths_r for rank-axis,
              // paths for file-axis)
  layer *perp; // output layer where branch rows are columns (paths for
               // rank-axis, paths_r for file-axis)
  const layer *below_masks; // row-range masks below the stem axis (exclusive)
  const layer *above_masks; // row-range masks above the stem axis (exclusive)
  const layer
      *below_inc_masks; // row-range masks below the stem axis (inclusive)
  const layer
      *above_inc_masks; // row-range masks above the stem axis (inclusive)
  int stem_row_idx;     // row index for writing stem cells
  int perp_row_idx;     // king's row index in the perpendicular layer
                        // (file for rank-axis, rank for file-axis);
                        // indexes into below/above masks
} axis_ctx;

// col_idx helpers: resolve at compile time via token pasting.
// true/false expand to 1/0 in C, so we define COL_IDX_0 and COL_IDX_1.
#define COL_IDX_0(row_idx) (row_idx)
#define COL_IDX_1(row_idx) (10 - (row_idx))
#define COL_IDX(flip, row_idx) COL_IDX_##flip(row_idx)

// Inclusive exit_check using axis context (for adjacent rows).
// flip must be a literal true/false matching ctx->col_flip.
#define exit_check_ctx(row_occ, row_idx, flip, ctx)                            \
  exit_check(                                                                  \
      row_occ,                                                                 \
      (ctx)->branch_pos,                                                       \
      row_idx,                                                                 \
      COL_IDX(flip, row_idx),                                                  \
      (ctx)->perp_idx,                                                         \
      (ctx)->par,                                                              \
      (ctx)->perp,                                                             \
      (ctx)->below_inc_masks,                                                  \
      (ctx)->above_inc_masks)

// Exclusive exit_check using axis context (for king's own row).
// flip must be a literal true/false matching ctx->col_flip.
#define exit_check_exc_ctx(row_occ, row_idx, flip, ctx)                        \
  exit_check_exc(                                                              \
      row_occ,                                                                 \
      (ctx)->branch_pos,                                                       \
      row_idx,                                                                 \
      COL_IDX(flip, row_idx),                                                  \
      (ctx)->perp_idx,                                                         \
      (ctx)->par,                                                              \
      (ctx)->perp,                                                             \
      (ctx)->below_masks,                                                      \
      (ctx)->above_masks)

// 2-move escape: check if the king can reach the near-edge row along its
// own row (stem), then find exits along the near-edge and edge rows (branch).
// dir and check_edge must be compile-time constants — the compiler eliminates
// dead branches.
//
// The reach check only verifies the king can reach the near-edge (position
// 1 or 9). Reachability to the edge (position 0 or 10) is handled implicitly:
// exit_check's range is inclusive of branch_pos, so a piece at the edge
// landing square shows up in edge_occ and causes the check to fail.
static inline void __attribute__((always_inline))
stem_and_branch(enum escape_dir dir, bool check_edge, const axis_ctx *ctx) {
  int stem_target = (dir == DIR_RIGHT) ? 1 : 9;
  int edge_target = (dir == DIR_RIGHT) ? 0 : 10;

  bool reached =
      (dir == DIR_RIGHT)
          ? can_reach_right(ctx->stem_occ, ctx->stem_pos, stem_target)
          : can_reach_left(ctx->stem_occ, ctx->stem_pos, stem_target);
  if (!reached)
    return;

  // dir operates in stem coordinates where "right" = toward position 0.
  // For rank-axis stems, stem position 0 is file 0, and the branch rows
  // we check in occ_r are the low-numbered ones (files 1, 0). For
  // file-axis stems, stem position 0 is rank 10 (because rot_row_pos = 10 -
  // rank), and the branch rows we check in occ are the high-numbered
  // ones (ranks 9, 10) — so the mapping inverts.
  // toward_low_rows: true when the target branch rows are 1/0, false
  // when they are 9/10.
  bool toward_low_rows = (dir == DIR_RIGHT) != ctx->col_flip;
  int near_row_n = toward_low_rows ? 1 : 9;
  int edge_row_n = toward_low_rows ? 0 : 10;
  int near_col_idx = ctx->col_flip ? 10 - near_row_n : near_row_n;
  int edge_col_idx = ctx->col_flip ? 10 - edge_row_n : edge_row_n;
  // When writing the stem to the escape path layer in which it has a
  // column orientation, we compose a full-board column mask with this
  // mask, which contains only the rows between the king and the board
  // edge where the exit is happening.
  layer stem_perp_half = (dir == DIR_RIGHT)
                             ? ctx->below_masks[ctx->perp_row_idx]
                             : ctx->above_masks[ctx->perp_row_idx];

  // Extract branch row occupancy only after confirming the stem is reachable.
  u16 near_occ = dirty_get_row_branchless(ctx->branch_src, near_row_n);
  u16 edge_occ =
      check_edge ? dirty_get_row_branchless(ctx->branch_src, edge_row_n) : 0;

  // Use inclusive exit_check: the landing square (branch_pos) must appear in
  // par so that paths and paths_r stay consistent (the stem already marks it
  // in perp). Exclusive perp arrays are passed through, so the king's row is
  // not marked in the perpendicular layer.
  bool found_near = exit_check(
      near_occ,
      ctx->branch_pos,
      near_row_n,
      near_col_idx,
      ctx->perp_idx,
      ctx->par,
      ctx->perp,
      ctx->below_masks,
      ctx->above_masks);

  bool found_edge = false;
  if (check_edge) {
    found_edge = exit_check(
        edge_occ,
        ctx->branch_pos,
        edge_row_n,
        edge_col_idx,
        ctx->perp_idx,
        ctx->par,
        ctx->perp,
        ctx->below_masks,
        ctx->above_masks);
  }

  // Stem cells go to perp (row representation) because the stem runs along
  // the king's row, which is perpendicular to the branch axis.
  // The stem also goes to par (column representation) via the perpendicular
  // stem component: file_mask_adjacent (for near) or legal_file_masks (for
  // edge) masked by the pre-looked-up half-board layer.
  if (found_near) {
    u16 stem_mask = (dir == DIR_RIGHT)
                        ? range_mask(ctx->stem_pos - 1, stem_target)
                        : range_mask(stem_target, ctx->stem_pos + 1);
    into_row(ctx->perp, stem_mask, ctx->stem_row_idx);
    ctx->par->_[0] |=
        file_mask_adjacent[ctx->branch_pos]._[0] & stem_perp_half._[0];
    ctx->par->_[1] |=
        file_mask_adjacent[ctx->branch_pos]._[1] & stem_perp_half._[1];
  }
  if (found_edge) {
    u16 stem_mask = (dir == DIR_RIGHT)
                        ? range_mask(ctx->stem_pos - 1, edge_target)
                        : range_mask(edge_target, ctx->stem_pos + 1);
    into_row(ctx->perp, stem_mask, ctx->stem_row_idx);
    ctx->par->_[0] |=
        legal_file_masks[ctx->branch_pos]._[0] & stem_perp_half._[0];
    ctx->par->_[1] |=
        legal_file_masks[ctx->branch_pos]._[1] & stem_perp_half._[1];
  }
}

// ---------------------------------------------------------------------------
// Zone dispatch for corner_paths_2_new

enum zone {
  Z_MIDDLE = 0,
  Z_ON_F0,
  Z_ON_F10,
  Z_ON_R0,
  Z_ON_R10,
  Z_ADJ_F1,
  Z_ADJ_F9,
  Z_ADJ_R1,
  Z_ADJ_R9,
  Z_IC_11,
  Z_IC_19,
  Z_IC_91,
  Z_IC_99,
};

// clang-format off
static const u8 __attribute__((unused)) zone_table[121] = {
  // rank 0: corners → Z_MIDDLE (king can't be there), rest → Z_ON_R0
  Z_MIDDLE, Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_ON_R0,  Z_MIDDLE,
  // rank 1
  Z_ON_F0,  Z_IC_11,  Z_ADJ_R1, Z_ADJ_R1, Z_ADJ_R1, Z_ADJ_R1, Z_ADJ_R1, Z_ADJ_R1, Z_ADJ_R1, Z_IC_19,  Z_ON_F10,
  // ranks 2-8
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  Z_ON_F0,  Z_ADJ_F1, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_MIDDLE, Z_ADJ_F9, Z_ON_F10,
  // rank 9
  Z_ON_F0,  Z_IC_91,  Z_ADJ_R9, Z_ADJ_R9, Z_ADJ_R9, Z_ADJ_R9, Z_ADJ_R9, Z_ADJ_R9, Z_ADJ_R9, Z_IC_99,  Z_ON_F10,
  // rank 10: corners → Z_MIDDLE, rest → Z_ON_R10
  Z_MIDDLE, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_ON_R10, Z_MIDDLE,
};
// clang-format on

void corner_paths_2_new(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    const int king_pos,
    layer *restrict paths,
    layer *restrict paths_r) {
  (void)king_pos;

  int rot_row_pos = 10 - rank; // king's index within a row in occ_r

  // Edge cases: king on file 0/10 or rank 0/10.
  // Only need one row extraction (stem axis).
  if (file == 0) {
    u16 rank_occ = dirty_get_row(occ, rank);
    axis_ctx rc = {
        .stem_occ = rank_occ,
        .stem_pos = file,
        .branch_src = occ_r,
        .branch_pos = rot_row_pos,
        .col_flip = false,
        .perp_idx = rank,
        .par = paths_r,
        .perp = paths,
        .below_masks = below_n,
        .above_masks = above_n,
        .below_inc_masks = below_n_inc,
        .above_inc_masks = above_n_inc,
        .stem_row_idx = rank,
        .perp_row_idx = file,
    };
    exit_check_exc_ctx(DIRTY_GET_ROW(0, occ_r), 0, false, &rc);
    exit_check_ctx(DIRTY_GET_ROW(1, occ_r), 1, false, &rc);
    stem_and_branch(DIR_LEFT, true, &rc);
  } else if (file == 10) {
    u16 rank_occ = dirty_get_row(occ, rank);
    axis_ctx rc = {
        .stem_occ = rank_occ,
        .stem_pos = file,
        .branch_src = occ_r,
        .branch_pos = rot_row_pos,
        .col_flip = false,
        .perp_idx = rank,
        .par = paths_r,
        .perp = paths,
        .below_masks = below_n,
        .above_masks = above_n,
        .below_inc_masks = below_n_inc,
        .above_inc_masks = above_n_inc,
        .stem_row_idx = rank,
        .perp_row_idx = file,
    };
    stem_and_branch(DIR_RIGHT, true, &rc);
    exit_check_ctx(DIRTY_GET_ROW(9, occ_r), 9, false, &rc);
    exit_check_exc_ctx(DIRTY_GET_ROW(10, occ_r), 10, false, &rc);
  } else if (rank == 0) {
    u16 file_occ = dirty_get_row(occ_r, file);
    axis_ctx fc = {
        .stem_occ = file_occ,
        .stem_pos = rot_row_pos,
        .branch_src = occ,
        .branch_pos = file,
        .col_flip = true,
        .perp_idx = file,
        .par = paths,
        .perp = paths_r,
        .below_masks = above_n,
        .above_masks = below_n,
        .below_inc_masks = above_n_inc,
        .above_inc_masks = below_n_inc,
        .stem_row_idx = file,
        .perp_row_idx = rank,
    };
    exit_check_exc_ctx(DIRTY_GET_ROW(0, occ), 0, true, &fc);
    exit_check_ctx(DIRTY_GET_ROW(1, occ), 1, true, &fc);
    stem_and_branch(DIR_RIGHT, true, &fc);
  } else if (rank == 10) {
    u16 file_occ = dirty_get_row(occ_r, file);
    axis_ctx fc = {
        .stem_occ = file_occ,
        .stem_pos = rot_row_pos,
        .branch_src = occ,
        .branch_pos = file,
        .col_flip = true,
        .perp_idx = file,
        .par = paths,
        .perp = paths_r,
        .below_masks = above_n,
        .above_masks = below_n,
        .below_inc_masks = above_n_inc,
        .above_inc_masks = below_n_inc,
        .stem_row_idx = file,
        .perp_row_idx = rank,
    };
    stem_and_branch(DIR_LEFT, true, &fc);
    exit_check_ctx(DIRTY_GET_ROW(9, occ), 9, true, &fc);
    exit_check_exc_ctx(DIRTY_GET_ROW(10, occ), 10, true, &fc);
  } else {
    // Interior cases: need both rows.
    u16 rank_occ = dirty_get_row(occ, rank);
    u16 file_occ = dirty_get_row(occ_r, file);
    axis_ctx rc = {
        .stem_occ = rank_occ,
        .stem_pos = file,
        .branch_src = occ_r,
        .branch_pos = rot_row_pos,
        .col_flip = false,
        .perp_idx = rank,
        .par = paths_r,
        .perp = paths,
        .below_masks = below_n,
        .above_masks = above_n,
        .below_inc_masks = below_n_inc,
        .above_inc_masks = above_n_inc,
        .stem_row_idx = rank,
        .perp_row_idx = file,
    };
    axis_ctx fc = {
        .stem_occ = file_occ,
        .stem_pos = rot_row_pos,
        .branch_src = occ,
        .branch_pos = file,
        .col_flip = true,
        .perp_idx = file,
        .par = paths,
        .perp = paths_r,
        .below_masks = above_n,
        .above_masks = below_n,
        .below_inc_masks = above_n_inc,
        .above_inc_masks = below_n_inc,
        .stem_row_idx = file,
        .perp_row_idx = rank,
    };

    if (file == 1) {
      if (rank == 1) {
        // Z_IC_11: king's own file and rank, both exclusive
        exit_check_exc_ctx(DIRTY_GET_ROW(1, occ_r), 1, false, &rc);
        exit_check_exc_ctx(DIRTY_GET_ROW(1, occ), 1, true, &fc);
        stem_and_branch(DIR_LEFT, false, &rc);
        stem_and_branch(DIR_RIGHT, false, &fc);
      } else if (rank == 9) {
        // Z_IC_91: king's own file and rank, both exclusive
        exit_check_exc_ctx(DIRTY_GET_ROW(1, occ_r), 1, false, &rc);
        exit_check_exc_ctx(DIRTY_GET_ROW(9, occ), 9, true, &fc);
        stem_and_branch(DIR_LEFT, false, &rc);
        stem_and_branch(DIR_LEFT, false, &fc);
      } else {
        // Z_ADJ_F1: king's file exclusive, adjacent file 0 inclusive
        exit_check_exc_ctx(DIRTY_GET_ROW(1, occ_r), 1, false, &rc);
        exit_check_ctx(DIRTY_GET_ROW(0, occ_r), 0, false, &rc);
        stem_and_branch(DIR_LEFT, true, &rc);
        stem_and_branch(DIR_RIGHT, false, &fc);
        stem_and_branch(DIR_LEFT, false, &fc);
      }
    } else if (file == 9) {
      if (rank == 1) {
        // Z_IC_19: king's own file and rank, both exclusive
        exit_check_exc_ctx(DIRTY_GET_ROW(9, occ_r), 9, false, &rc);
        exit_check_exc_ctx(DIRTY_GET_ROW(1, occ), 1, true, &fc);
        stem_and_branch(DIR_RIGHT, false, &rc);
        stem_and_branch(DIR_RIGHT, false, &fc);
      } else if (rank == 9) {
        // Z_IC_99: king's own file and rank, both exclusive
        exit_check_exc_ctx(DIRTY_GET_ROW(9, occ_r), 9, false, &rc);
        exit_check_exc_ctx(DIRTY_GET_ROW(9, occ), 9, true, &fc);
        stem_and_branch(DIR_RIGHT, false, &rc);
        stem_and_branch(DIR_LEFT, false, &fc);
      } else {
        // Z_ADJ_F9: king's file exclusive, adjacent file 10 inclusive
        stem_and_branch(DIR_RIGHT, true, &rc);
        exit_check_exc_ctx(DIRTY_GET_ROW(9, occ_r), 9, false, &rc);
        exit_check_ctx(DIRTY_GET_ROW(10, occ_r), 10, false, &rc);
        stem_and_branch(DIR_RIGHT, false, &fc);
        stem_and_branch(DIR_LEFT, false, &fc);
      }
    } else {
      if (rank == 1) {
        // Z_ADJ_R1: king's rank exclusive, adjacent rank 0 inclusive
        exit_check_exc_ctx(DIRTY_GET_ROW(1, occ), 1, true, &fc);
        exit_check_ctx(DIRTY_GET_ROW(0, occ), 0, true, &fc);
        stem_and_branch(DIR_RIGHT, true, &fc);
        stem_and_branch(DIR_RIGHT, false, &rc);
        stem_and_branch(DIR_LEFT, false, &rc);
      } else if (rank == 9) {
        // Z_ADJ_R9: king's rank exclusive, adjacent rank 10 inclusive
        stem_and_branch(DIR_LEFT, true, &fc);
        exit_check_exc_ctx(DIRTY_GET_ROW(9, occ), 9, true, &fc);
        exit_check_ctx(DIRTY_GET_ROW(10, occ), 10, true, &fc);
        stem_and_branch(DIR_RIGHT, false, &rc);
        stem_and_branch(DIR_LEFT, false, &rc);
      } else {
        // Z_MIDDLE: all S&B, all exclusive
        stem_and_branch(DIR_RIGHT, true, &rc);
        stem_and_branch(DIR_LEFT, true, &rc);
        stem_and_branch(DIR_RIGHT, true, &fc);
        stem_and_branch(DIR_LEFT, true, &fc);
      }
    }
  }
}

/* _axis is a token named either rank or file, which determines the
axis _i should be interpreted as. _i is an int constant. */
#define GET_POS(_axis, _i)                                                     \
  (GET_AXIS_VAL(_axis, _axis)                                                  \
   + GET_AXIS_VAL(NOT_AXIS(_axis), ADJUST_AXIS_VAL_I(NOT_AXIS(_axis), _i)))
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
      const u16 left_mask = MASK_LEFTWARD(ADJUST_AXIS_VAL(_variable));         \
      const u16 right_mask = MASK_RIGHTWARD(ADJUST_AXIS_VAL(_variable));       \
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
      int exits = 0;                                                           \
      if (!(row & left_mask)) {                                                \
        exits++;                                                               \
      }                                                                        \
      if (!(row & right_mask)) {                                               \
        exits++;                                                               \
      }                                                                        \
      if (exits == 1) {                                                        \
        ADD_MOVE(GET_POS(_axis, VERT_INDEX(_axis, _target)))                   \
      }                                                                        \
      if (exits == 2) {                                                        \
        return true;                                                           \
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
      int exits = 0;                                                           \
      if (!(row & left_mask)) {                                                \
        exits++;                                                               \
      }                                                                        \
      if (!(row & right_mask)) {                                               \
        exits++;                                                               \
      }                                                                        \
      if (exits == 1) {                                                        \
        ADD_MOVE(GET_POS(_axis, _target))                                      \
      }                                                                        \
      if (exits == 2) {                                                        \
        return true;                                                           \
      }                                                                        \
    }                                                                          \
  }

// TODO: consider removing the single move checks from this function and using
// it in conjuntion with the other function.
// IMPORTANT: occ must not include the king
// returns true when there exists a move which inevetably leads to escape,
// either a single move directly to a corner or a fork.
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
