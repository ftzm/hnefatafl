/* This is a WIP module providing generator-based move generation. It should
work, but hasn't been thoroughly tested, and the code isn't in a state I'm happy
with either.

I'd like to make the following changes:
- change the next-move function to not be in a while loop
- I'd like to remove all of the break statements; the cases should all fall
through to each other unless they return.
- we don't need to set the next state label on each case, we only need to set
the current state label before each return statetment.
- there's a lot of repetetive logic in each case branch, I'd like to factor that
out.
- the occ layers in the state struct can probably be generated when needed, not
ahead of time and stored in the struct.
*/

#include "move_generator.h"
#include "constants.h"
#include "layer.h"
#include "limits.h"
#include "macro_util.h"
#include "move.h"
#include "stdbool.h"
#include "x86intrin.h" // IWYU pragma: export

#define DROP_1_EAST_0 18410697675910412286ULL
#define DROP_1_EAST_1 144044784955154427ULL

#define HALF_MASK_0 LOWER_HALF_MASK
#define HALF_MASK_1 UPPER_HALF_MASK

/* Offset if the index is 1, otherwise do nothing */
#define OFFSET_0(_)
#define OFFSET_1(_n) (_n += 64)
#define OFFSET(_n, _i) OFFSET_##_i(_n)

#define ROTATE rotate_right
#define ROTATE_r rotate_left
#define ROTATE_DIR(_r) ROTATE##_r

// Shared macros for move generation and extraction

// Generate leftward destinations for a half-layer
#define GEN_LEFTWARD_DESTS(_i, _r, _gen)                                       \
  (_gen->targets##_r._[_i]                                                     \
   & (_gen->leftward_occ##_r._[_i] - (_gen->movers##_r._[_i] << 1))            \
   & DROP_1_EAST_##_i)

// Extract a leftward move from dests (non-rotated)
#define EXTRACT_LEFTWARD_MOVE(_i, _gen, _result)                               \
  do {                                                                         \
    u64 dest_bit = _blsi_u64(_gen->dests);                                     \
    u8 dest = _tzcnt_u64(dest_bit);                                            \
    OFFSET(dest, _i);                                                          \
    u8 orig =                                                                  \
        63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & _gen->leftward_occ._[_i]);     \
    OFFSET(orig, _i);                                                          \
    _result->orig = orig;                                                      \
    _result->dest = dest;                                                      \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Extract a leftward move from dests (rotated)
#define EXTRACT_LEFTWARD_MOVE_R(_i, _gen, _result)                             \
  do {                                                                         \
    u64 dest_bit = _blsi_u64(_gen->dests);                                     \
    u8 dest = _tzcnt_u64(dest_bit);                                            \
    OFFSET(dest, _i);                                                          \
    u8 orig =                                                                  \
        63 - _lzcnt_u64(_blsmsk_u64(dest_bit) & _gen->leftward_occ_r._[_i]);   \
    OFFSET(orig, _i);                                                          \
    u8 orig_r = rotate_left[orig];                                             \
    u8 dest_r = rotate_left[dest];                                             \
    _result->orig = orig_r;                                                    \
    _result->dest = dest_r;                                                    \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Generate leftward destinations for center row
#define GEN_LEFTWARD_CENTER_DESTS(_r, _gen)                                    \
  (GET_CENTER_ROW(_gen->targets##_r)                                           \
   & (_gen->center_occ##_r - (_gen->center_movers##_r << 1)))

// Extract a leftward center move (non-rotated)
#define EXTRACT_LEFTWARD_CENTER_MOVE(_gen, _result)                            \
  do {                                                                         \
    u16 dest_bit = _gen->dests & -_gen->dests;                                 \
    u8 dest = _tzcnt_u16(dest_bit);                                            \
    dest += 55;                                                                \
    u8 orig = 15 - __lzcnt16((dest_bit - 1) & _gen->center_occ);               \
    orig += 55;                                                                \
    _result->orig = orig;                                                      \
    _result->dest = dest;                                                      \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Extract a leftward center move (rotated)
#define EXTRACT_LEFTWARD_CENTER_MOVE_R(_gen, _result)                          \
  do {                                                                         \
    u16 dest_bit = _gen->dests & -_gen->dests;                                 \
    u8 dest = _tzcnt_u16(dest_bit);                                            \
    dest += 55;                                                                \
    u8 orig = 15 - __lzcnt16((dest_bit - 1) & _gen->center_occ_r);             \
    orig += 55;                                                                \
    u8 orig_r = rotate_left[orig];                                             \
    u8 dest_r = rotate_left[dest];                                             \
    _result->orig = orig_r;                                                    \
    _result->dest = dest_r;                                                    \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Initialize rightward move generation for a half-layer
#define INIT_RIGHTWARD_MOVES(_i, _r, _gen)                                     \
  do {                                                                         \
    u64 blockers = _gen->occ##_r._[_i] | file_mask_10._[_i];                   \
    u64 movers_ext = _pext_u64(_gen->movers##_r._[_i], blockers) >> 1;         \
    u64 movers_dep = _pdep_u64(movers_ext, (blockers << 1));                   \
    _gen->move_mask =                                                          \
        (_gen->movers##_r._[_i] - movers_dep) & _gen->targets##_r._[_i];       \
    _gen->origs = _gen->movers##_r._[_i]                                       \
                  & ~(_gen->movers##_r._[_i] - _gen->targets##_r._[_i]);       \
  } while (0)

// Initialize rightward move generation for a half-layer (king version)
#define INIT_RIGHTWARD_MOVES_KING(_i, _r, _gen)                                \
  do {                                                                         \
    u64 blockers = _gen->occ##_r._[_i] | file_mask_10._[_i];                   \
    u64 movers_ext = _pext_u64(_gen->movers##_r._[_i], 1 | blockers) >> 1;     \
    u64 movers_dep = _pdep_u64(movers_ext, 1 | (blockers << 1));               \
    _gen->move_mask = (_gen->movers##_r._[_i] - movers_dep)                    \
                      & HALF_MASK_##_i                                         \
                      & _gen->targets##_r._[_i];                               \
    _gen->origs = _gen->movers##_r._[_i]                                       \
                  & ~(_gen->movers##_r._[_i] - _gen->targets##_r._[_i]);       \
  } while (0)

// Get next origin for rightward moves and calculate its destinations
// (non-rotated)
#define GET_NEXT_RIGHTWARD_ORIGIN(_i, _gen)                                    \
  do {                                                                         \
    if (_gen->origs == 0)                                                      \
      break;                                                                   \
    _gen->current_orig_bit = _blsi_u64(_gen->origs);                           \
    _gen->current_orig = _tzcnt_u64(_gen->current_orig_bit);                   \
    OFFSET(_gen->current_orig, _i);                                            \
    _gen->origs -= _gen->current_orig_bit;                                     \
    _gen->dests = _gen->move_mask & (_gen->current_orig_bit - 1);              \
    _gen->move_mask &= 0 - _gen->current_orig_bit;                             \
  } while (0)

// Get next origin for rightward moves and calculate its destinations (rotated)
#define GET_NEXT_RIGHTWARD_ORIGIN_R(_i, _gen)                                  \
  do {                                                                         \
    if (_gen->origs == 0)                                                      \
      break;                                                                   \
    _gen->current_orig_bit = _blsi_u64(_gen->origs);                           \
    u8 rotated_orig = _tzcnt_u64(_gen->current_orig_bit);                      \
    OFFSET(rotated_orig, _i);                                                  \
    _gen->current_orig = rotate_left[rotated_orig];                            \
    _gen->origs -= _gen->current_orig_bit;                                     \
    _gen->dests = _gen->move_mask & (_gen->current_orig_bit - 1);              \
    _gen->move_mask &= 0 - _gen->current_orig_bit;                             \
  } while (0)

// Extract a rightward move (non-rotated)
#define EXTRACT_RIGHTWARD_MOVE(_i, _gen, _result)                              \
  do {                                                                         \
    u8 dest = _tzcnt_u64(_gen->dests);                                         \
    u64 dest_bit = (u64)1 << dest;                                             \
    OFFSET(dest, _i);                                                          \
    _result->orig = _gen->current_orig;                                        \
    _result->dest = dest;                                                      \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Extract a rightward move (rotated)
#define EXTRACT_RIGHTWARD_MOVE_R(_i, _gen, _result)                            \
  do {                                                                         \
    u8 rotated_dest = _tzcnt_u64(_gen->dests);                                 \
    u64 dest_bit = (u64)1 << rotated_dest;                                     \
    OFFSET(rotated_dest, _i);                                                  \
    u8 dest_r = rotate_left[rotated_dest];                                     \
    _result->orig = _gen->current_orig;                                        \
    _result->dest = dest_r;                                                    \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Initialize rightward center moves
#define INIT_RIGHTWARD_CENTER_MOVES(_r, _gen)                                  \
  (_gen->origs =                                                               \
       _gen->center_movers##_r                                                 \
       & ~(_gen->center_occ##_r - (GET_CENTER_ROW(_gen->targets##_r))))

// Get next center origin and calculate its destinations
#define GET_NEXT_RIGHTWARD_CENTER_ORIGIN(_r, _gen)                             \
  do {                                                                         \
    if (_gen->origs == 0)                                                      \
      break;                                                                   \
    u16 orig_bit = _gen->origs & -_gen->origs;                                 \
    _gen->current_orig = _tzcnt_u16(orig_bit);                                 \
    _gen->origs -= orig_bit;                                                   \
    _gen->current_orig += 55;                                                  \
    u16 below = orig_bit - 1;                                                  \
    u16 above_highest_occ_mask =                                               \
        (_gen->center_occ##_r & below)                                         \
            ? ((u16) - 1 << (16 - __lzcnt16(_gen->center_occ##_r & below)))    \
            : (u16) - 1;                                                       \
    _gen->dests =                                                              \
        GET_CENTER_ROW(_gen->targets##_r) & below & above_highest_occ_mask;    \
  } while (0)

// Extract a rightward center move (non-rotated)
#define EXTRACT_RIGHTWARD_CENTER_MOVE(_gen, _result)                           \
  do {                                                                         \
    u8 dest = _tzcnt_u16(_gen->dests);                                         \
    u16 dest_bit = (u16)1 << dest;                                             \
    dest += 55;                                                                \
    _result->orig = _gen->current_orig;                                        \
    _result->dest = dest;                                                      \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Extract a rightward center move (rotated)
#define EXTRACT_RIGHTWARD_CENTER_MOVE_R(_gen, _result)                         \
  do {                                                                         \
    u8 dest = _tzcnt_u16(_gen->dests);                                         \
    u16 dest_bit = (u16)1 << dest;                                             \
    dest += 55;                                                                \
    u8 orig_r = rotate_left[_gen->current_orig];                               \
    u8 dest_r = rotate_left[dest];                                             \
    _result->orig = orig_r;                                                    \
    _result->dest = dest_r;                                                    \
    _gen->dests -= dest_bit;                                                   \
  } while (0)

// Initialize the move generator
void init_move_generator(
    move_generator *gen,
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r) {

  gen->targets = targets;
  gen->targets_r = targets_r;
  gen->movers = movers;
  gen->movers_r = movers_r;
  gen->occ = occ;
  gen->occ_r = occ_r;

  // Calculate derived values
  gen->center_occ = GET_CENTER_ROW(occ);
  gen->center_occ_r = GET_CENTER_ROW(occ_r);
  gen->center_movers = GET_CENTER_ROW(movers);
  gen->center_movers_r = GET_CENTER_ROW(movers_r);
  gen->leftward_occ = LAYER_OR(occ, file_mask_0);
  gen->leftward_occ_r = LAYER_OR(occ_r, file_mask_0);
  gen->rightward_occ = LAYER_OR(occ, file_mask_10);
  gen->rightward_occ_r = LAYER_OR(occ_r, file_mask_10);

  // Mask movers as in original function
  gen->movers._[0] &= LOWER_HALF_MASK;
  gen->movers._[1] &= UPPER_HALF_MASK;
  gen->movers_r._[0] &= LOWER_HALF_MASK;
  gen->movers_r._[1] &= UPPER_HALF_MASK;

  // Initialize state
  gen->state = GEN_LEFTWARD_0;
  gen->dests = 0;
  gen->origs = 0;
  gen->current_orig_bit = 0;
  gen->current_orig = 0;
  gen->move_mask = 0;
}

// Initialize the king move generator
void init_move_generator_king(
    move_generator *gen,
    layer targets,
    layer targets_r,
    layer movers,
    layer movers_r,
    layer occ,
    layer occ_r) {

  gen->targets = targets;
  gen->targets_r = targets_r;
  gen->movers = movers;
  gen->movers_r = movers_r;
  gen->occ = occ;
  gen->occ_r = occ_r;

  // Calculate derived values
  gen->center_occ = GET_CENTER_ROW(occ);
  gen->center_occ_r = GET_CENTER_ROW(occ_r);
  gen->center_movers = GET_CENTER_ROW(movers);
  gen->center_movers_r = GET_CENTER_ROW(movers_r);
  gen->leftward_occ = LAYER_OR(occ, file_mask_0);
  gen->leftward_occ_r = LAYER_OR(occ_r, file_mask_0);
  gen->rightward_occ = LAYER_OR(occ, file_mask_10);
  gen->rightward_occ_r = LAYER_OR(occ_r, file_mask_10);

  // Mask movers as in original function
  gen->movers._[0] &= LOWER_HALF_MASK;
  gen->movers._[1] &= UPPER_HALF_MASK;
  gen->movers_r._[0] &= LOWER_HALF_MASK;
  gen->movers_r._[1] &= UPPER_HALF_MASK;

  // Initialize state for king generator
  gen->state = GEN_KING_LEFTWARD_0;
  gen->dests = 0;
  gen->origs = 0;
  gen->current_orig_bit = 0;
  gen->current_orig = 0;
  gen->move_mask = 0;
}

// Get the next move from the generator
// Returns true if a move was generated, false if no more moves
bool next_move(move_generator *gen, move *result) {
  while (gen->state < GEN_DONE) {
    switch (gen->state) {
    case GEN_LEFTWARD_0:
      gen->dests = GEN_LEFTWARD_DESTS(0, , gen);
      /* fallthrough */

    case GEN_LEFTWARD_0_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE(0, gen, result);
        gen->state = GEN_LEFTWARD_0_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_LEFTWARD_1:
      gen->dests = GEN_LEFTWARD_DESTS(1, , gen);
      /* fallthrough */

    case GEN_LEFTWARD_1_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE(1, gen, result);
        gen->state = GEN_LEFTWARD_1_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_LEFTWARD_CENTER:
      gen->dests = GEN_LEFTWARD_CENTER_DESTS(, gen);
      /* fallthrough */

    case GEN_LEFTWARD_CENTER_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_CENTER_MOVE(gen, result);
        gen->state = GEN_LEFTWARD_CENTER_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_LEFTWARD_0_R:
      gen->dests = GEN_LEFTWARD_DESTS(0, _r, gen);
      /* fallthrough */

    case GEN_LEFTWARD_0_R_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE_R(0, gen, result);
        gen->state = GEN_LEFTWARD_0_R_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_LEFTWARD_1_R:
      gen->dests = GEN_LEFTWARD_DESTS(1, _r, gen);
      /* fallthrough */

    case GEN_LEFTWARD_1_R_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE_R(1, gen, result);
        gen->state = GEN_LEFTWARD_1_R_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_LEFTWARD_CENTER_R:
      gen->dests = GEN_LEFTWARD_CENTER_DESTS(_r, gen);
      /* fallthrough */

    case GEN_LEFTWARD_CENTER_R_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_CENTER_MOVE_R(gen, result);
        gen->state = GEN_LEFTWARD_CENTER_R_EXTRACT;
        return true;
      }
      /* fallthrough */

    // Rightward moves
    case GEN_RIGHTWARD_0:
      INIT_RIGHTWARD_MOVES(0, , gen);
      gen->state = GEN_RIGHTWARD_0_EXTRACT;
      /* fallthrough */

    case GEN_RIGHTWARD_0_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN(0, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_RIGHTWARD_1;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE(0, gen, result);
        return true;
      }
      break;

    case GEN_RIGHTWARD_1:
      INIT_RIGHTWARD_MOVES(1, , gen);
      gen->state = GEN_RIGHTWARD_1_EXTRACT;
      /* fallthrough */

    case GEN_RIGHTWARD_1_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN(1, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_RIGHTWARD_CENTER;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE(1, gen, result);
        return true;
      }
      break;

    case GEN_RIGHTWARD_CENTER:
      INIT_RIGHTWARD_CENTER_MOVES(, gen);
      gen->state = GEN_RIGHTWARD_CENTER_EXTRACT;
      /* fallthrough */

    case GEN_RIGHTWARD_CENTER_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_CENTER_ORIGIN(, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_RIGHTWARD_0_R;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_CENTER_MOVE(gen, result);
        return true;
      }
      break;

    case GEN_RIGHTWARD_0_R:
      INIT_RIGHTWARD_MOVES(0, _r, gen);
      gen->state = GEN_RIGHTWARD_0_R_EXTRACT;
      /* fallthrough */

    case GEN_RIGHTWARD_0_R_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN_R(0, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_RIGHTWARD_1_R;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE_R(0, gen, result);
        return true;
      }
      break;

    case GEN_RIGHTWARD_1_R:
      INIT_RIGHTWARD_MOVES(1, _r, gen);
      gen->state = GEN_RIGHTWARD_1_R_EXTRACT;
      /* fallthrough */

    case GEN_RIGHTWARD_1_R_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN_R(1, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_RIGHTWARD_CENTER_R;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE_R(1, gen, result);
        return true;
      }
      break;

    case GEN_RIGHTWARD_CENTER_R:
      INIT_RIGHTWARD_CENTER_MOVES(_r, gen);
      gen->state = GEN_RIGHTWARD_CENTER_R_EXTRACT;
      /* fallthrough */

    case GEN_RIGHTWARD_CENTER_R_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_CENTER_ORIGIN(_r, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_DONE;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_CENTER_MOVE_R(gen, result);
        return true;
      }
      break;

    case GEN_DONE:
    default:
      return false;
    }
  }
  return false;
}

// Get the next move from the king generator
// Returns true if a move was generated, false if no more moves
bool next_move_king(move_generator *gen, move *result) {
  while (gen->state < GEN_KING_DONE) {
    switch (gen->state) {
    case GEN_KING_LEFTWARD_0:
      gen->dests = GEN_LEFTWARD_DESTS(0, , gen);
      /* fallthrough */

    case GEN_KING_LEFTWARD_0_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE(0, gen, result);
        gen->state = GEN_KING_LEFTWARD_0_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_KING_LEFTWARD_1:
      gen->dests = GEN_LEFTWARD_DESTS(1, , gen);
      /* fallthrough */

    case GEN_KING_LEFTWARD_1_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE(1, gen, result);
        gen->state = GEN_KING_LEFTWARD_1_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_KING_LEFTWARD_CENTER:
      gen->dests = GEN_LEFTWARD_CENTER_DESTS(, gen);
      /* fallthrough */

    case GEN_KING_LEFTWARD_CENTER_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_CENTER_MOVE(gen, result);
        gen->state = GEN_KING_LEFTWARD_CENTER_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_KING_LEFTWARD_0_R:
      gen->dests = GEN_LEFTWARD_DESTS(0, _r, gen);
      /* fallthrough */

    case GEN_KING_LEFTWARD_0_R_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE_R(0, gen, result);
        gen->state = GEN_KING_LEFTWARD_0_R_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_KING_LEFTWARD_1_R:
      gen->dests = GEN_LEFTWARD_DESTS(1, _r, gen);
      /* fallthrough */

    case GEN_KING_LEFTWARD_1_R_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_MOVE_R(1, gen, result);
        gen->state = GEN_KING_LEFTWARD_1_R_EXTRACT;
        return true;
      }
      /* fallthrough */

    case GEN_KING_LEFTWARD_CENTER_R:
      gen->dests = GEN_LEFTWARD_CENTER_DESTS(_r, gen);
      /* fallthrough */

    case GEN_KING_LEFTWARD_CENTER_R_EXTRACT:
      if (gen->dests) {
        EXTRACT_LEFTWARD_CENTER_MOVE_R(gen, result);
        gen->state = GEN_KING_LEFTWARD_CENTER_R_EXTRACT;
        return true;
      }
      /* fallthrough */

    // Rightward moves (king-specific for lower half)
    case GEN_KING_RIGHTWARD_0:
      INIT_RIGHTWARD_MOVES_KING(0, , gen);
      gen->state = GEN_KING_RIGHTWARD_0_EXTRACT;
      /* fallthrough */

    case GEN_KING_RIGHTWARD_0_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN(0, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_KING_RIGHTWARD_1;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE(0, gen, result);
        return true;
      }
      break;

    case GEN_KING_RIGHTWARD_1:
      INIT_RIGHTWARD_MOVES(1, , gen);
      gen->state = GEN_KING_RIGHTWARD_1_EXTRACT;
      /* fallthrough */

    case GEN_KING_RIGHTWARD_1_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN(1, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_KING_RIGHTWARD_CENTER;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE(1, gen, result);
        return true;
      }
      break;

    case GEN_KING_RIGHTWARD_CENTER:
      INIT_RIGHTWARD_CENTER_MOVES(, gen);
      gen->state = GEN_KING_RIGHTWARD_CENTER_EXTRACT;
      /* fallthrough */

    case GEN_KING_RIGHTWARD_CENTER_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_CENTER_ORIGIN(, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_KING_RIGHTWARD_0_R;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_CENTER_MOVE(gen, result);
        return true;
      }
      break;

    case GEN_KING_RIGHTWARD_0_R:
      INIT_RIGHTWARD_MOVES_KING(0, _r, gen);
      gen->state = GEN_KING_RIGHTWARD_0_R_EXTRACT;
      /* fallthrough */

    case GEN_KING_RIGHTWARD_0_R_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN_R(0, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_KING_RIGHTWARD_1_R;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE_R(0, gen, result);
        return true;
      }
      break;

    case GEN_KING_RIGHTWARD_1_R:
      INIT_RIGHTWARD_MOVES(1, _r, gen);
      gen->state = GEN_KING_RIGHTWARD_1_R_EXTRACT;
      /* fallthrough */

    case GEN_KING_RIGHTWARD_1_R_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_ORIGIN_R(1, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_KING_RIGHTWARD_CENTER_R;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_MOVE_R(1, gen, result);
        return true;
      }
      break;

    case GEN_KING_RIGHTWARD_CENTER_R:
      INIT_RIGHTWARD_CENTER_MOVES(_r, gen);
      gen->state = GEN_KING_RIGHTWARD_CENTER_R_EXTRACT;
      /* fallthrough */

    case GEN_KING_RIGHTWARD_CENTER_R_EXTRACT:
      if (gen->dests == 0) {
        GET_NEXT_RIGHTWARD_CENTER_ORIGIN(_r, gen);
        if (gen->origs == 0 && gen->dests == 0) {
          gen->state = GEN_KING_DONE;
          break;
        }
      }
      if (gen->dests) {
        EXTRACT_RIGHTWARD_CENTER_MOVE_R(gen, result);
        return true;
      }
      break;

    case GEN_KING_DONE:
    default:
      return false;
    }
  }
  return false;
}
