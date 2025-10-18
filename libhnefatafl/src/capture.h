#pragma once

#include "types.h"

extern const layer foe_masks[120];
extern const layer foe_masks_r[120];
extern const layer ally_masks[120];
extern const layer ally_masks_r[120];
extern const layer surround_masks[120];
extern const layer surround_masks_r[120];

layer simple_capture_destinations(
    const layer allies,
    const layer foes,
    const layer occ);
layer black_capture_destinations(const board *b);
layer white_capture_destinations(const board *b);
layer black_capture_destinations_r(const board *b);
layer white_capture_destinations_r(const board *b);

u8 apply_captures_niave(
    const layer friends,
    layer *foes,
    layer *foes_r,
    int dest);

layer apply_captures_z_black(board *b, u64 *z, u8 dest);
layer apply_captures_z_white(board *b, u64 *z, u8 dest);
#define apply_captures_z_king apply_captures_z_white

layer shield_wall_black(board *b, u64 *z, u8 pos);
layer shield_wall_white(board *b, u64 *z, u8 pos);

// Micro-benchmarks indicated that these are significantly slower than the above
// versions, but there's also a significant code-size difference, so we'll need
// to do realistic search benchmarks before we get a conclusive answer regarding
// which to use.
void shield_wall_black_gen(board *b, u8 pos);
void shield_wall_white_gen(board *b, u8 pos);
