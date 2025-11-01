#include "../src/layer.h"
#include "../src/types.h"
#include <stdio.h>

// Local mask arrays for generation
layer foe_masks[120];
layer foe_masks_r[120];
layer ally_masks[120];
layer ally_masks_r[120];
layer surround_masks[120];
layer surround_masks_r[120];

void gen_foe_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 11;
      OP_LAYER_BIT(foe_masks[i], target, |=);
      target_r = rotate_right[target];
      OP_LAYER_BIT(foe_masks_r[i], target_r, |=);
    }
    if (i > 21) {
      target = i - 11;
      foe_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      foe_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 1;
      foe_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      foe_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 1;
      foe_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      foe_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

void gen_surround_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 110) {
      target = i + 11;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 10) {
      target = i - 11;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 10) {
      target = i + 1;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 0) {
      target = i - 1;
      surround_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      surround_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

void gen_ally_masks() {
  int i, modDest, target, target_r;
  for (i = 0; i < 120; i++) {
    modDest = i % 11;
    if (i < 99) {
      target = i + 22;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (i > 21) {
      target = i - 22;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest < 9) {
      target = i + 2;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
    if (modDest > 1) {
      target = i - 2;
      ally_masks[i]._[SUB_LAYER(target)] |=
          ((u64)1 << sub_layer_offset_direct[target]);
      target_r = rotate_right[target];
      ally_masks_r[i]._[SUB_LAYER(target_r)] |=
          ((u64)1 << sub_layer_offset_direct[target_r]);
    }
  }
}

void print_mask_array(const char *array_name, layer masks[], int size) {
  printf("const layer %s[%d] = {\n", array_name, size);
  for (int i = 0; i < size; i++) {
    printf("  {{%juULL, %juULL}}", masks[i]._[0], masks[i]._[1]);
    if (i < size - 1) {
      printf(",");
    }
    printf("\n");
  }
  printf("};\n\n");
}

int main() {
  // Initialize mask arrays
  gen_foe_masks();
  gen_ally_masks();
  gen_surround_masks();

  // Print header
  printf("#pragma once\n\n");
  printf("#include \"layer.h\"\n\n");

  // Generate mask constants
  print_mask_array("foe_masks", foe_masks, 120);
  print_mask_array("foe_masks_r", foe_masks_r, 120);
  print_mask_array("ally_masks", ally_masks, 120);
  print_mask_array("ally_masks_r", ally_masks_r, 120);
  print_mask_array("surround_masks", surround_masks, 120);
  print_mask_array("surround_masks_r", surround_masks_r, 120);

  return 0;
}