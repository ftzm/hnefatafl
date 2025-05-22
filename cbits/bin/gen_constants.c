#include "io.h"
#include "layer.h"
#include "stdio.h"

// -----------------------------------------------------------------------------
// Setup

void print_layer_info(char *layer_str, char *name_str) {
  layer l = read_layer(layer_str, 'X');
  printf("const layer %s = {%juULL, %juULL};\n", name_str, l._[0], l._[1]);
};

void print_layer_defines(char *layer_str, char *name_str) {
  layer l = read_layer(layer_str, 'X');
  printf("#define %s_0 %juULL\n", name_str, l._[0]);
  printf("#define %s_1 %juULL\n", name_str, l._[1]);
  printf("#define %s ((layer){%juULL, %juULL})\n", name_str, l._[0], l._[1]);
};

void print_layer_direct(layer l, char *name_str) {
  printf("const layer %s = {%juULL, %juULL};\n", name_str, l._[0], l._[1]);
};

#define PRINT_LAYER_DIRECT(name) print_layer_direct(name, #name);

// -----------------------------------------------------------------------------

int main() {
  printf("\n");
  print_layer_info(
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "exclude_center");
  print_layer_info(
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X"
      ".  .  X  X  X  X  X  X  X  X  X",
      "drop_2_west");
  print_layer_info(
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  X",
      "drop_1_west");
  print_layer_info(
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  ."
      "X  X  X  X  X  X  X  X  X  .  .",
      "drop_2_east");
  print_layer_info(
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  .",
      "drop_1_east");

  printf("\n");

  print_layer_info(
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "edges");

  printf("\n");

  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_10");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_9");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_8");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_7");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_6");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_5");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_4");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_3");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_2");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  X  X  X  X  X  X  X  X  X  X",
      "below_1");
  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "below_0");

  printf("\n");

  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X",
      "file_mask_0");

  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  .",
      "legal_file_mask_0");

  print_layer_info(
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  .",
      "file_mask_1");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  .",
      "FILE_MASK_2");

  print_layer_defines(
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  .",
      "FILE_MASK_3");

  print_layer_defines(
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  .",
      "FILE_MASK_4");

  print_layer_defines(
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  .",
      "FILE_MASK_5");

  print_layer_defines(
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  .",
      "FILE_MASK_6");


  print_layer_defines(
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  .",
      "FILE_MASK_7");

  print_layer_defines(
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  .",
      "FILE_MASK_8");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_2");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_3");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_4");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_5");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_6");


  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_7");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_ADJACENT_8");

  print_layer_info(
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  .",
      "file_mask_9");

  print_layer_info(
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  .",
      "file_mask_10");

  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "legal_file_mask_10");

  printf("\n");

  print_layer_info(
      "X  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  X",
      "corners");

  print_layer_info(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "throne");

  print_layer_info(
      ".  X  X  X  X  X  X  X  X  X  ."
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      "X  X  X  X  X  X  X  X  X  X  X"
      ".  X  X  X  X  X  X  X  X  X  .",
      "not_corners");

  layer above_0 = layer_shiftl(layer_neg(EMPTY_LAYER), 11);
  layer above_1 = layer_shiftl(above_0, 11);
  layer above_2 = layer_shiftl(above_1, 11);
  layer above_3 = layer_shiftl(above_2, 11);
  layer above_4 = layer_shiftl(above_3, 11);
  layer above_5 = layer_shiftl(above_4, 11);
  layer above_6 = layer_shiftl(above_5, 11);
  layer above_7 = layer_shiftl(above_6, 11);
  layer above_8 = layer_shiftl(above_7, 11);
  layer above_9 = layer_shiftl(above_8, 11);
  layer above_10 = layer_shiftl(above_9, 11);

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_0");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  X  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_1");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  X  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_2");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  X  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_3");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  X  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_4");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  X  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_5");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  X  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_6");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  X  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_7");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  X  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_8");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  X  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_9");

  print_layer_defines(
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  .",
      "FILE_MASK_EDGE_ADJACENT_10");




  printf("\n");

  PRINT_LAYER_DIRECT(above_0);
  PRINT_LAYER_DIRECT(above_1);
  PRINT_LAYER_DIRECT(above_2);
  PRINT_LAYER_DIRECT(above_3);
  PRINT_LAYER_DIRECT(above_4);
  PRINT_LAYER_DIRECT(above_5);
  PRINT_LAYER_DIRECT(above_6);
  PRINT_LAYER_DIRECT(above_7);
  PRINT_LAYER_DIRECT(above_8);
  PRINT_LAYER_DIRECT(above_9);
  PRINT_LAYER_DIRECT(above_10);
}
