//  TODO: switch to defines everywhere

#include "layer.c"
#include "stdio.h"
#include "string.h"

// -----------------------------------------------------------------------------
// Setup

layer read_layer(const char *string, uint8_t symbol) {
  layer output = EMPTY_LAYER;
  int len = strlen(string);
  int index = 120;
  for (int i = 0; i < len; i++) {
    char c = string[i];
    if (c == symbol) {
      output._[sub_layer(index)] |=
          ((uint64_t)1 << sub_layer_offset_direct[index]);
      index--;
    } else if (c == ' ') {
      // skip space
    } else {
      index--; // skip other chars but increment
    }
  }

  return output;
}

void print_layer_info(char *layer_str, char *name_str) {
  layer l = read_layer(layer_str, 'X');
  printf("static const layer %s = {%juULL, %juULL};\n", name_str, l._[0], l._[1]);
};

void print_layer_defines(char *layer_str, char *name_str) {
  layer l = read_layer(layer_str, 'X');
  printf("#define %s_0 %juULL\n", name_str, l._[0]);
  printf("#define %s_1 %juULL\n", name_str, l._[1]);
  printf("#define %s ((layer){%juULL, %juULL})\n", name_str, l._[0], l._[1]);
};

void print_layer_direct(layer l, char *name_str) {
  printf("static const layer %s = {%juULL, %juULL};\n", name_str, l._[0], l._[1]);
};

#define PRINT_LAYER_DIRECT(name) print_layer_direct(name, #name);

// -----------------------------------------------------------------------------

int main() {
  printf("#pragma once\n\n");
  printf("#include \"layer.h\"\n");
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

  layer above_0 = LAYER_SHIFTL_SHORT(layer_neg(EMPTY_LAYER), 11);
  layer above_1 = LAYER_SHIFTL_SHORT(above_0, 11);
  layer above_2 = LAYER_SHIFTL_SHORT(above_1, 11);
  layer above_3 = LAYER_SHIFTL_SHORT(above_2, 11);
  layer above_4 = LAYER_SHIFTL_SHORT(above_3, 11);
  layer above_5 = LAYER_SHIFTL_SHORT(above_4, 11);
  layer above_6 = LAYER_SHIFTL_SHORT(above_5, 11);
  layer above_7 = LAYER_SHIFTL_SHORT(above_6, 11);
  layer above_8 = LAYER_SHIFTL_SHORT(above_7, 11);
  layer above_9 = LAYER_SHIFTL_SHORT(above_8, 11);
  layer above_10 = LAYER_SHIFTL_SHORT(above_9, 11);

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

  print_layer_defines(
      ".  X  .  .  .  .  .  .  .  X  ."
      "X  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  X"
      ".  X  .  .  .  .  .  .  .  X  .",
      "ADJACENTS");

  print_layer_defines(
      "X  X  .  .  .  .  .  .  .  X  X"
      "X  .  .  .  .  .  .  .  .  .  X"
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      ".  .  .  .  .  .  .  .  .  .  ."
      "X  .  .  .  .  .  .  .  .  .  X"
      "X  X  .  .  .  .  .  .  .  X  X",
      "CORNERS_AND_ADJACENTS");




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

  print_layer_info(
    ".  .  X  .  .  .  .  .  .  .  ."
    ".  X  .  .  .  .  .  .  .  .  ."
    "X  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
      "corner_guard_nw");

  print_layer_info(
    ".  .  .  .  .  .  .  .  X  .  ."
    ".  .  .  .  .  .  .  .  .  X  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  .",
      "corner_guard_ne");

  print_layer_info(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    "X  .  .  .  .  .  .  .  .  .  ."
    ".  X  .  .  .  .  .  .  .  .  ."
    ".  .  X  .  .  .  .  .  .  .  .",
      "corner_guard_sw");

  print_layer_info(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  X"
    ".  .  .  .  .  .  .  .  .  X  ."
    ".  .  .  .  .  .  .  .  X  .  .",
      "corner_guard_se");

  print_layer_info(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  X  X  X  X  X  X  X  X  X  ."
    ".  .  .  .  .  .  .  .  .  .  .",
      "INTERIOR");

  print_layer_info(
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  .  ."
    ".  .  .  .  .  .  .  .  .  X  ."
    ".  .  .  .  .  .  .  .  X  .  X"
    ".  .  .  .  .  .  .  .  .  X  .",
      "SURROUND_MASK");
}
