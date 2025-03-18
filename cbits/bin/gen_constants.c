#include "io.h"
#include "layer.h"
#include "stdio.h"

char *drop_2_west = ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X"
                    ".  .  X  X  X  X  X  X  X  X  X";

char *drop_2_east = "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  ."
                    "X  X  X  X  X  X  X  X  X  .  .";

char *edges = "X  X  X  X  X  X  X  X  X  X  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  .  .  .  .  .  .  .  .  .  X"
              "X  X  X  X  X  X  X  X  X  X  X";

void print_layer_info(char *layer_str, char *name_str) {
  printf("LAYER: %s\n\n", name_str);
  layer l = read_layer(layer_str, 'X');
  print_layer(l);
  printf("{%juULL, %juULL}\n\n", l._[0], l._[1]);
  printf("------------------------------------------------------------\n\n");
};

#define PRINT_LAYER(name) print_layer_info(name, #name)

int main() {
  printf("\n");
  PRINT_LAYER(drop_2_west);
  PRINT_LAYER(drop_2_east);
  PRINT_LAYER(edges);
  }
