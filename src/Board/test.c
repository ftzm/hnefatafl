#include <stdio.h>
#include <assert.h>
#include <locale.h>
#include "move.c"

const char* corners_string =
  " X  .  .  .  .  .  .  .  .  .  X "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " X  .  .  .  .  .  .  .  .  .  X ";

const char* start_board_string =
  " .  .  .  X  X  X  X  X  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  .  .  .  .  .  .  .  . "
  " X  .  .  .  .  O  .  .  .  .  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  X  .  O  O  #  O  O  .  X  X "
  " X  .  .  .  O  O  O  .  .  .  X "
  " X  .  .  .  .  O  .  .  .  .  X "
  " .  .  .  .  .  .  .  .  .  .  . "
  " .  .  .  .  .  X  .  .  .  .  . "
  " .  .  .  X  X  X  X  X  .  .  . ";

int main(int argc, char **argv) {
  printf("Running test\n");

  printf("\n");
  printf("------------------------------------------------------------\n");
  printf("Original\n");
  printf("\n");

  // read and verify boards
  u128 corner_u128 = read_layer(corners_string);

  // begin time
  // setup
  // run for result
  // run for bench
  // end time

  printf("\n");
  printf("------------------------------------------------------------\n");
  printf("tri rot\n");
  printf("\n");


}

