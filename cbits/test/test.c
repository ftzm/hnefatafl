#include "greatest.h"

SUITE_EXTERN(capture_suite);

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {

  GREATEST_MAIN_BEGIN();
  RUN_SUITE(capture_suite);
  GREATEST_MAIN_END();
}
