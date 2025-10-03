#include <stdio.h>
#include "../src/mix.h"

void print_hash_array(const char* name, int seed_start, int count) {
  printf("static const u64 %s[%d] = {\n", name, count);
  for (int i = 0; i < count; i++) {
    u64 hash = mix(seed_start + i);
    printf("  %juULL", hash);
    if (i < count - 1) {
      printf(",");
    }
    printf("\n");
  }
  printf("};\n\n");
}

int main() {
  printf("#pragma once\n\n");
  printf("#include <stdint.h>\n\n");
  printf("typedef uint64_t u64;\n\n");
  
  int seed = 78;
  
  // Generate black piece hashes (positions 0-120)
  print_hash_array("black_hashes", seed, 121);
  seed += 121;
  
  // Generate white piece hashes (positions 0-120)
  print_hash_array("white_hashes", seed, 121);
  seed += 121;
  
  // Generate king piece hashes (positions 0-120)
  print_hash_array("king_hashes", seed, 121);
  seed += 121;
  
  // Generate is_black_turn hash
  u64 is_black_hash = mix(seed);
  printf("static const u64 is_black_hash = %juULL;\n", is_black_hash);
  
  return 0;
}