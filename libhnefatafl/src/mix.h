#pragma once

#include <stdint.h>

typedef uint64_t u64;

/*
pseudo random number generation
https://jonkagstrom.com/mx3/mx3_rev2.html

These have really nice distribution over the whole integer, making it
appropriate for use with e.g. fastrange
*/
static inline u64 mix(u64 x) {
  x ^= x >> 32;
  x *= 0xbea225f9eb34556d;
  x ^= x >> 29;
  x *= 0xbea225f9eb34556d;
  x ^= x >> 32;
  x *= 0xbea225f9eb34556d;
  x ^= x >> 29;
  return x;
}