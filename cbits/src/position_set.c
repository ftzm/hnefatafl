/*******************************************************************************
A set containing all positions that have occured in a given game.

Its elements are zobrist hashes.

It is implemented as a hash table which uses simplified linear
probing. The LIFO access pattern characteristic of a game tree search
(in which the elements will only ever be removed in the reverse order
of their insertion) allows us to remove elements without worrying
about the "holes" which typical linear probing implementations must
deal with, which will speed up removal compared to a standard
implementation. Holes are created by removing an element which
occupies a cell which a later-inserted element would have liked to
occupy--we will never do that.

*******************************************************************************/

#include "position_set.h"
#include "stdint.h"
#include "stddef.h"
#include "stdlib.h"

/**
from https://github.com/lemire/fastrange/blob/master/fastrange.h

Given a value "word", produces an integer in [0,p) without division.
The function is as fair as possible in the sense that if you iterate
through all possible values of "word", then you will generate all
possible outputs as uniformly as possible.
*/
static inline uint64_t fastrange64(uint64_t word, uint64_t p) {
  return (uint64_t)(((__uint128_t)word * (__uint128_t)p) >> 64);
}

position_set_t *create_position_set(size_t max_elems) {
  size_t size = max_elems * 1.3;
  position_set_t *set = malloc(sizeof(position_set_t));
  *set = (position_set_t) {
    .size = size,
    .elements = malloc(sizeof(uint64_t) * size)
  };
  return set;
}

void destroy_position_set(position_set_t *set) {
  free(set->elements);
  free(set);
}

int insert_position(position_set_t *set, uint64_t position, int *deletion_index) {
  uint64_t index = fastrange64(position, set->size);

  // iterate until we find an empty cell
  while (set->elements[index]) {

    // if a cell has the value we're trying to insert then we bail out
    // and return an error.
    if (set->elements[index] == position) {
      return 1;
    }

    if (index < (set->size - 1)) {
      index++;
    } else {
      index = 0;
    }
  }

  // If we hit this point we've found an empty cell into which we insert
  set->elements[index] = position;
  *deletion_index = index;
  return 0;
}

void delete_position(position_set_t *set, uint64_t index) {
  set->elements[index] = 0;
}
