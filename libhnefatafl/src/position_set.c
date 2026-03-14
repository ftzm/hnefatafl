/*******************************************************************************
A set containing all positions that have occured in a given game.

Its elements are zobrist hashes.

It is implemented as a hash table which uses simplified linear
probing. The LIFO/stack access pattern characteristic of a game tree
search (in which the elements will only ever be removed in the reverse
order of their insertion) allows us to remove elements without
worrying about the "holes" which typical linear probing
implementations must deal with. This speeds up removal compared to a
standard implementation. In a typical linear probing implementation
holes are created by removing an element which occupies a cell which a
later-inserted element would have liked to occupy--we will never do
that.

NOTE: 0 is used as the empty-slot sentinel. A zobrist hash of exactly
0 cannot be stored or looked up correctly. This is an accepted
trade-off, as the probability of a zobrist hash of 0 is low.

*******************************************************************************/

#include "position_set.h"
#include "stddef.h"
#include "stdlib.h"

static inline size_t next_power_of_2(size_t v) {
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  v |= v >> 32;
  v++;
  return v;
}

position_set *create_position_set(size_t max_elems) {
  // Use power-of-2 sizing so index computation is a bitmask (single AND)
  // instead of a 128-bit multiply (fastrange64).
  // Oversize by ~2x for low load factor with linear probing.
  size_t size = next_power_of_2(MAX(max_elems * 2, 4));
  position_set *set = malloc(sizeof(position_set));
  *set = (position_set){.mask = size - 1, .elements = calloc(size, sizeof(u64))};
  return set;
}

void destroy_position_set(position_set *set) {
  free(set->elements);
  free(set);
}

int insert_position(position_set *set, u64 position, int *deletion_index) {
  u64 index = position & set->mask;

  // iterate until we find an empty cell
  while (set->elements[index]) {

    // if a cell has the value we're trying to insert then we bail out
    // and return an error.
    if (set->elements[index] == position) {
      return 1;
    }

    index = (index + 1) & set->mask;
  }

  // If we hit this point we've found an empty cell into which we insert
  set->elements[index] = position;
  *deletion_index = index;
  return 0;
}

int check_position(position_set *set, u64 position) {
  u64 index = position & set->mask;

  // iterate until we find an empty cell
  while (set->elements[index]) {

    // if a cell has the value we're looking for we return an error.
    if (set->elements[index] == position) {
      return 1;
    }

    index = (index + 1) & set->mask;
  }

  // If we hit this point we've not encountered our value
  return 0;
}

void delete_position(position_set *set, u64 index) { set->elements[index] = 0; }
