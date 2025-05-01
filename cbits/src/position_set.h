#include "stdint.h"
#include "stddef.h"

static inline uint64_t fastrange64(uint64_t word, uint64_t p);

typedef struct position_set {
  size_t size;
  uint64_t *elements;
} position_set_t;

position_set_t *create_position_set(size_t max_elems);

void destroy_position_set(position_set_t *set);

int insert_position(position_set_t *set, uint64_t position, int *deletion_index);

void delete_position(position_set_t *set, uint64_t index);
