#include "stdint.h"
#include "stddef.h"
#include "util.h"


typedef struct position_set {
  size_t size;
  u64 *elements;
} position_set;

position_set *create_position_set(size_t max_elems);

void destroy_position_set(position_set *set);

int insert_position(position_set *set, u64 position, int *deletion_index);

void delete_position(position_set *set, u64 index);
