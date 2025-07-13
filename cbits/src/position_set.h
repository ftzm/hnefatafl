#include "stdint.h"
#include "stddef.h"
#include "util.h"

static inline u64 fastrange64(u64 word, u64 p);

typedef struct position_set {
  size_t size;
  u64 *elements;
} position_set_t;

position_set_t *create_position_set(size_t max_elems);

void destroy_position_set(position_set_t *set);

int insert_position(position_set_t *set, u64 position, int *deletion_index);

void delete_position(position_set_t *set, u64 index);
