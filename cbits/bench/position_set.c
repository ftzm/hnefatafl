#include "position_set.h"
#include "zobrist.h"
#include "ubench.h"
#include "stdio.h"
#include "stdlib.h"

// Simple array-based set for comparison
typedef struct simple_set {
  u64 *elements;
  int count;
  int capacity;
} simple_set;

simple_set *create_simple_set(int capacity) {
  simple_set *set = malloc(sizeof(simple_set));
  set->elements = malloc(capacity * sizeof(u64));
  set->count = 0;
  set->capacity = capacity;
  return set;
}

void destroy_simple_set(simple_set *set) {
  free(set->elements);
  free(set);
}

// Returns 1 if element already exists, 0 if inserted successfully
int simple_set_insert(simple_set *set, u64 value) {
  // Check if already exists
  for (int i = 0; i < set->count; i++) {
    if (set->elements[i] == value) {
      return 1;  // Already exists
    }
  }
  
  // Insert if not full
  if (set->count < set->capacity) {
    set->elements[set->count] = value;
    set->count++;
    return 0;  // Successfully inserted
  }
  
  return -1;  // Set full
}

// Returns 1 if element exists, 0 if not found
int simple_set_contains(simple_set *set, u64 value) {
  for (int i = 0; i < set->count; i++) {
    if (set->elements[i] == value) {
      return 1;  // Found
    }
  }
  return 0;  // Not found
}

void simple_set_clear(simple_set *set) {
  set->count = 0;
}

// From fastrange.h - needed for position_set_contains
static inline u64 fastrange64(u64 word, u64 p) {
  return (u64)(((__uint128_t)word * (__uint128_t)p) >> 64);
}

// Helper function to test membership in position_set without modifying it
int position_set_contains(position_set *set, u64 value) {
  u64 index = fastrange64(value, set->size);
  
  // iterate until we find an empty cell or the value
  while (set->elements[index]) {
    if (set->elements[index] == value) {
      return 1;  // Found
    }
    
    if (index < (set->size - 1)) {
      index++;
    } else {
      index = 0;
    }
  }
  
  return 0;  // Not found
}

// Generate test data for given size
void generate_test_data(u64 *data, int size) {
  for (int i = 0; i < size; i++) {
    data[i] = mix(i * 12345 + 67890);  // Generate pseudo-random hashes
  }
}

// Benchmark insertion at different load factors
UBENCH_EX(position_set_insert, load_10_percent) {
  const int target_size = 30;
  const int insert_count = 3;  // 10% of 30
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    for (size_t i = 0; i < ps->size; i++) {
      ps->elements[i] = 0;
    }
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      int index;
      insert_position(ps, test_data[i], &index);
    }
    UBENCH_DO_NOTHING(ps);
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_insert, load_25_percent) {
  const int target_size = 30;
  const int insert_count = 7;  // ~25% of 30
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    for (size_t i = 0; i < ps->size; i++) {
      ps->elements[i] = 0;
    }
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      int index;
      insert_position(ps, test_data[i], &index);
    }
    UBENCH_DO_NOTHING(ps);
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_insert, load_50_percent) {
  const int target_size = 30;
  const int insert_count = 15;  // 50% of 30
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    for (size_t i = 0; i < ps->size; i++) {
      ps->elements[i] = 0;
    }
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      int index;
      insert_position(ps, test_data[i], &index);
    }
    UBENCH_DO_NOTHING(ps);
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_insert, load_75_percent) {
  const int target_size = 30;
  const int insert_count = 22;  // ~75% of 30
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    for (size_t i = 0; i < ps->size; i++) {
      ps->elements[i] = 0;
    }
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      int index;
      insert_position(ps, test_data[i], &index);
    }
    UBENCH_DO_NOTHING(ps);
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_insert, load_90_percent) {
  const int target_size = 30;
  const int insert_count = 27;  // 90% of 30
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    for (size_t i = 0; i < ps->size; i++) {
      ps->elements[i] = 0;
    }
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      int index;
      insert_position(ps, test_data[i], &index);
    }
    UBENCH_DO_NOTHING(ps);
  }
  
  destroy_position_set(ps);
}

// Benchmark simple array-based set insertion at different load factors
UBENCH_EX(simple_set_insert, load_10_percent) {
  const int target_size = 30;
  const int insert_count = 3;  // 10% of 30
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    simple_set_clear(ss);
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      simple_set_insert(ss, test_data[i]);
    }
    UBENCH_DO_NOTHING(ss);
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_insert, load_25_percent) {
  const int target_size = 30;
  const int insert_count = 7;  // ~25% of 30
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    simple_set_clear(ss);
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      simple_set_insert(ss, test_data[i]);
    }
    UBENCH_DO_NOTHING(ss);
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_insert, load_50_percent) {
  const int target_size = 30;
  const int insert_count = 15;  // 50% of 30
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    simple_set_clear(ss);
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      simple_set_insert(ss, test_data[i]);
    }
    UBENCH_DO_NOTHING(ss);
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_insert, load_75_percent) {
  const int target_size = 30;
  const int insert_count = 22;  // ~75% of 30
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    simple_set_clear(ss);
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      simple_set_insert(ss, test_data[i]);
    }
    UBENCH_DO_NOTHING(ss);
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_insert, load_90_percent) {
  const int target_size = 30;
  const int insert_count = 27;  // 90% of 30
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  UBENCH_DO_BENCHMARK() {
    // Reset set
    simple_set_clear(ss);
    
    // Insert elements
    for (int i = 0; i < insert_count; i++) {
      simple_set_insert(ss, test_data[i]);
    }
    UBENCH_DO_NOTHING(ss);
  }
  
  destroy_simple_set(ss);
}

// Benchmark membership tests (successful lookups) at different load factors
UBENCH_EX(position_set_lookup_hit, load_10_percent) {
  const int target_size = 30;
  const int insert_count = 3;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = position_set_contains(ps, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_lookup_hit, load_25_percent) {
  const int target_size = 30;
  const int insert_count = 7;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = position_set_contains(ps, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_lookup_hit, load_50_percent) {
  const int target_size = 30;
  const int insert_count = 15;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = position_set_contains(ps, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_lookup_hit, load_75_percent) {
  const int target_size = 30;
  const int insert_count = 22;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = position_set_contains(ps, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_lookup_hit, load_90_percent) {
  const int target_size = 30;
  const int insert_count = 27;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = position_set_contains(ps, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_position_set(ps);
}

// Benchmark simple array-based set membership tests (successful lookups) at different load factors
UBENCH_EX(simple_set_lookup_hit, load_10_percent) {
  const int target_size = 30;
  const int insert_count = 3;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = simple_set_contains(ss, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_lookup_hit, load_25_percent) {
  const int target_size = 30;
  const int insert_count = 7;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = simple_set_contains(ss, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_lookup_hit, load_50_percent) {
  const int target_size = 30;
  const int insert_count = 15;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = simple_set_contains(ss, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_lookup_hit, load_75_percent) {
  const int target_size = 30;
  const int insert_count = 22;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = simple_set_contains(ss, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_lookup_hit, load_90_percent) {
  const int target_size = 30;
  const int insert_count = 27;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for all inserted elements
    for (int i = 0; i < insert_count; i++) {
      int result = simple_set_contains(ss, test_data[i]);
      UBENCH_DO_NOTHING(&result);  // Should be 1 (found)
    }
  }
  
  destroy_simple_set(ss);
}

// Benchmark simple array-based set membership tests (failed lookups) at different load factors
UBENCH_EX(simple_set_lookup_miss, load_10_percent) {
  const int target_size = 30;
  const int insert_count = 3;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for non-existent elements
    for (int i = 0; i < insert_count; i++) {
      u64 non_existent = test_data[i] + 999999;
      int result = simple_set_contains(ss, non_existent);
      UBENCH_DO_NOTHING(&result);  // Should be 0 (not found)
    }
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_lookup_miss, load_50_percent) {
  const int target_size = 30;
  const int insert_count = 15;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for non-existent elements
    for (int i = 0; i < insert_count; i++) {
      u64 non_existent = test_data[i] + 999999;
      int result = simple_set_contains(ss, non_existent);
      UBENCH_DO_NOTHING(&result);  // Should be 0 (not found)
    }
  }
  
  destroy_simple_set(ss);
}

UBENCH_EX(simple_set_lookup_miss, load_90_percent) {
  const int target_size = 30;
  const int insert_count = 27;
  simple_set *ss = create_simple_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    simple_set_insert(ss, test_data[i]);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for non-existent elements
    for (int i = 0; i < insert_count; i++) {
      u64 non_existent = test_data[i] + 999999;
      int result = simple_set_contains(ss, non_existent);
      UBENCH_DO_NOTHING(&result);  // Should be 0 (not found)
    }
  }
  
  destroy_simple_set(ss);
}

// Benchmark membership tests (failed lookups) at different load factors
UBENCH_EX(position_set_lookup_miss, load_10_percent) {
  const int target_size = 30;
  const int insert_count = 3;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for non-existent elements
    for (int i = 0; i < insert_count; i++) {
      u64 non_existent = test_data[i] + 999999;
      int result = position_set_contains(ps, non_existent);
      UBENCH_DO_NOTHING(&result);  // Should be 0 (not found)
    }
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_lookup_miss, load_50_percent) {
  const int target_size = 30;
  const int insert_count = 15;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for non-existent elements
    for (int i = 0; i < insert_count; i++) {
      u64 non_existent = test_data[i] + 999999;
      int result = position_set_contains(ps, non_existent);
      UBENCH_DO_NOTHING(&result);  // Should be 0 (not found)
    }
  }
  
  destroy_position_set(ps);
}

UBENCH_EX(position_set_lookup_miss, load_90_percent) {
  const int target_size = 30;
  const int insert_count = 27;
  position_set *ps = create_position_set(target_size);
  u64 test_data[insert_count];
  generate_test_data(test_data, insert_count);
  
  // Pre-fill the set
  for (int i = 0; i < insert_count; i++) {
    int index;
    insert_position(ps, test_data[i], &index);
  }
  
  UBENCH_DO_BENCHMARK() {
    // Test membership for non-existent elements
    for (int i = 0; i < insert_count; i++) {
      u64 non_existent = test_data[i] + 999999;
      int result = position_set_contains(ps, non_existent);
      UBENCH_DO_NOTHING(&result);  // Should be 0 (not found)
    }
  }
  
  destroy_position_set(ps);
}

// needs to be at top level
UBENCH_STATE();

int main() {
  return ubench_main(0, NULL);
}