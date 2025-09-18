#include "greatest.h"
#include "zobrist.h"
#include "position_set.h"

TEST test_add_and_remove_nullifies() {
  position_set *ps = create_position_set(70);
  int indexes[70] = {0};

  for (int i = 0; i < 70; i++) {
    u64 hash = mix(i);
    insert_position(ps, hash, &indexes[i]);
  }

  // 70 elements exist
  int pre_elem_count = 0;
  for (size_t i = 0; i < ps->size; i++) {
    if (ps->elements[i]) {
      pre_elem_count++;
    }
  }

  for (int i = 69; i >= 0; i--) {
    delete_position(ps, indexes[i]);
  }

  // 0 elements exist
  int post_elem_count = 0;
  for (size_t i = 0; i < ps->size; i++) {
    if (ps->elements[i]) {
      post_elem_count++;
    }
  }

  destroy_position_set(ps);

  ASSERT_EQ_FMT(pre_elem_count, 69, "elem count should be 69 after insertion, is %d");
  ASSERT_EQ_FMT(post_elem_count, 0, "elem count should be 0 after insertion, is %d");
  return GREATEST_TEST_RES_PASS;
}

TEST test_single_insertion_succeeds() {
  // creation
  position_set *ps = create_position_set(70);

  // act
  int index;
  int res = insert_position(ps, 7, &index);

  // cleanup 
  destroy_position_set(ps);

  // assert
  ASSERT_EQ_FMT(res, 0, "signal should be 0, is %d");

  return GREATEST_TEST_RES_PASS;
}

TEST test_duplicate_insertion_errors() {
  // creation
  position_set *ps = create_position_set(70);

  // act
  int index;
  insert_position(ps, 7, &index);
  int res = insert_position(ps, 7, &index);

  // cleanup 
  destroy_position_set(ps);

  // assert
  ASSERT_EQ_FMT(res, 1, "signal should be 1, is %d");

  return GREATEST_TEST_RES_PASS;
}

TEST test_position_value_zero() {
  // creation
  position_set *ps = create_position_set(70);

  // act - try to insert position 0
  int index;
  int res = insert_position(ps, 0, &index);

  // cleanup 
  destroy_position_set(ps);

  // assert - should succeed since 0 is a valid position value
  ASSERT_EQ_FMT(res, 0, "insertion of position 0 should succeed, got %d");

  return GREATEST_TEST_RES_PASS;
}

TEST test_hash_collisions_linear_probing() {
  // creation - use small size to force collisions
  position_set *ps = create_position_set(3);
  
  // Find values that hash to the same index
  u64 val1 = ps->size;  // Will hash to 0 
  u64 val2 = ps->size * 2;  // Will also hash to 0
  u64 val3 = ps->size * 3;  // Will also hash to 0
  
  // act - insert values that should collide
  int index1, index2, index3;
  int res1 = insert_position(ps, val1, &index1);
  int res2 = insert_position(ps, val2, &index2);
  int res3 = insert_position(ps, val3, &index3);
  
  // cleanup
  destroy_position_set(ps);
  
  // assert - all should succeed and get different indices due to linear probing
  ASSERT_EQ_FMT(res1, 0, "first insertion should succeed, got %d");
  ASSERT_EQ_FMT(res2, 0, "second insertion should succeed, got %d");
  ASSERT_EQ_FMT(res3, 0, "third insertion should succeed, got %d");
  ASSERT_NEQ(index1, index2);
  ASSERT_NEQ(index2, index3);
  ASSERT_NEQ(index1, index3);
  
  return GREATEST_TEST_RES_PASS;
}

TEST test_wraparound_behavior() {
  // creation - use small size to force wraparound
  position_set *ps = create_position_set(3);
  
  // Manually fill the last slot to force wraparound
  ps->elements[ps->size - 1] = 999;  // Fill last slot
  
  // Now insert a value that should probe at last index and wrap to beginning
  u64 val = ps->size - 1;  // Should hash to last index but find it occupied
  int index;
  int res = insert_position(ps, val, &index);
  
  // cleanup
  destroy_position_set(ps);
  
  // assert - should succeed and wrap to index 0
  ASSERT_EQ_FMT(res, 0, "insertion should succeed, got %d");
  ASSERT_EQ_FMT(index, 0, "insertion should wrap to index 0, got %d");
  
  return GREATEST_TEST_RES_PASS;
}

SUITE(position_set_suite) {
  RUN_TEST(test_add_and_remove_nullifies);
  RUN_TEST(test_single_insertion_succeeds);
  RUN_TEST(test_duplicate_insertion_errors);
  RUN_TEST(test_position_value_zero);
  RUN_TEST(test_hash_collisions_linear_probing);
  RUN_TEST(test_wraparound_behavior);
}
