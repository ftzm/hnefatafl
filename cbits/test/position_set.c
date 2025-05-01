#include "greatest.h"
#include "zobrist.h"
#include "position_set.h"

TEST test_add_and_remove_nullifies() {
  position_set_t *ps = create_position_set(70);
  int indexes[70] = {0};

  for (int i = 0; i < 70; i++) {
    uint64_t hash = mix(i);
    insert_position(ps, hash, &indexes[i]);
  }

  // 70 elements exist
  int pre_elem_count = 0;
  for (int i = 0; i < ps->size; i++) {
    if (ps->elements[i]) {
      pre_elem_count++;
    }
  }

  for (int i = 69; i >= 0; i--) {
    delete_position(ps, indexes[i]);
  }

  // 0 elements exist
  int post_elem_count = 0;
  for (int i = 0; i < ps->size; i++) {
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
  position_set_t *ps = create_position_set(70);

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
  position_set_t *ps = create_position_set(70);

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

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {

  GREATEST_MAIN_BEGIN();

  RUN_TEST(test_add_and_remove_nullifies);
  RUN_TEST(test_single_insertion_succeeds);
  RUN_TEST(test_duplicate_insertion_errors);

  GREATEST_MAIN_END();
}
