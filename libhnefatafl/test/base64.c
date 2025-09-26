#include "greatest.h"
#include "move_list.h"
#include "theft.h"
#include "theft_types.h"
#include <stdlib.h>
#include <string.h>

// Structure to hold generated test data (array of chars with length)
typedef struct {
  char *data;
  int length;
} test_data;

// Allocate random test data for property testing
static enum theft_alloc_res alloc_test_data(struct theft *t, void *env, void **instance) {
  (void)env;
  
  test_data *td = malloc(sizeof(test_data));
  if (!td) return THEFT_ALLOC_ERROR;
  
  // Generate random length between 0 and 100
  td->length = theft_random_choice(t, 101);
  
  if (td->length == 0) {
    td->data = NULL;
    *instance = td;
    return THEFT_ALLOC_OK;
  }
  
  td->data = malloc(td->length);
  if (!td->data) {
    free(td);
    return THEFT_ALLOC_ERROR;
  }
  
  // Fill with random bytes (including null bytes)
  for (int i = 0; i < td->length; i++) {
    td->data[i] = (char)theft_random_choice(t, 256);
  }
  
  *instance = td;
  return THEFT_ALLOC_OK;
}

// Free test data
static void free_test_data(void *instance, void *type_info) {
  (void)type_info;
  test_data *td = (test_data *)instance;
  if (td) {
    free(td->data);
    free(td);
  }
}

// Print test data for debugging
static void print_test_data(FILE *f, const void *instance, void *type_info) {
  (void)type_info;
  const test_data *td = (const test_data *)instance;
  fprintf(f, "test_data{length=%d, data=[", td->length);
  for (int i = 0; i < td->length && i < 20; i++) {
    fprintf(f, "%02x", (unsigned char)td->data[i]);
    if (i < td->length - 1 && i < 19) fprintf(f, " ");
  }
  if (td->length > 20) fprintf(f, "...");
  fprintf(f, "]}");
}

// Type info for theft
static struct theft_type_info test_data_info = {
    .alloc = alloc_test_data,
    .free = free_test_data,
    .print = print_test_data,
    .autoshrink_config = {.enable = false},
};

// Property test: encode then decode should return original data
static enum theft_trial_res prop_encode_decode_roundtrip(struct theft *t, void *arg1) {
  (void)t;
  test_data *td = (test_data *)arg1;
  
  if (td->length == 0) {
    // Test empty input
    int encoded_size = base64_encoded_size(0);
    char *encoded = malloc(encoded_size);
    base64_encode(td->data, 0, encoded);
    
    char decoded[1];
    int decoded_len = base64_decode(encoded, decoded, 1);
    
    free(encoded);
    return (decoded_len == 0) ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
  }
  
  // Calculate required buffer size and allocate
  int encoded_size = base64_encoded_size(td->length);
  char *encoded = malloc(encoded_size);
  if (!encoded) return THEFT_TRIAL_ERROR;
  
  // Encode the data
  base64_encode(td->data, td->length, encoded);
  
  // Allocate buffer for decoded data (should be at least as big as original)
  char *decoded = malloc(td->length);
  if (!decoded) {
    free(encoded);
    return THEFT_TRIAL_ERROR;
  }
  
  // Decode the encoded data
  int decoded_len = base64_decode(encoded, decoded, td->length);
  
  // Check that decode length matches original length
  if (decoded_len != td->length) {
    free(encoded);
    free(decoded);
    return THEFT_TRIAL_FAIL;
  }
  
  // Check that decoded data matches original data
  int matches = (memcmp(td->data, decoded, td->length) == 0);
  
  free(encoded);
  free(decoded);
  
  return matches ? THEFT_TRIAL_PASS : THEFT_TRIAL_FAIL;
}

// Test with property-based testing
TEST base64_encode_decode_roundtrip() {
  uint64_t seed = theft_seed_of_time();
  
  struct theft_run_config config = {
      .name = __func__,
      .prop1 = prop_encode_decode_roundtrip,
      .type_info = {&test_data_info},
      .trials = 1000,
      .seed = seed,
  };
  
  enum theft_run_res res = theft_run(&config);
  
  ASSERT_ENUM_EQm("pass", THEFT_RUN_PASS, res, theft_run_res_str);
  PASS();
}

// Simple unit tests for edge cases
TEST base64_empty_input() {
  char output[10];
  base64_encode(NULL, 0, output);
  ASSERT_STR_EQ("", output);
  
  char decoded[10];
  int len = base64_decode("", decoded, 10);
  ASSERT_EQ(0, len);
  PASS();
}

TEST base64_known_values() {
  // Test known base64 encodings
  char output[20];
  
  // "A" -> "QQ=="
  base64_encode("A", 1, output);
  ASSERT_STR_EQ("QQ==", output);
  
  // "AB" -> "QUI="
  base64_encode("AB", 2, output);
  ASSERT_STR_EQ("QUI=", output);
  
  // "ABC" -> "QUJD"
  base64_encode("ABC", 3, output);
  ASSERT_STR_EQ("QUJD", output);
  
  PASS();
}

TEST base64_decode_known_values() {
  char decoded[10];
  int len;
  
  // "QQ==" -> "A"
  len = base64_decode("QQ==", decoded, 10);
  ASSERT_EQ(1, len);
  ASSERT_EQ('A', decoded[0]);
  
  // "QUI=" -> "AB" 
  len = base64_decode("QUI=", decoded, 10);
  ASSERT_EQ(2, len);
  ASSERT_EQ('A', decoded[0]);
  ASSERT_EQ('B', decoded[1]);
  
  // "QUJD" -> "ABC"
  len = base64_decode("QUJD", decoded, 10);
  ASSERT_EQ(3, len);
  ASSERT_EQ('A', decoded[0]);
  ASSERT_EQ('B', decoded[1]);
  ASSERT_EQ('C', decoded[2]);
  
  PASS();
}

TEST move_list_roundtrip() {
  // Test empty move list
  char encoded[100];
  move_list_to_base64(NULL, 0, encoded);
  ASSERT_STR_EQ("", encoded);
  
  int decoded_count_empty;
  move *decoded_moves_empty = move_list_from_base64("", &decoded_count_empty);
  ASSERT_EQ(0, decoded_count_empty);
  ASSERT_EQ(NULL, decoded_moves_empty);
  
  // Test single move
  move single_move = {5, 10}; // orig=5, dest=10
  
  char single_encoded[100];
  move_list_to_base64(&single_move, 1, single_encoded);
  
  int decoded_count_single;
  move *decoded_single = move_list_from_base64(single_encoded, &decoded_count_single);
  ASSERT_EQ(1, decoded_count_single);
  ASSERT_EQ(5, decoded_single[0].orig);
  ASSERT_EQ(10, decoded_single[0].dest);
  free(decoded_single);
  
  // Test multiple moves
  move moves[] = {{1, 2}, {3, 4}, {255, 0}}; // Test edge values too
  
  char multiple_encoded[100];
  move_list_to_base64(moves, 3, multiple_encoded);
  
  int decoded_count_multiple;
  move *decoded_multiple = move_list_from_base64(multiple_encoded, &decoded_count_multiple);
  ASSERT_EQ(3, decoded_count_multiple);
  ASSERT_EQ(1, decoded_multiple[0].orig);
  ASSERT_EQ(2, decoded_multiple[0].dest);
  ASSERT_EQ(3, decoded_multiple[1].orig);
  ASSERT_EQ(4, decoded_multiple[1].dest);
  ASSERT_EQ(255, decoded_multiple[2].orig);
  ASSERT_EQ(0, decoded_multiple[2].dest);
  free(decoded_multiple);
  
  PASS();
}

TEST move_list_invalid_base64() {
  // Test invalid base64 (wrong length for moves)
  int result_count;
  move *result_moves = move_list_from_base64("QQ", &result_count); // Decodes to 1 byte, not even number
  ASSERT_EQ(0, result_count);
  ASSERT_EQ(NULL, result_moves);
  
  PASS();
}

SUITE(base64_suite) {
  RUN_TEST(base64_encode_decode_roundtrip);
  RUN_TEST(base64_empty_input);
  RUN_TEST(base64_known_values);
  RUN_TEST(base64_decode_known_values);
  RUN_TEST(move_list_roundtrip);
  RUN_TEST(move_list_invalid_base64);
}