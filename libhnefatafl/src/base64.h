#pragma once

// Base64 encoding/decoding functions
int base64_encoded_size(int input_len);
void base64_encode(const char *data, int data_len, char *output);
int base64_decode(const char *encoded, char *output, int max_output_len);