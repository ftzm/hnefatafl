#include "base64.h"
#include "string.h"

static const char base64_chars[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int base64_encoded_size(int input_len) {
  return ((input_len + 2) / 3) * 4 + 1; // +1 for null terminator
}

void base64_encode(const char *data, int data_len, char *output) {
  int i, j = 0;

  for (i = 0; i < data_len; i += 3) {
    // Pack 3 bytes into 24 bits
    unsigned int triple = 0;
    int bytes_in_group = 0;

    // First byte
    if (i < data_len) {
      triple |= ((unsigned char)data[i]) << 16;
      bytes_in_group++;
    }

    // Second byte
    if (i + 1 < data_len) {
      triple |= ((unsigned char)data[i + 1]) << 8;
      bytes_in_group++;
    }

    // Third byte
    if (i + 2 < data_len) {
      triple |= ((unsigned char)data[i + 2]);
      bytes_in_group++;
    }

    // Extract 4 base64 characters from 24 bits
    output[j++] = base64_chars[(triple >> 18) & 0x3F];
    output[j++] = base64_chars[(triple >> 12) & 0x3F];
    output[j++] =
        (bytes_in_group > 1) ? base64_chars[(triple >> 6) & 0x3F] : '=';
    output[j++] = (bytes_in_group > 2) ? base64_chars[triple & 0x3F] : '=';
  }

  output[j] = '\0';
}

int base64_decode(const char *encoded, char *output, int max_output_len) {
  int i, j = 0;
  int len = strlen(encoded);

  // Create lookup table for base64 characters
  int decode_table[128];
  for (i = 0; i < 128; i++)
    decode_table[i] = -1;
  for (i = 0; i < 64; i++)
    decode_table[(int)base64_chars[i]] = i;

  for (i = 0; i < len && j < max_output_len; i += 4) {
    // Get 4 base64 characters
    int a = (i < len && encoded[i] != '=') ? decode_table[(int)encoded[i]] : 0;
    int b = (i + 1 < len && encoded[i + 1] != '=')
                ? decode_table[(int)encoded[i + 1]]
                : 0;
    int c = (i + 2 < len && encoded[i + 2] != '=')
                ? decode_table[(int)encoded[i + 2]]
                : 0;
    int d = (i + 3 < len && encoded[i + 3] != '=')
                ? decode_table[(int)encoded[i + 3]]
                : 0;

    // Combine into 24 bits
    unsigned int triple = (a << 18) | (b << 12) | (c << 6) | d;

    // Extract 3 bytes
    if (j < max_output_len)
      output[j++] = (triple >> 16) & 0xFF;
    if (j < max_output_len && encoded[i + 2] != '=')
      output[j++] = (triple >> 8) & 0xFF;
    if (j < max_output_len && encoded[i + 3] != '=')
      output[j++] = triple & 0xFF;
  }

  return j; // Return number of bytes decoded
}