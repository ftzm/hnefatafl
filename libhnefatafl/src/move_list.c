#include "move.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>

// Move list functions now use separate variables instead of struct

// Simple pedagogical base64 implementation
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

// Convert move array to base64 string
void move_list_to_base64(const move *moves, int count, char *output) {
  if (!moves || count == 0) {
    output[0] = '\0';
    return;
  }

  // Calculate size: moves data only (count * 2 bytes per move)
  int data_size = count * sizeof(move);

  // Encode moves directly to base64 (each move is 2 bytes: orig, dest)
  base64_encode((const char *)moves, data_size, output);
}

// Convert base64 string to move array (caller owns the returned moves)
move *move_list_from_base64(const char *base64_string, int *count) {
  *count = 0;

  if (!base64_string || base64_string[0] == '\0') {
    return NULL;
  }

  // Calculate maximum possible decoded size and number of moves
  int encoded_len = strlen(base64_string);
  int max_decoded_size = (encoded_len / 4) * 3;
  int max_move_count = max_decoded_size / sizeof(move);

  if (max_move_count == 0) {
    return NULL;
  }

  // We prefer to segfault on allocation error rather than fail silently
  move *moves = malloc(max_move_count * sizeof(move));

  // Decode directly into the move array
  int decoded_size =
      base64_decode(base64_string, (char *)moves, max_decoded_size);

  // Each move is 2 bytes, so count must be even
  if (decoded_size % sizeof(move) != 0) {
    free(moves);
    return NULL; // Return NULL on invalid data
  }

  // Set the actual count
  *count = decoded_size / sizeof(move);
  return moves;
}
