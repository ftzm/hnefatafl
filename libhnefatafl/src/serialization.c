#include "serialization.h"
#include "base64.h"
#include "stdlib.h"
#include "string.h"

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