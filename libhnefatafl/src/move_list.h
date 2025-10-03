#include "move.h"
#include "board.h"
#include "position_set.h"
#include "stdbool.h"

// Move list functions (using separate variables instead of struct)
void move_list_to_base64(const move *moves, int count, char *output);
move *move_list_from_base64(const char *base64_string, int *count);

// Board state reconstruction from move history
int board_state_from_move_list(const move *moves, int count, board **b_ptr, position_set **ps_ptr, bool *is_black_turn);

// Simple pedagogical base64 implementation
int base64_encoded_size(int input_len);
void base64_encode(const char *data, int data_len, char *output);
int base64_decode(const char *encoded, char *output, int max_output_len);