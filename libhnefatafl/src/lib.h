/* public-facing interface for this library. It's designed be ergononmic when
 * used via Haskell's FFI.
 */

#include "board.h"
#include "layer.h"
#include "move.h"
#include "move_list.h"
#include "position_set.h"
#include "stdbool.h"
#include "stdlib.h"
#include "util.h"
#include "x86intrin.h" // IWYU pragma: export

typedef struct {
  layer black;
  layer white;
  u8 king;
} compact_board;

/* Function declarations */
compact_board to_compact(const board *b);
board from_compact(compact_board *b);
void start_board_extern(compact_board *b);

/* Get all moves for white player (white pieces + king).
 * Returns dynamically allocated array of moves and sets move_count.
 * Caller must free the returned array.
 */
move *all_white_and_king_moves(board b, position_set *ps, int *total);

/* Get the next game state based on the previously played moves. If the game is
 * ongoing we return an "ongoing" status and the available moves for the
 * position. If the last move trigger a victory then we indicate the nature of
 * the victory in the status type. Returns dynamically allocated array of moves
 * and sets move_count. Caller must free the returned array. Returns NULL on
 * error.
 */
move *next_game_state(
    const move *move_history,
    int history_count,
    int *move_count,
    game_status *gs);

/* Validate if a move is legal on the given board for the given team.
 * Returns true if the move is valid, false otherwise.
 */
bool validate_move(board b, move m, bool is_black_turn);

/* Helper functions for move validation - exposed for testing */
piece_type get_piece_at(board b, u8 position);
bool is_valid_destination(board b, u8 position, piece_type moving_piece);
bool is_correct_piece(bool is_black_turn, piece_type piece);
bool is_orthogonal_move(move m);
dir get_move_direction(move m);
int get_step_for_direction(dir direction);
layer draw_line_between(move m);
bool has_clear_path(board b, move m);
