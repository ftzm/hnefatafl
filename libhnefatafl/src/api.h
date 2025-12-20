/* public-facing interface for this library. It's designed to be ergononmic when
 * used via Haskell's FFI.
 */

#include "stdbool.h"
#include "types.h"
#include "util.h"
#include "x86intrin.h" // IWYU pragma: export

typedef struct {
  layer black;
  layer white;
  u8 king;
} compact_board;

typedef struct {
  move move;
  compact_board board;
  layer captures;
  bool was_black_turn;
} move_result;

compact_board to_compact(const board *b);
board from_compact(compact_board *b);
void start_board_extern(compact_board *b);

/* Get all moves for white player (white pieces + king).
 * Returns dynamically allocated array of moves and sets move_count.
 * Caller must free the returned array.
 */
// move *all_white_and_king_moves(board b, position_set *ps, int *total);

/* Get the game status after applying the move history.
 * Writes validation result to validation_out.
 */
void next_game_state(
    const move *move_history,
    int history_count,
    game_status *gs,
    move_validation_result *validation_out,
    bool allow_repetition);

/* Get the next game state based on the previously played moves. If the game is
 * ongoing we return an "ongoing" status and the available moves for the
 * position. If the last move triggered a victory then we indicate the nature of
 * the victory in the status type. Sets moves_out to dynamically allocated array
 * of moves and sets move_count. Caller must free the returned array.
 * Writes validation result to validation_out.
 */
void next_game_state_with_moves(
    const move *move_history,
    int history_count,
    move **moves_out,
    int *move_count,
    game_status *gs,
    move_validation_result *validation_out,
    bool allow_repetition);

/* Get the next game state with moves using a trusted board state as input.
 * This function doesn't reconstruct the board state from move history but
 * accepts it as input. The move history is only used to construct a position
 * set for tracking threefold repetition. Sets moves_out to dynamically
 * allocated array of moves and sets move_count. Caller must free the returned
 * array. Returns 0 on success, non-zero error code on failure.
 */
int next_game_state_with_moves_trusted(
    compact_board *trusted_board,
    bool is_black_turn,
    const move *move_history,
    int history_count,
    move **moves_out,
    int *move_count,
    game_status *gs);

/* Apply a sequence of moves and return detailed data about each move.
 * Does not perform move validation or game state logic - just applies moves
 * and captures. Returns dynamically allocated array of move_result structures.
 * Caller must free the returned array.
 */
move_result *apply_move_sequence(const move *moves, int move_count);
