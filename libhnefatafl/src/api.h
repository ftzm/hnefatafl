/* public-facing interface for this library. It's designed be ergononmic when
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

/* Function declarations */
compact_board to_compact(const board *b);
board from_compact(compact_board *b);
void start_board_extern(compact_board *b);

/* Get all moves for white player (white pieces + king).
 * Returns dynamically allocated array of moves and sets move_count.
 * Caller must free the returned array.
 */
// move *all_white_and_king_moves(board b, position_set *ps, int *total);

/* Get the game status after applying the move history.
 * Returns 0 on success, non-zero error code on failure.
 */
int next_game_state(
    const move *move_history,
    int history_count,
    game_status *gs);

/* Get the next game state based on the previously played moves. If the game is
 * ongoing we return an "ongoing" status and the available moves for the
 * position. If the last move triggered a victory then we indicate the nature of
 * the victory in the status type. Sets moves_out to dynamically allocated array
 * of moves and sets move_count. Caller must free the returned array.
 * Returns 0 on success, non-zero error code on failure.
 */
int next_game_state_with_moves(
    const move *move_history,
    int history_count,
    move **moves_out,
    int *move_count,
    game_status *gs);

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
