/* public-facing interface for this library. It's designed to be ergononmic when
 * used via Haskell's FFI.
 */

#include "stdbool.h"
#include "types.h"
#include "util.h"
#include "x86intrin.h" // IWYU pragma: export

typedef struct transposition_table transposition_table;

typedef struct {
  move move;
  compact_board updated_board;
  layer captures;
  u64 updated_zobrist_hash;
  game_status status;
} search_trusted_result;

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
    move_with_captures **moves_out,
    int *move_count,
    game_status *gs,
    move_validation_result *validation_out,
    bool allow_repetition,
    move_result *last_move_out);

/* Validate and apply a single move to a trusted board state. Returns the move
 * result (board, captures, hash), game status, and valid moves for the next
 * turn. The board state and zobrist hash history are trusted (not recomputed).
 * The move itself is validated. Sets moves_out to dynamically allocated array.
 * Caller must free it. Returns 0 on success, non-zero move_error on failure.
 */
int next_game_state_with_moves_trusted(
    compact_board *trusted_board,
    bool is_black_turn,
    move *m,
    u64 *zobrist_hashes,
    int hash_count,
    move_result *result_out,
    game_status *status_out,
    move_with_captures **moves_out,
    int *move_count);

/* Apply a sequence of moves and return detailed data about each move.
 * Assumes moves are valid - does not perform move validation.
 * For each move, records the resulting board state, captures, and zobrist hash.
 * After applying all moves, checks for victory conditions.
 * Returns dynamically allocated array of move_result structures.
 * Writes the final game status to final_status_out.
 * Caller must free the returned array.
 */
move_result *apply_move_sequence(
    const move *moves,
    int move_count,
    game_status *final_status_out);

/* Perform search from a trusted board state with zobrist hash history.
 * Writes the best move, updated board state, captures, updated zobrist hash,
 * and game status to the provided output parameters. The should_stop flag can
 * be set by external code to cancel the search.
 * If enable_administrative_endings is true, checks for draw/resign conditions
 * before searching and may resign or offer a draw.
 * Returns 0 on success, non-zero if the search produced no legal move
 * (output parameters are undefined in that case). A non-zero return is an
 * invariant violation in normal play: either the board was already terminal
 * or the caller built a malformed history.
 */
int search_trusted(
    compact_board *trusted_board,
    bool is_black_turn,
    u64 *zobrist_hashes,
    int hash_count,
    _Atomic bool *should_stop,
    transposition_table *tt,
    move *move_out,
    compact_board *board_out,
    layer *captures_out,
    u64 *hash_out,
    game_status *status_out,
    bool enable_administrative_endings);
