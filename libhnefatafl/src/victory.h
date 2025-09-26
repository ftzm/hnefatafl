#include "board.h"
#include "stdbool.h"

bool king_capture_check_ref(const board *b);
bool king_capture_check(const board *b);
bool king_escaped(const board *b);
bool king_effectively_escaped(const board *b);
bool king_captured(const board *b);
bool surrounded(const board *b);
bool exit_fort(const board *b);
bool black_victory(const board *b);
bool white_victory(const board *b);
