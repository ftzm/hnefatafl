#pragma once

#include "types.h"

// Move list serialization functions
void move_list_to_base64(const move *moves, int count, char *output);
move *move_list_from_base64(const char *base64_string, int *count);