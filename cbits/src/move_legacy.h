#pragma once

#include "board.h"
#include "move.h"

int get_king_move_count(const board b);

void get_team_moves_black(
    const board current, int *total, move *moves, board *boards);

void get_team_moves_white(
    const board current, int *total, move *moves, board *boards);

void get_king_moves(
    const board current, int *total, move *moves, board *boards);

void init_move_globals();

layer find_capture_destinations(
    const layer allies, const layer foes, const layer occ);

void gen_reference_moves_black3(const board b, int *total, move *ms, board *bs);

void gen_reference_moves_white3(const board b, int *total, move *ms, board *bs);

uint16_t get_team_move_count(
    const layer occ, const layer team, const layer occ_r, const layer team_r);
