#pragma once

#include "layer.h"
#include "stdbool.h"

bool corner_moves_1(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file);

bool corner_moves_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    int *dests,
    int *total);

void corner_paths_1(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r);

void corner_paths_2(
    const layer occ,
    const layer occ_r,
    const int rank,
    const int file,
    layer *paths,
    layer *paths_r);
