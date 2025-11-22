#!/usr/bin/env bash

# Format all C and header files in libhnefatafl directories
find libhnefatafl/src libhnefatafl/bin libhnefatafl/test -name "*.c" -o -name "*.h" | xargs clang-format -i