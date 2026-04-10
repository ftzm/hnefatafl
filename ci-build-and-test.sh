#!/usr/bin/env bash
set -euo pipefail

print_header() {
    echo
    echo "================================================================================"
    echo "$1"
    echo "================================================================================"
    echo
}

build_target() {
    local nix_target="$1"

    if nix path-info .#"$nix_target" >/dev/null 2>&1; then
        echo "cached"
    else
        nix build .#"$nix_target" #--print-build-logs
    fi
}

print_header "Building libhnefatafl"
build_target "libhnefatafl-all"

print_header "Running libhnefatafl tests"
nix run .#test-libhnefatafl

print_header "Building Haskell library"
build_target "backend.\"hnefatafl:lib:hnefatafl\""

print_header "Running Haskell tests"
nix run .#test-backend

print_header "Running frontend lint"
build_target "lint"

print_header "Running frontend typecheck"
build_target "typecheck"

print_header "Checking generated frontend types"
build_target "check-generated-types"
