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
    local target_name="$1"
    local nix_target="$2"

    print_header "Building $target_name"
    if nix path-info .#"$nix_target" >/dev/null 2>&1; then
        echo "cached"
    else
        nix build .#"$nix_target" #--print-build-logs
    fi
}

build_target "libhnefatafl" "libhnefatafl-all"

print_header "Running libhnefatafl tests"
nix run .#test-libhnefatafl

build_target "Haskell core-data" "backend.hnefatafl:lib:core-data"
build_target "Haskell storage" "backend.hnefatafl:lib:storage"
build_target "Haskell storage-sqlite" "backend.hnefatafl:lib:storage-sqlite"
build_target "Haskell bindings" "backend.hnefatafl:lib:bindings"

print_header "Running Haskell bindings tests"
nix run .#backend.hnefatafl:test:bindings-test

print_header "Running Haskell storage tests"
nix run .#backend.hnefatafl:test:storage-test
