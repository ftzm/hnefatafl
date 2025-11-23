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

build_target "Haskell library" "backend.\"hnefatafl:lib:hnefatafl\""

print_header "Running Haskell tests"
nix run .#backend.\"hnefatafl:test:hnefatafl-test\"
