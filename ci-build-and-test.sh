#!/usr/bin/env bash
set -euo pipefail

print_header() {
    echo 
    echo "================================================================================"
    echo "$1"
    echo "================================================================================"
    echo 
}

print_header "Building libhnefatafl"
if nix path-info .#libhnefatafl-all >/dev/null 2>&1; then
    echo "cached"
else
    nix build .#libhnefatafl-all --print-build-logs
fi

print_header "Running libhnefatafl tests"
nix run .#test-libhnefatafl

print_header "Building Haskell bindings"
if nix path-info .#"hnefatafl:lib:bindings" >/dev/null 2>&1; then
    echo "cached"
else
    nix build .#"hnefatafl:lib:bindings" --print-build-logs
fi

print_header "Running Haskell tests"
nix run .#test-bindings
