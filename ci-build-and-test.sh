#!/usr/bin/env bash
set -euo pipefail

echo "Building libhnefatafl..."
nix build .#libhnefatafl.all --print-build-logs

echo "Running libhnefatafl tests..."
nix run .#test-libhnefatafl

echo "Building Haskell bindings..."
nix build .#"hnefatafl:lib:bindings" --print-build-logs

echo "Running Haskell tests..."
nix run .#"hnefatafl:test:bindings-test"

echo "All builds and tests completed successfully!"