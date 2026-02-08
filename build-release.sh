#!/usr/bin/env bash
set -euo pipefail

version=$(nix eval '.#packages.x86_64-linux.backend."hnefatafl:exe:cli".identifier.version' --raw)

echo "Building hnefatafl version $version"

nix build '.#packages.x86_64-linux.backend."hnefatafl:exe:cli"'

mkdir -p "dist/$version"
cp result/bin/cli "dist/$version/hnefatafl"

echo "Built to $version/hnefatafl"
