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

print_header "Frontend: install dependencies"
(cd frontend && npm ci)

print_header "Frontend: lint and format check"
(cd frontend && npx biome check src)

print_header "Generating API spec files"
(cd backend && cabal run dump-specs)

print_header "Frontend: check generated types are up to date"
(cd frontend && npm run generate:types)
if ! git diff --exit-code frontend/src/api/generated/; then
    echo "ERROR: Generated types are out of date. Run 'cabal run dump-specs' in backend/ then 'npm run generate:types' in frontend/ and commit the result."
    exit 1
fi

print_header "Frontend: typecheck"
(cd frontend && npm run typecheck)
