# Hnefatafl

Viking board game (11x11, Hnefatafl Copenhagen rules). Three sub-projects:

- **libhnefatafl/** — C game engine (bitboard-based, with AI search)
- **backend/** — Haskell web API server (Servant + Effectful, links against C library)
- **frontend/** — SolidJS TypeScript UI (Vite, currently uses mock backends)

Each has its own `CLAUDE.md` with project-specific details.

## Game Rules

Copenhagen Hnefatafl rules are documented in `RULES.md`. Read that file when working on game logic, captures, victory conditions, or anything rule-related.

## Build Order

libhnefatafl must be built first — the backend links against `libhnefatafl.a`.

```
cd libhnefatafl && make static   # produces .lib/libhnefatafl.a
cd backend && cabal build        # links against the static lib
cd frontend && npm install && npm run dev
```

## Root Commands

| Command | What it does |
|---|---|
| `nix develop` | Enter dev shell (C + Haskell + Python tools) |
| `./ci-build-and-test.sh` | Full CI: build + test C lib, build + test backend, lint + typecheck frontend |
| `./format.sh` | `clang-format` all C sources (`src/`, `bin/`, `test/`) |
| `./build-release.sh` | Build versioned CLI binary to `dist/<version>/hnefatafl` |
| `./test-engine.sh [version]` | Self-play current engine vs previous version for regression testing |

## CI Pipeline (`ci-build-and-test.sh`)

Runs in order: C library build → C tests → Haskell build → Haskell tests → frontend `npm ci` → Biome lint → TypeScript typecheck.

## Git Conventions

- Main branch: `master`
- Pre-commit hooks (via Nix `git-hooks`): no direct commits to master, merge conflict check, commitizen conventional commit format
- On merge to master: full CI runs automatically

## Cross-Project Changes

When changing the C library's API (`api.h`):
1. Update the FFI bindings in `backend/src/Hnefatafl/Bindings.hs`
2. Update corresponding `Storable*` types and `DomainMapping` instances
3. Run both `make test` (in libhnefatafl) and `cabal test` (in backend)

## Analysis Tools

`analysis/fit_pst.py` — learns piece-square table weights from a game database via logistic regression. Requires numpy, scikit-learn, matplotlib (available in the Nix dev shell).

```
python analysis/fit_pst.py stats <db>          # game statistics
python analysis/fit_pst.py fit <db> --plot      # fit PSTs, generate heatmaps
```

## Testing Requirements

All tests must pass before work is considered complete. Run the full suite:
```
cd libhnefatafl && make test
cd backend && cabal test
cd frontend && npm test
```
