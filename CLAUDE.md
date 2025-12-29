# Hnefatafl Multi-Project Repository - Development Guide for Claude

## Project Overview
This is a multi-project repository for a Hnefatafl (Viking board game) implementation consisting of:
- **libhnefatafl** (C library) - Core game engine and logic
- **backend** (Haskell) - Web API server that uses the C library

## Development Philosophy
- **Separation of Concerns**: Game logic in C, web API in Haskell
- **Performance**: Critical game calculations in optimized C
- **Type Safety**: Business logic and web API in Haskell
- **Integration**: Haskell backend links against C library

## Repository Structure
```
├── libhnefatafl/     # C library project (game engine)
├── backend/          # Haskell project (web API)
├── flake.nix         # Root Nix development environment
├── ci-build-and-test.sh  # CI/CD script for both projects
└── format.sh         # Code formatting for both projects
```

## Build and Development Workflow

### Prerequisites
- Use Nix development environment: `nix develop`
- Both projects must build successfully for integration

### Build Order (Important!)
1. **First**: Build libhnefatafl C library
2. **Second**: Build backend (depends on libhnefatafl.a)

### Root-Level Commands
- **Format all code**: `./format.sh`
- **CI build and test**: `./ci-build-and-test.sh`
- **Development shell**: `nix develop`

### Project-Specific Development
Each sub-project has its own CLAUDE.md with specific instructions:
- See `libhnefatafl/CLAUDE.md` for C library development
- See `backend/CLAUDE.md` for Haskell backend development

## Integration Points

### C Library → Haskell Backend
- Haskell backend links against `libhnefatafl.a`
- C library provides game logic functions
- Haskell provides web API and business logic

### Shared Considerations
- **Data Types**: Ensure C and Haskell data representations are compatible
- **Memory Management**: C library owns game state, Haskell manages API state
- **Error Handling**: C functions return status codes, Haskell translates to effects

## Development Guidelines for Claude

### When Working on Both Projects
1. **Always build libhnefatafl first** before building backend
2. **Run tests for both projects** to ensure integration works
3. **Use project-specific CLAUDE.md files** for detailed instructions
4. **Check CI script** (`ci-build-and-test.sh`) for complete build process

### Cross-Project Changes
- If changing C library API, update Haskell bindings accordingly
- Test both projects after cross-cutting changes
- Consider backward compatibility for API changes

### Git Workflow
- **Current branch**: `self-play`
- **Main branch**: `master`
- Both projects share the same git repository

## Troubleshooting

### Build Issues
- Ensure libhnefatafl builds first and produces `libhnefatafl.a`
- Check that Haskell can find the C library (linking errors)
- Verify Nix environment is active for consistent toolchain

### Integration Issues
- Verify C library exports expected functions
- Check Haskell FFI bindings match C signatures
- Test with simple C library calls before complex integration

## Quick Reference
- **C project**: `cd libhnefatafl && make`
- **Haskell project**: `cd backend && cabal build`
- **Full CI build**: `./ci-build-and-test.sh`
- **Code formatting**: `./format.sh`

## Important Notes
- **Never modify auto-generated files** (e.g., backend/hnefatafl.cabal)
- **Follow each project's conventions** as specified in their CLAUDE.md
- **Test integration** after changes to either project
- **Use Nix environment** for consistent builds across machines