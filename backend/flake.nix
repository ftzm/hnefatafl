{
  description = "Haskell game server for hnefatafl";

  nixConfig = {
    # Binary Cache for haskell.nix
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    extra-substituters = [
      "https://cache.iog.io"
    ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
    git-hooks,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (
      system: let
        cabalProject = builtins.readFile ./cabal.project;
        index-state = pkgs.haskell-nix.haskellLib.parseIndexState cabalProject;
        libhnefatafl = import ../libhnefatafl/default.nix {inherit pkgs;};
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            # This overlay adds our project to pkgs
            backend = final.haskell-nix.project' {
              inherit index-state;
              src = ./.;
              # evalSystem = "x86_64-linux";
              compiler-nix-name = "ghc912";

              modules = [
                {
                  packages.hnefatafl.components.tests.hnefatafl-test = {
                    libs = [libhnefatafl.static];
                    # ghcOptions = ["-O1" "-Werror"];
                    # Don't depend on GHC in build artifacts.  Otherwise GHC may
                    # be pulled in as a dependency, which causes docker images to
                    # balloon in size.
                    dontStrip = false;
                  };
                }
              ];
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell = {
                tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                  fourmolu = {};
                };
                # Non-built-in shell tools go here
                buildInputs = with pkgs; [
                  bashInteractive
                  zlib
                  hpack
                  (writeShellScriptBin "haskell-language-server-wrapper" ''
                    exec haskell-language-server $@
                  '')
                  sqlite
                  dbmate
                ];
                shellHook = ''
                  export LD_LIBRARY_PATH="${pkgs.zlib}/lib:$LD_LIBRARY_PATH"
                  # Find backend directory from git root and prioritize local libhnefatafl.a
                  REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
                  BACKEND_DIR="$REPO_ROOT/backend"
                  export NIX_LDFLAGS="-L$BACKEND_DIR $NIX_LDFLAGS"
                  export LIBRARY_PATH="$BACKEND_DIR:$LIBRARY_PATH"
                '';
              };
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };
        flake = pkgs.backend.flake {};
      in
        pkgs.lib.attrsets.recursiveUpdate flake rec {
          hooks = {
          };
          checks = {
            pre-commit-check = git-hooks.lib.${system}.run {
              src = ./.;
              inherit hooks;
            };
          };
        }
    );
}
