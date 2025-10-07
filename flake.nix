{
  description = "Hnefatafl game implementation with C library";

  nixConfig = {
    bash-prompt = "[nix]\\e[38;5;172mλ \\e[m";

    # Binary Cache for haskell.nix
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    extra-substituters = [
      "https://cache.iog.io"
    ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (
      system: let
        cabalProject = builtins.readFile ./backend/cabal.project;
        index-state =
          pkgs.haskell-nix.haskellLib.parseIndexState cabalProject;
        libhnefatafl = pkgs.callPackage ./libhnefatafl/default.nix {};
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            # This overlay adds our project to pkgs
            backend = final.haskell-nix.project' {
              inherit index-state;
              src = ./backend/.;
              compiler-nix-name = "ghc910";

              modules = [
                {
                  packages.hnefatafl.components.tests.bindings-test = {
                    libs = [libhnefatafl];
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
                };
                # Non-Haskell shell tools go here
                buildInputs = with pkgs; [
                  bashInteractive
                  zlib
                  fourmolu
                  hpack
                  (pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
                    exec haskell-language-server $@
                  '')
                ];
              };
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };
        flake = pkgs.backend.flake {};
      in
        flake
        // rec {
          packages = {
            libhnefatafl = libhnefatafl;
          };
          devShells = {
            libhnefatafl = pkgs.mkShell rec {
              buildInputs = with pkgs; [
                # first and foremost: a tolerable shell
                bashInteractive
                # C development tools
                clang-tools
                clang
                libclang
                bear
                cmake
                clangStdenv
                gdb
                valgrind
                just
                ripgrep
                # Additional utilities
                python312Packages.cogapp
              ];
              shellHook = ''
                export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
              '';
            };
            backend = pkgs.backend.shellFor {};
          };
        }
    );
}
