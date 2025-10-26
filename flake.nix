{
  description = "Hnefatafl game implementation with C library";

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
    nixpkgs.follows = "backend/haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks.url = "github:cachix/git-hooks.nix";
    backend.url = "./backend";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    git-hooks,
    backend,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
        libhnefatafl = pkgs.callPackage ./libhnefatafl/default.nix {};
      in rec {
        packages = {
          libhnefatafl = libhnefatafl;
        };
        devShells = {
          default = pkgs.mkShell {fromInputs = [libhnefatafl.devShell backend.devShells.${system}.default];};
        };
        # Run the hooks in a sandbox with `nix flake check`.
        # Read-only filesystem and no internet access.
        checks = {
          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = backend.hooks;
          };
        };
      }
    );
}
