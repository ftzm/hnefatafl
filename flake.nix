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

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-hooks,
      backend,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        libhnefatafl = pkgs.callPackage ./libhnefatafl/default.nix { };
      in
      {
        packages = {
          libhnefatafl = libhnefatafl.static;
          inherit (backend.packages.${system}) "hnefatafl:lib:bindings";
        };
        apps = {
          test-libhnefatafl = libhnefatafl.test.app;
          inherit (backend.apps.${system}) test-bindings;
        };
        devShells = {
          default =
            let
              inherit (self.checks.${system}.pre-commit-check) shellHook; # enabledPackages;
            in
            pkgs.mkShell {
              inputsFrom = [
                libhnefatafl.devShell
                backend.devShells.${system}.default
              ];
              # buildInputs = enabledPackages;
              inherit shellHook;
            };
        };
        # Run the hooks in a sandbox with `nix flake check`.
        # Read-only filesystem and no internet access.
        checks = {
          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks =
              libhnefatafl.hooks
              // backend.hooks.${system}
              // {
                no-commit-to-branch = {
                  enable = true;
                  args = [
                    "--branch"
                    "master"
                  ];
                };
                check-merge-conflicts.enable = true;
                commitizen.enable = true;
                ci-build-and-test = {
                  enable = true;
                  name = "ci-build-and-test";
                  entry = "${pkgs.bash}/bin/bash";
                  args = [
                    "-c"
                    ''
                      # Check if we're merging to master
                      target_branch=$(git rev-parse --abbrev-ref HEAD)
                      if [ "$target_branch" = "master" ]; then
                        echo "Detected merge to master branch, running CI build and test..."
                        exec ${pkgs.bash}/bin/bash ./ci-build-and-test.sh
                      fi
                    ''
                  ];
                  stages = [ "pre-merge-commit" ];
                };
              };
          };
        };
      }
    );
}
