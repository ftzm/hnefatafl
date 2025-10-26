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
        materializedSha = "1gj62f9k0a92as5i85ypa974kv2mm2m9x8dvn5lgp5qb1yn4kszf";
        cabalProject = builtins.readFile ./cabal.project;
        index-state =
          pkgs.haskell-nix.haskellLib.parseIndexState cabalProject;
        libhnefatafl = import ../libhnefatafl/default.nix {inherit pkgs;};
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            # This overlay adds our project to pkgs
            backend = final.haskell-nix.project' {
              inherit index-state;
              src = ./.;
              # evalSystem = "x86_64-linux";
              compiler-nix-name = "ghc910";
              # plan-sha256 = "sha256-7utJrA8Ll/tosbuhnqqoVexJTlLXFxSLViIpMJMTRr4=";
              materialized = ./materialized;

              modules = [
                {
                  packages.hnefatafl.components.tests.bindings-test = {
                    libs = [libhnefatafl.lib];
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
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };
        flake = pkgs.backend.flake {};

        # Import test caching utilities
        testCache = import ../nix/test-cache.nix {inherit pkgs;};

        # Create cached tests
        bindingsTest =
          testCache.mkCachedTest
          "${flake.packages."hnefatafl:test:bindings-test"}/bin/bindings-test"
          "bindings";

        update-materialized = pkgs.writeShellScript "update-all-materialized-${system}" ''
          set -eEuo pipefail
          echo "Updating project materialization" >&2
          current_sha=$(${pkgs.backend.plan-nix.passthru.calculateMaterializedSha})
          echo "saved sha:"
          echo "${materializedSha}"
          echo "current sha:"
          echo "$current_sha"
          rm -rf materialized
          cp -r ${pkgs.backend.plan-nix} materialized
          chmod -R +w materialized
        '';
      in
        pkgs.lib.attrsets.recursiveUpdate
        flake
        rec {
          apps = {
            update-materialized = {
              type = "app";
              program = update-materialized.outPath;
            };
            test-bindings = bindingsTest.app;
          };
          hooks = {
            materialization = {
              enable = true;
              name = "materialization";
              entry = "${apps.update-materialized.program}";
              pass_filenames = false;
              always_run = true;
            };
          };
          # devShells = {
          #   backend = let
          #     inherit (self.checks.${system}.pre-commit-check) shellHook enabledPackages;
          #   in
          #     pkgs.backend.shellFor {
          #       inherit shellHook;
          #       buildInputs = enabledPackages ++ [pkgs.pre-commit];
          #     };
          # };
          # Run the hooks in a sandbox with `nix flake check`.
          # Read-only filesystem and no internet access.
          checks = {
            pre-commit-check = git-hooks.lib.${system}.run {
              src = ./.;
              inherit hooks;
            };
          };
        }
    );
}
