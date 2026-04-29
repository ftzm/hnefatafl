{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    backend.url = "../backend";
  };
  outputs = { nixpkgs, flake-utils, backend, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        nodejs = pkgs.nodejs;
        frontendSrc = pkgs.lib.sources.sourceByRegex ./. [
          "src"
          "src/.*"
          "scripts"
          "scripts/.*"
          "tsconfig.json"
          "biome.json"
          "vite.config.ts"
          "vitest.config.ts"
          "package.json"
          "package-lock.json"
        ];
        nodeModules = pkgs.importNpmLock.buildNodeModules {
          npmRoot = ./.;
          inherit nodejs;
        };
        apiSpecs = backend.packages.${system}.api-specs;
        dumpSpecsProgram = backend.apps.${system}."hnefatafl:exe:dump-specs".program;
      in {
        apps.generate-types = {
          type = "app";
          program = "${pkgs.writeShellApplication {
            name = "generate-types";
            runtimeInputs = [nodejs];
            text = ''
              ${dumpSpecsProgram}
              mv openapi.json ../backend/
              cd ../frontend
              npm run generate:types
            '';
          }}/bin/generate-types";
        };
        packages = {
          # Mirrors `npm run check` from package.json (lint + typecheck + tests),
          # but uses pkgs.biome directly because importNpmLock doesn't
          # materialise biome's platform-specific optional native binary.
          # Keep this list in sync with package.json's `check` script.
          check = pkgs.stdenv.mkDerivation {
            name = "check-frontend";
            src = frontendSrc;
            nativeBuildInputs = [nodejs pkgs.biome pkgs.importNpmLock.hooks.linkNodeModulesHook];
            npmDeps = nodeModules;
            # linkNodeModulesHook prepends node_modules/.bin to PATH, which
            # contains a biome wrapper that fails (see comment above). Invoke
            # pkgs.biome by absolute path to bypass it.
            buildPhase = ''
              ${pkgs.biome}/bin/biome check src
              npx tsc --noEmit
              npx vitest run
            '';
            installPhase = "touch $out";
          };
          check-generated-types = pkgs.stdenv.mkDerivation {
            name = "check-generated-types";
            src = frontendSrc;
            nativeBuildInputs = [nodejs pkgs.importNpmLock.hooks.linkNodeModulesHook];
            npmDeps = nodeModules;
            buildPhase = ''
              # Save committed generated types
              cp -r src/api/generated committed-generated

              # Set up expected directory structure for spec references
              mkdir -p ../backend
              cp ${apiSpecs}/openapi.json ../backend/

              # Generate types
              npx openapi-typescript ../backend/openapi.json -o src/api/generated/rest.ts

              # Diff against committed versions
              diff -r src/api/generated/ committed-generated/ || {
                echo "ERROR: Generated types are out of date. Run 'nix run .#generate-types' and commit the result."
                exit 1
              }
            '';
            installPhase = "touch $out";
          };
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.nodejs
            pkgs.biome
          ];
          shellHook = ''
            export PLAYWRIGHT_BROWSERS_PATH="${pkgs.playwright-driver.browsers}"
          '';
        };
      });
}
