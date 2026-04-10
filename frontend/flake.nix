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
              mv openapi.json asyncapi.json ../backend/
              cd ../frontend
              npm run generate:types
            '';
          }}/bin/generate-types";
        };
        packages = {
          lint = pkgs.stdenv.mkDerivation {
            name = "lint-frontend";
            src = frontendSrc;
            nativeBuildInputs = [pkgs.biome];
            buildPhase = ''
              biome check src
            '';
            installPhase = "touch $out";
          };
          typecheck = pkgs.stdenv.mkDerivation {
            name = "typecheck-frontend";
            src = frontendSrc;
            nativeBuildInputs = [nodejs pkgs.importNpmLock.hooks.linkNodeModulesHook];
            npmDeps = nodeModules;
            buildPhase = ''
              npx tsc --noEmit
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
              cp ${apiSpecs}/asyncapi.json ../backend/

              # Generate types
              npx openapi-typescript ../backend/openapi.json -o src/api/generated/rest.ts
              node scripts/generate-ws-types.mjs

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
