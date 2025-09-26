{
  description = "C library for hnefatafl game logic";

  nixConfig.bash-prompt = "[nix]\\e[38;5;172mλ \\e[m";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    flake-utils,
    nixpkgs,
    self,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      config = {};
      overlays = [];
      pkgs = import nixpkgs {inherit config overlays system;};
    in rec {
      devShell = pkgs.mkShell rec {
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

          # Build tools
          just
          ripgrep

          # Additional utilities
          python312Packages.cogapp
        ];

        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
        '';
      };
    });
}