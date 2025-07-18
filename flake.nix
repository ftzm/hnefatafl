{
  description = "Simple haskell nix flake";

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
      devShell = pkgs.haskell.packages.ghc945.shellFor rec {
        packages = p: [];

        buildInputs = with pkgs.haskell.packages.ghc945; [
          # first and foremost: a tolerable shell
          pkgs.bashInteractive

          cabal-install

          # Helpful tools for `nix develop` shells
          #
          #ghcid                   # https://github.com/ndmitchell/ghcid
          haskell-language-server # https://github.com/haskell/haskell-language-server
          # haskell-langauge-server-wrapper
          (pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
            exec haskell-language-server $@
          '')

          #hlint                   # https://github.com/ndmitchell/hlint
          #ormolu                  # https://github.com/tweag/ormolu
          fourmolu
          hpack
          pkgs.zlib
          pkgs.libz
          pkgs.gdb
          pkgs.sqlite

          # C
          pkgs.clang-tools
          pkgs.clang
          pkgs.libclang
          #pkgs.ccls
          pkgs.bear
          pkgs.cmake
          pkgs.clangStdenv
          pkgs.just
          pkgs.ripgrep

          pkgs.python312Packages.cogapp
        ];

        withHoogle = true;

        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
        '';
      };
    });
}
