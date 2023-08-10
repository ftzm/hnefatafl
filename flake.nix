{
  description = "Simple haskell nix flake";

  nixConfig.bash-prompt = "[nix]\\e[38;5;172mλ \\e[m";

  inputs = {
    nixpkgs.url = "nixpkgs";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = { };
        overlays = [ ];
        pkgs = import nixpkgs { inherit config overlays system; };
      in rec {
        devShell = pkgs.haskell.packages.ghc945.shellFor {
          packages = p: [ ];

          buildInputs = with pkgs.haskell.packages.ghc945; [
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
            pkgs.gdb
            pkgs.sqlite
          ];

          withHoogle = true;
        };
      });
}
