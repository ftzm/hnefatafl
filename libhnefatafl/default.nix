{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  pname = "libhnefatafl";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = with pkgs; [
    gcc
  ];

  makeFlags = [ "static" ];

  preBuild = ''
    export STATIC_LIB_DIR="$out/lib"
    mkdir -p $out/lib
  '';

  installPhase = ''
    mkdir -p $out/include
    cp src/*.h $out/include/
  '';

  meta = with pkgs.lib; {
    description = "C library for hnefatafl game logic";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}