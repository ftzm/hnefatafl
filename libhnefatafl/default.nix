{pkgs ? import <nixpkgs> {}}: let
  # Common attributes shared across all derivations
  commonAttrs = {
    version = "0.1.0";
    nativeBuildInputs = with pkgs; [gcc coreutils gawk];

    meta = with pkgs.lib; {
      description = "C library for hnefatafl game logic";
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };

  # Build object files once - this is the expensive compilation step
  objects = pkgs.stdenv.mkDerivation (commonAttrs
    // {
      pname = "libhnefatafl-objects";
      src = pkgs.lib.sources.sourceByRegex ./. ["Makefile" "src" "src/.*"];

      buildPhase = ''
        # Build only the library object files
        make lib-objs
      '';

      installPhase = ''
        mkdir -p $out
        cp -r .lib_obj $out/
      '';
    });
  # Theft property testing library - separate derivation for caching
  theft = pkgs.stdenv.mkDerivation (commonAttrs
    // {
      pname = "theft";
      src = pkgs.lib.sources.sourceByRegex ./vendor/theft [
        "Makefile"
        "src" "src/.*"
        "inc" "inc/.*"
        "scripts" "scripts/.*"
        "pc" "pc/.*"
      ];

      patchPhase = ''
        # Fix shebang in theft script
        substituteInPlace scripts/mk_bits_lut \
          --replace "#!/usr/bin/env -S awk -f" "#!${pkgs.gawk}/bin/awk -f"
      '';

      buildPhase = ''
        make build/libtheft.a LDFLAGS=-lm
      '';

      installPhase = ''
        mkdir -p $out/lib $out/include
        cp build/libtheft.a $out/lib/
        cp inc/*.h $out/include/
      '';
    });
in rec {
  inherit theft;
  # Shared library - reuses cached objects
  shared-lib = pkgs.stdenv.mkDerivation (commonAttrs
    // {
      pname = "libhnefatafl-shared";
      src = pkgs.lib.sources.sourceByRegex ./. ["Makefile" "src" "src/.*"];

      buildInputs = [objects];

      buildPhase = ''
        # Build shared library directly from cached objects
        make lib-only \
          LIB_OBJ_DIR=${objects}/.lib_obj
      '';

      installPhase = ''
        mkdir -p $out/lib $out/include
        cp .lib/libhnefatafl.so $out/lib/
        cp src/*.h $out/include/
      '';
    });

  # Static library - reuses cached objects
  lib = pkgs.stdenv.mkDerivation (commonAttrs
    // {
      pname = "libhnefatafl";
      src = pkgs.lib.sources.sourceByRegex ./. ["Makefile" "src" "src/.*"];

      buildInputs = [objects];

      buildPhase = ''
        # Build static library from cached objects
        mkdir -p $out/lib
        make static \
          LIB_OBJ_DIR=${objects}/.lib_obj \
          STATIC_LIB_DIR=$out/lib
      '';

      installPhase = ''
        mkdir -p $out/include
        cp src/*.h $out/include/
      '';
    });

  # Tests - depends on shared library and theft
  tests = pkgs.stdenv.mkDerivation (commonAttrs
    // {
      pname = "libhnefatafl-tests";
      src = pkgs.lib.sources.sourceByRegex ./. ["Makefile" "src" "src/.*" "test" "test/.*" "vendor" "vendor/greatest" "vendor/greatest/.*" "vendor/ubench.h" "vendor/ubench.h/.*"];

      buildInputs = [shared-lib theft];

      buildPhase = ''
        make .test_bin/test \
          LIB_TARGET="" \
          TEST_DEPS_TARGET="" \
          THEFT_LIB="${theft}/lib/libtheft.a" \
          THEFT_INCLUDES="-I${theft}/include" \
          LDFLAGS="-Wl,-rpath,${shared-lib}/lib"
      '';

      installPhase = ''
        mkdir -p $out/bin
        cp .test_bin/test $out/bin/libhnefatafl-test
      '';
    });

  # Benchmarks - depends on shared library
  benchmarks = pkgs.stdenv.mkDerivation (commonAttrs
    // {
      pname = "libhnefatafl-benchmarks";
      src = pkgs.lib.sources.sourceByRegex ./. ["Makefile" "src" "src/.*" "bench" "bench/.*" "vendor" "vendor/.*"];

      buildInputs = [shared-lib];

      buildPhase = ''
        make benchmarks \
          LIB_TARGET="" \
          LDFLAGS="-Wl,-rpath,${shared-lib}/lib"
      '';

      installPhase = ''
        mkdir -p $out/bin
        cp .bench_bin/* $out/bin/
      '';
    });
  # Convenience derivation that provides all components
  all = pkgs.buildEnv {
    name = "libhnefatafl-all";
    paths = [lib tests benchmarks];
  };
}
