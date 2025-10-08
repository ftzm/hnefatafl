{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { sumtypes = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "derive-storable"; version = "0.3.1.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "mateusz.p.kloczko@gmail.com";
      author = "Mateusz Kloczko";
      homepage = "https://www.github.com/mkloczko/derive-storable/";
      url = "";
      synopsis = "Derive Storable instances with GHC.Generics.";
      description = "Derive Storable instances with GHC.Generics. The derived Storable instances have the same alignment as C structs.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
      tests = {
        "c_alignment" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/derive-storable-0.3.1.0.tar.gz";
      sha256 = "4c337451af8309e2597c388b734d4c4837bd2117438fcf947d285d8faf90447f";
    });
  }) // {
    package-description-override = "name:                derive-storable\n\nversion:             0.3.1.0\nsynopsis:            Derive Storable instances with GHC.Generics.\n\ndescription:         Derive Storable instances with GHC.Generics. The derived Storable instances have the same alignment as C structs.\n\nhomepage:            https://www.github.com/mkloczko/derive-storable/\nlicense:             MIT\n\nlicense-file:        LICENSE\n\nauthor:              Mateusz Kloczko\n\nmaintainer:          mateusz.p.kloczko@gmail.com\ncategory:            Foreign\n\nbuild-type:          Simple\n\nextra-source-files:  ChangeLog.md README.md\n\ncabal-version:       >=1.10\ntested-with:         GHC==8.2.2, GHC==8.4.2, GHC==8.6.4, GHC==8.8.1, GHC==8.10.4, GHC==9.0.1, GHC==9.2.1\n\nFlag sumtypes\n  Description:   Enable support for non-recursive sum types.\n  Default:       False\n\nlibrary\n  exposed-modules:     Foreign.Storable.Generic \n                     , Foreign.Storable.Generic.Internal\n                     , Foreign.Storable.Generic.Tools\n                     , Foreign.Storable.Generic.Tools.TypeFuns\n  build-depends:       base >=4.8 && < 5\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  if flag(sumtypes)\n    cpp-options: -DGSTORABLE_SUMTYPES\n\nbenchmark benchmark\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      benchmark/\n  default-language:    Haskell2010\n  other-modules:       TestCases\n  Main-is:             Main.hs\n  build-depends:       base >= 4.8 && < 5, deepseq, criterion >= 1.1.0\n                    ,  derive-storable \n\ntest-suite c_alignment\n  type:                exitcode-stdio-1.0\n  \n  hs-source-dirs:      src, test/Basic, test/Basic/cbits\n  c-sources:           test/Basic/cbits/TestCases.c \n  main-is:             MemoryCSpec.hs\n  other-modules:       Foreign.Storable.Generic\n                     , Foreign.Storable.Generic.Internal\n                     , Foreign.Storable.Generic.Tools\n                     , Foreign.Storable.Generic.Tools.TypeFuns\n                     , TestCases \n  build-depends:       base >= 4.8 && < 5, hspec >= 2.4, QuickCheck >= 2.10\n  \n  default-language:    Haskell2010\n  if flag(sumtypes)\n    cpp-options: -DGSTORABLE_SUMTYPES\n\nsource-repository head\n  type:                git\n  location:            https://github.com/mkloczko/derive-storable\n  branch:              master\n\n";
  }