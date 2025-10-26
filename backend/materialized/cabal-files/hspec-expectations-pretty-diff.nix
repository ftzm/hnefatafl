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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "hspec-expectations-pretty-diff";
        version = "0.7.2.6";
      };
      license = "MIT";
      copyright = "(c) 2011-2015 Simon Hengel";
      maintainer = "greg@unrelenting.technology";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "https://github.com/myfreeweb/hspec-expectations-pretty-diff#readme";
      url = "";
      synopsis = "Catchy combinators for HUnit";
      description = "Catchy combinators for HUnit: <https://github.com/myfreeweb/hspec-expectations-pretty-diff#readme>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."nicify-lib" or (errorHandler.buildDepError "nicify-lib"))
          (hsPkgs."hscolour" or (errorHandler.buildDepError "hscolour"))
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."unicode-show" or (errorHandler.buildDepError "unicode-show"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec-expectations-pretty-diff" or (errorHandler.buildDepError "hspec-expectations-pretty-diff"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-expectations-pretty-diff-0.7.2.6.tar.gz";
      sha256 = "8954f322a53c5755fe182f606ad13dbb1c2e1c53958fb7c81dcc8322db7e7a72";
    });
  }) // {
    package-description-override = "name:             hspec-expectations-pretty-diff\r\nversion:          0.7.2.6\r\nx-revision: 1\r\nsynopsis:         Catchy combinators for HUnit\r\ndescription:      Catchy combinators for HUnit: <https://github.com/myfreeweb/hspec-expectations-pretty-diff#readme>\r\nbug-reports:      https://github.com/myfreeweb/hspec-expectations-pretty-diff/issues\r\nlicense:          MIT\r\nlicense-file:     LICENSE\r\ncopyright:        (c) 2011-2015 Simon Hengel\r\nauthor:           Simon Hengel <sol@typeful.net>\r\nmaintainer:       greg@unrelenting.technology\r\nbuild-type:       Simple\r\ncategory:         Testing\r\ncabal-version:    >= 1.10\r\nhomepage:         https://github.com/myfreeweb/hspec-expectations-pretty-diff#readme\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/myfreeweb/hspec-expectations-pretty-diff\r\n\r\nlibrary\r\n  hs-source-dirs:\r\n      src\r\n  ghc-options: -Wall\r\n  build-depends:\r\n      base == 4.*\r\n    , HUnit\r\n    , text\r\n    , nicify-lib\r\n    , hscolour\r\n    , Diff\r\n    , ansi-terminal\r\n    , unicode-show\r\n  exposed-modules:\r\n      Test.Hspec.Expectations.Pretty\r\n      Test.Hspec.Expectations.Pretty.Contrib\r\n      Test.Hspec.Expectations.Pretty.Matcher\r\n  default-language: Haskell2010\r\n\r\ntest-suite tests\r\n  build-depends:\r\n      base >= 4.0.0.0 && < 5\r\n    , hspec-expectations-pretty-diff\r\n    , HUnit >= 1.3.0.0\r\n    , aeson < 2\r\n    , text\r\n    , hspec\r\n  default-language: Haskell2010\r\n  ghc-options: -threaded -Wall\r\n  hs-source-dirs:\r\n      test\r\n  main-is: Spec.hs\r\n  other-modules:\r\n    Test.Hspec.Expectations.PrettySpec\r\n    Test.Hspec.Expectations.Pretty.MatcherSpec\r\n  type: exitcode-stdio-1.0\r\n";
  }