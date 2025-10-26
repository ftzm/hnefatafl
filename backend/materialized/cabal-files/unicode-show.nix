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
      identifier = { name = "unicode-show"; version = "0.1.1.1"; };
      license = "BSD-3-Clause";
      copyright = "2016 Takayuki Muranushi";
      maintainer = "igrep@n.email.ne.jp";
      author = "Takayuki Muranushi";
      homepage = "http://github.com/haskell-jp/unicode-show#readme";
      url = "";
      synopsis = "print and show in unicode";
      description = "This package provides variants of 'show' and 'print' functions that does not escape non-ascii characters.\n\nSee <http://github.com/haskell-jp/unicode-show#readme README> for usage.\n\nRun ghci with  @-interactive-print@ flag to print unicode characters. See <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/interactive-evaluation.html#ghci-interactive-print Using a custom interactive printing function> section in the GHC manual.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
        ];
        buildable = true;
      };
      tests = {
        "unicode-show-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."unicode-show" or (errorHandler.buildDepError "unicode-show"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unicode-show-0.1.1.1.tar.gz";
      sha256 = "f4afd53c0cb8485203b3f63fa6785ef1e8e43057b638cf277b3b745b79b83fea";
    });
  }) // {
    package-description-override = "name:                unicode-show\nversion:             0.1.1.1\nsynopsis:            print and show in unicode\ndescription:\n            This package provides variants of 'show' and 'print' functions that does not escape non-ascii characters.\n            .\n            See <http://github.com/haskell-jp/unicode-show#readme README> for usage.\n            .\n            Run ghci with  @-interactive-print@ flag to print unicode characters. See <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/interactive-evaluation.html#ghci-interactive-print Using a custom interactive printing function> section in the GHC manual.\n\n\n\n\nhomepage:            http://github.com/haskell-jp/unicode-show#readme\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Takayuki Muranushi\nmaintainer:          igrep@n.email.ne.jp\ncopyright:           2016 Takayuki Muranushi\ncategory:            Text\nbuild-type:          Simple\n-- extra-source-files:\ncabal-version:       >=1.10\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Text.Show.Unicode\n  build-depends:       base >= 4.8.1.0 && < 5\n                     , transformers >= 0.4.0.0\n                     , safe >= 0.3.5\n  default-language:    Haskell2010\n\ntest-suite unicode-show-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  build-depends:       base\n                     , hspec\n                     , QuickCheck\n                     , unicode-show\n\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-jp/unicode-show\n";
  }