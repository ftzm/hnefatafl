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
      specVersion = "3.8";
      identifier = { name = "ulid"; version = "0.3.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ulid@ad-si.com";
      author = "Steve Kollmansberger, Adrian Sieber";
      homepage = "https://github.com/ad-si/ulid";
      url = "";
      synopsis = "Implementation of ULID - Universally Unique\nLexicographically Sortable Identifier";
      description = "Implementation of Alizain Feerasta's ULID specification.\nA 26 character string identifier,\nas opposed to the 36 character UUID string.\nUses Douglas Crockford's base 32 encoding\nfor better efficiency and readability\n(5 bits per character).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."crypto-api" or (errorHandler.buildDepError "crypto-api"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
      };
      exes = {
        "ulid-exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."crypto-api" or (errorHandler.buildDepError "crypto-api"))
          ];
          buildable = true;
        };
      };
      tests = {
        "ulid-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "ulid-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."format-numbers" or (errorHandler.buildDepError "format-numbers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ulid-0.3.3.0.tar.gz";
      sha256 = "3990a1c1dcb6e13e49eda990310abfd00d1716f643b17d4b6e88a8e912c89229";
    });
  }) // {
    package-description-override = "cabal-version:       3.8\nname:                ulid\nversion:             0.3.3.0\nsynopsis:            Implementation of ULID - Universally Unique\n                     Lexicographically Sortable Identifier\ndescription:         Implementation of Alizain Feerasta's ULID specification.\n                     A 26 character string identifier,\n                     as opposed to the 36 character UUID string.\n                     Uses Douglas Crockford's base 32 encoding\n                     for better efficiency and readability\n                     (5 bits per character).\nhomepage:            https://github.com/ad-si/ulid\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Steve Kollmansberger, Adrian Sieber\nmaintainer:          ulid@ad-si.com\ncategory:            Data, Codec, Database\nbuild-type:          Simple\nextra-source-files:  README.md\n\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Data.ULID\n                    ,  Data.ULID.Base32\n                    ,  Data.ULID.Digits\n                    ,  Data.ULID.Random\n                    ,  Data.ULID.TimeStamp\n  other-modules:       Data.Binary.Roll\n  build-depends:       base >= 4.7 && < 5\n                    ,  binary\n                    ,  bytestring\n                    ,  crypto-api\n                    ,  deepseq\n                    ,  hashable\n                    ,  random\n                    ,  text\n                    ,  time\n  default-language:    Haskell2010\n  default-extensions:  OverloadedStrings\n\n\nexecutable ulid-exe\n  hs-source-dirs:      app\n  main-is:             Main.hs\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  build-depends:       base\n                     , ulid\n                     , crypto-api\n  default-language:    Haskell2010\n\n\ntest-suite ulid-test\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  other-modules:       Data.ULID.Base32Spec\n                     , Data.ULID.RandomSpec\n                     , Data.ULID.TimeStampSpec\n                     , Data.ULIDSpec\n  build-depends:       base\n                     , hspec\n                     , ulid\n                     , bytestring\n                     , binary\n                     , random\n                     , hashable\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n  default-extensions:  OverloadedStrings\n\n\nbenchmark ulid-bench\n  hs-source-dirs:      bench\n  main-is:             Main.hs\n  build-depends:       base\n                    ,  ulid\n                    ,  time\n                    ,  text\n                    ,  format-numbers\n                    ,  deepseq\n  default-language:    Haskell2010\n  default-extensions:  OverloadedStrings\n\n\nsource-repository head\n  type:     git\n  location: https://github.com/ad-si/ulid.git\n";
  }