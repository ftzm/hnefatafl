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
      specVersion = "2.2";
      identifier = { name = "optics"; version = "0.4.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "optics@well-typed.com";
      author = "Adam Gundry, Andres Löh, Andrzej Rybczak, Oleg Grenrus";
      homepage = "";
      url = "";
      synopsis = "Optics as an abstract interface";
      description = "This package makes it possible to define and use Lenses, Traversals, Prisms\nand other optics, using an abstract interface. See the main module \"Optics\"\nfor the documentation.\n\nThis is the \"batteries-included\" variant with many dependencies; see the\n@<https://hackage.haskell.org/package/optics-core optics-core>@ package and\nother @optics-*@ dependencies if you need a more limited dependency footprint.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."optics-extra" or (errorHandler.buildDepError "optics-extra"))
          (hsPkgs."optics-th" or (errorHandler.buildDepError "optics-th"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      tests = {
        "optics-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."indexed-profunctors" or (errorHandler.buildDepError "indexed-profunctors"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
          buildable = if compiler.isGhcjs && true then false else true;
        };
      };
      benchmarks = {
        "folds" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = if compiler.isGhcjs && true then false else true;
        };
        "traversals" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = if compiler.isGhcjs && true then false else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optics-0.4.2.1.tar.gz";
      sha256 = "e653d86aba75454fac21ab2f4220e895ad7f6e06889bc08d3f6522de4f7c5f6b";
    });
  }) // {
    package-description-override = "cabal-version:   2.2\nname:            optics\nversion:         0.4.2.1\nx-revision:      1\nlicense:         BSD-3-Clause\nlicense-file:    LICENSE\nbuild-type:      Simple\nmaintainer:      optics@well-typed.com\nauthor:          Adam Gundry, Andres Löh, Andrzej Rybczak, Oleg Grenrus\ntested-with:     GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7\n                  || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.5 || ==9.8.2\n                  || ==9.10.1,\n                 GHCJS ==8.4\nsynopsis:        Optics as an abstract interface\ncategory:        Data, Optics, Lenses\ndescription:\n  This package makes it possible to define and use Lenses, Traversals, Prisms\n  and other optics, using an abstract interface. See the main module \"Optics\"\n  for the documentation.\n  .\n\n  This is the \"batteries-included\" variant with many dependencies; see the\n  @<https://hackage.haskell.org/package/optics-core optics-core>@ package and\n  other @optics-*@ dependencies if you need a more limited dependency footprint.\n\nextra-doc-files:\n  diagrams/*.png\n  CHANGELOG.md\n\nbug-reports:   https://github.com/well-typed/optics/issues\nsource-repository head\n  type:     git\n  location: https://github.com/well-typed/optics.git\n  subdir:   optics\n\ncommon language\n    ghc-options:        -Wall -Wcompat\n\n    default-language:   Haskell2010\n\n    default-extensions: BangPatterns\n                        ConstraintKinds\n                        DefaultSignatures\n                        DeriveFoldable\n                        DeriveFunctor\n                        DeriveGeneric\n                        DeriveTraversable\n                        EmptyCase\n                        FlexibleContexts\n                        FlexibleInstances\n                        FunctionalDependencies\n                        GADTs\n                        GeneralizedNewtypeDeriving\n                        InstanceSigs\n                        KindSignatures\n                        LambdaCase\n                        OverloadedLabels\n                        PatternSynonyms\n                        RankNTypes\n                        ScopedTypeVariables\n                        TupleSections\n                        TypeApplications\n                        TypeFamilies\n                        TypeOperators\n                        ViewPatterns\n\nlibrary\n  import:             language\n  hs-source-dirs:     src\n\n  build-depends: base                   >= 4.10      && <5\n               , array                  >= 0.5.2.0   && <0.6\n               , containers             >= 0.5.10.2  && <0.8\n               , mtl                    >= 2.2.2     && <2.4\n               , optics-core            >= 0.4.1     && <0.4.2\n               , optics-extra           >= 0.4.2     && <0.4.3\n               , optics-th              >= 0.4.1     && <0.4.2\n               , transformers           >= 0.5       && <0.7\n\n  -- main module to land with repl\n  exposed-modules:    Optics\n\n  -- main optic type\n  reexported-modules: Optics.Optic\n\n  -- optic flavours\n  reexported-modules: Optics.AffineFold\n                    , Optics.AffineTraversal\n                    , Optics.Fold\n                    , Optics.Getter\n                    , Optics.Iso\n                    , Optics.IxAffineFold\n                    , Optics.IxAffineTraversal\n                    , Optics.IxFold\n                    , Optics.IxGetter\n                    , Optics.IxLens\n                    , Optics.IxSetter\n                    , Optics.IxTraversal\n                    , Optics.Lens\n                    , Optics.Prism\n                    , Optics.ReversedLens\n                    , Optics.ReversedPrism\n                    , Optics.Review\n                    , Optics.Setter\n                    , Optics.Traversal\n\n  -- optics utilities\n  reexported-modules: Optics.Arrow\n                    , Optics.At\n                    , Optics.Coerce\n                    , Optics.Cons\n                    , Optics.Each\n                    , Optics.Empty\n                    , Optics.Generic\n                    , Optics.Indexed\n                    , Optics.Label\n                    , Optics.Mapping\n                    , Optics.Operators\n                    , Optics.Operators.Unsafe\n                    , Optics.Passthrough\n                    , Optics.Re\n                    , Optics.ReadOnly\n                    , Optics.State\n                    , Optics.State.Operators\n                    , Optics.View\n                    , Optics.Zoom\n\n  -- template haskell support\n  reexported-modules: Optics.TH\n\n  -- data specific optics\n  reexported-modules: Data.ByteString.Lazy.Optics\n                    , Data.ByteString.Optics\n                    , Data.ByteString.Strict.Optics\n                    , Data.Either.Optics\n                    , Data.HashMap.Optics\n                    , Data.HashSet.Optics\n                    , Data.IntMap.Optics\n                    , Data.IntSet.Optics\n                    , Data.List.Optics\n                    , Data.Map.Optics\n                    , Data.Maybe.Optics\n                    , Data.Sequence.Optics\n                    , Data.Set.Optics\n                    , Data.Text.Lazy.Optics\n                    , Data.Text.Optics\n                    , Data.Text.Strict.Optics\n                    , Data.Tree.Optics\n                    , Data.Tuple.Optics\n                    , Data.Typeable.Optics\n                    , Data.Vector.Generic.Optics\n                    , Data.Vector.Optics\n                    , GHC.Generics.Optics\n                    , Numeric.Optics\n\ntest-suite optics-tests\n  import:           language\n  hs-source-dirs:   tests\n\n  -- inspection-testing doesn't compile with GHCJS\n  if impl(ghcjs)\n    buildable: False\n\n  build-depends: base\n               , containers\n               , indexed-profunctors    >= 0.1        && <0.2\n               , inspection-testing     >= 0.5        && <0.6\n               , mtl\n               , optics\n               , optics-core\n               , QuickCheck\n               , random\n               , tasty\n               , tasty-hunit\n               , tasty-quickcheck\n               , template-haskell\n\n  type:    exitcode-stdio-1.0\n  main-is: Optics/Tests.hs\n\n  other-modules: Optics.Tests.Computation\n                 Optics.Tests.Core\n                 Optics.Tests.Eta\n                 Optics.Tests.Labels.Generic\n                 Optics.Tests.Labels.TH\n                 Optics.Tests.Misc\n                 Optics.Tests.Properties\n                 Optics.Tests.Utils\n\n-- Benchmarking folds\nbenchmark folds\n  import:           language\n  hs-source-dirs:   benchmarks\n\n  -- GHCJS takes forever to compile dependencies\n  if impl(ghcjs)\n    buildable: False\n\n  build-depends: base\n               , bytestring\n               , containers\n               , lens\n               , optics\n               , tasty-bench\n               , unordered-containers\n               , vector\n\n  type:    exitcode-stdio-1.0\n  main-is: folds.hs\n\n-- Benchmarking traversals\nbenchmark traversals\n  import:           language\n  hs-source-dirs:   benchmarks\n\n  -- GHCJS takes forever to compile dependencies\n  if impl(ghcjs)\n    buildable: False\n\n  build-depends: base\n               , bytestring\n               , containers\n               , lens\n               , optics\n               , tasty-bench\n               , transformers\n               , unordered-containers\n               , vector\n\n  type:    exitcode-stdio-1.0\n  main-is: traversals.hs\n";
  }