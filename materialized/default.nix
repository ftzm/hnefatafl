{
  pkgs = hackage:
    {
      packages = {
        ghc-boot.revision = hackage.ghc-boot."9.10.2".revisions.default;
        tasty-quickcheck.revision = import ./cabal-files/tasty-quickcheck.nix;
        filepath.revision = hackage.filepath."1.5.4.0".revisions.default;
        semaphore-compat.revision = hackage.semaphore-compat."1.0.0".revisions.default;
        unordered-containers.revision = import ./cabal-files/unordered-containers.nix;
        unordered-containers.flags.debug = false;
        logict.revision = import ./cabal-files/logict.nix;
        tasty.revision = import ./cabal-files/tasty.nix;
        tasty.flags.unix = true;
        HUnit.revision = import ./cabal-files/HUnit.nix;
        ghc.revision = hackage.ghc."9.10.2".revisions.default;
        hscolour.revision = import ./cabal-files/hscolour.nix;
        haskell-lexer.revision = import ./cabal-files/haskell-lexer.nix;
        ghc-heap.revision = hackage.ghc-heap."9.10.2".revisions.default;
        quickcheck-io.revision = import ./cabal-files/quickcheck-io.nix;
        derive-storable.revision = import ./cabal-files/derive-storable.nix;
        derive-storable.flags.sumtypes = true;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        tasty-smallcheck.revision = import ./cabal-files/tasty-smallcheck.nix;
        stm.revision = hackage.stm."2.5.3.1".revisions.default;
        unicode-show.revision = import ./cabal-files/unicode-show.nix;
        derive-storable-plugin.revision = import ./cabal-files/derive-storable-plugin.nix;
        derive-storable-plugin.flags.sumtypes = false;
        transformers.revision = hackage.transformers."0.6.1.1".revisions.default;
        deepseq.revision = hackage.deepseq."1.5.0.0".revisions.default;
        optparse-applicative.revision = import ./cabal-files/optparse-applicative.nix;
        optparse-applicative.flags.process = true;
        directory.revision = hackage.directory."1.3.8.5".revisions.default;
        ghci.revision = hackage.ghci."9.10.2".revisions.default;
        parsec.revision = hackage.parsec."3.1.18.0".revisions.default;
        Diff.revision = import ./cabal-files/Diff.nix;
        mtl.revision = hackage.mtl."2.3.1".revisions.default;
        tasty-hunit.revision = import ./cabal-files/tasty-hunit.nix;
        ansi-terminal-types.revision = import ./cabal-files/ansi-terminal-types.nix;
        process.revision = hackage.process."1.6.25.0".revisions.default;
        base.revision = hackage.base."4.20.1.0".revisions.default;
        call-stack.revision = import ./cabal-files/call-stack.nix;
        QuickCheck.revision = import ./cabal-files/QuickCheck.nix;
        QuickCheck.flags.old-random = false;
        QuickCheck.flags.templatehaskell = true;
        hspec.revision = import ./cabal-files/hspec.nix;
        random.revision = import ./cabal-files/random.nix;
        text.revision = hackage.text."2.1.2".revisions.default;
        tasty-hspec.revision = import ./cabal-files/tasty-hspec.nix;
        time.revision = hackage.time."1.12.2".revisions.default;
        array.revision = hackage.array."0.5.8.0".revisions.default;
        tf-random.revision = import ./cabal-files/tf-random.nix;
        hspec-discover.revision = import ./cabal-files/hspec-discover.nix;
        hspec-api.revision = import ./cabal-files/hspec-api.nix;
        hspec-core.revision = import ./cabal-files/hspec-core.nix;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.random-initial-seed = false;
        hashable.flags.arch-native = false;
        hpc.revision = hackage.hpc."0.7.0.2".revisions.default;
        ghc-internal.revision = hackage.ghc-internal."9.1002.0".revisions.default;
        binary.revision = hackage.binary."0.8.9.3".revisions.default;
        template-haskell.revision = hackage.template-haskell."2.22.0.0".revisions.default;
        unix.revision = hackage.unix."2.8.6.0".revisions.default;
        primitive.revision = import ./cabal-files/primitive.nix;
        ansi-terminal.revision = import ./cabal-files/ansi-terminal.nix;
        ansi-terminal.flags.example = false;
        colour.revision = import ./cabal-files/colour.nix;
        exceptions.revision = hackage.exceptions."0.10.9".revisions.default;
        bytestring.revision = hackage.bytestring."0.12.2.0".revisions.default;
        relude.revision = import ./cabal-files/relude.nix;
        ghc-platform.revision = hackage.ghc-platform."0.1.0.0".revisions.default;
        nicify-lib.revision = import ./cabal-files/nicify-lib.nix;
        tagged.revision = import ./cabal-files/tagged.nix;
        tagged.flags.transformers = true;
        tagged.flags.deepseq = true;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.10.2".revisions.default;
        safe.revision = import ./cabal-files/safe.nix;
        Glob.revision = import ./cabal-files/Glob.nix;
        os-string.revision = hackage.os-string."2.0.4".revisions.default;
        transformers-compat.revision = import ./cabal-files/transformers-compat.nix;
        transformers-compat.flags.three = false;
        transformers-compat.flags.four = false;
        transformers-compat.flags.five-three = true;
        transformers-compat.flags.mtl = true;
        transformers-compat.flags.generic-deriving = true;
        transformers-compat.flags.two = false;
        transformers-compat.flags.five = false;
        prettyprinter.revision = import ./cabal-files/prettyprinter.nix;
        prettyprinter.flags.buildreadme = false;
        prettyprinter.flags.text = true;
        ghc-prim.revision = hackage.ghc-prim."0.12.0".revisions.default;
        hspec-expectations.revision = import ./cabal-files/hspec-expectations.nix;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        containers.revision = hackage.containers."0.7".revisions.default;
        prettyprinter-ansi-terminal.revision = import ./cabal-files/prettyprinter-ansi-terminal.nix;
        hspec-expectations-pretty-diff.revision = import ./cabal-files/hspec-expectations-pretty-diff.nix;
        tasty-discover.revision = import ./cabal-files/tasty-discover.nix;
        tasty-discover.flags.dev = false;
        dlist.revision = import ./cabal-files/dlist.nix;
        dlist.flags.werror = false;
        smallcheck.revision = import ./cabal-files/smallcheck.nix;
      };
      compiler = {
        version = "9.10.2";
        nix-name = "ghc9102";
        packages = {
          "ghc-boot-th" = "9.10.2";
          "binary" = "0.8.9.3";
          "pretty" = "1.1.3.6";
          "array" = "0.5.8.0";
          "time" = "1.12.2";
          "ghc-prim" = "0.12.0";
          "bytestring" = "0.12.2.0";
          "process" = "1.6.25.0";
          "ghci" = "9.10.2";
          "mtl" = "2.3.1";
          "text" = "2.1.2";
          "template-haskell" = "2.22.0.0";
          "semaphore-compat" = "1.0.0";
          "parsec" = "3.1.18.0";
          "ghc-bignum" = "1.3";
          "stm" = "2.5.3.1";
          "filepath" = "1.5.4.0";
          "os-string" = "2.0.4";
          "unix" = "2.8.6.0";
          "ghc-platform" = "0.1.0.0";
          "ghc" = "9.10.2";
          "ghc-boot" = "9.10.2";
          "hpc" = "0.7.0.2";
          "exceptions" = "0.10.9";
          "deepseq" = "1.5.0.0";
          "transformers" = "0.6.1.1";
          "containers" = "0.7";
          "ghc-internal" = "9.1002.0";
          "base" = "4.20.1.0";
          "ghc-heap" = "9.10.2";
          "directory" = "1.3.8.5";
        };
      };
    };
  extras = hackage:
    { packages = { hnefatafl = ./.plan.nix/hnefatafl.nix; }; };
  modules = [
    {
      preExistingPkgs = [
        "ghc-boot"
        "filepath"
        "semaphore-compat"
        "ghc"
        "ghc-heap"
        "ghc-bignum"
        "stm"
        "transformers"
        "deepseq"
        "directory"
        "ghci"
        "parsec"
        "mtl"
        "process"
        "base"
        "text"
        "time"
        "array"
        "hpc"
        "ghc-internal"
        "binary"
        "template-haskell"
        "unix"
        "exceptions"
        "bytestring"
        "ghc-platform"
        "ghc-boot-th"
        "os-string"
        "ghc-prim"
        "pretty"
        "containers"
      ];
    }
    ({ lib, ... }:
      { packages = { "hnefatafl" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "tf-random".components.library.planned = lib.mkOverride 900 true;
          "quickcheck-io".components.library.planned = lib.mkOverride 900 true;
          "semaphore-compat".components.library.planned = lib.mkOverride 900 true;
          "nicify-lib".components.library.planned = lib.mkOverride 900 true;
          "unicode-show".components.library.planned = lib.mkOverride 900 true;
          "hspec-discover".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "hscolour".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "hnefatafl".components.sublibs."bindings".planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "hspec-core".components.library.planned = lib.mkOverride 900 true;
          "Glob".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "tasty-hunit".components.library.planned = lib.mkOverride 900 true;
          "call-stack".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "hscolour".components.exes."HsColour".planned = lib.mkOverride 900 true;
          "Diff".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "HUnit".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "hspec-expectations-pretty-diff".components.library.planned = lib.mkOverride 900 true;
          "derive-storable".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "dlist".components.library.planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "tasty-discover".components.library.planned = lib.mkOverride 900 true;
          "ghc".components.library.planned = lib.mkOverride 900 true;
          "prettyprinter".components.library.planned = lib.mkOverride 900 true;
          "hspec-discover".components.exes."hspec-discover".planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          "hspec".components.library.planned = lib.mkOverride 900 true;
          "tasty-smallcheck".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "hspec-api".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "colour".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "optparse-applicative".components.library.planned = lib.mkOverride 900 true;
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "ghc-internal".components.library.planned = lib.mkOverride 900 true;
          "smallcheck".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "haskell-lexer".components.library.planned = lib.mkOverride 900 true;
          "os-string".components.library.planned = lib.mkOverride 900 true;
          "prettyprinter-ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "safe".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "logict".components.library.planned = lib.mkOverride 900 true;
          "derive-storable-plugin".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal-types".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "tasty-discover".components.exes."tasty-discover".planned = lib.mkOverride 900 true;
          "tasty-quickcheck".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "tasty-hspec".components.library.planned = lib.mkOverride 900 true;
          "hspec-expectations".components.library.planned = lib.mkOverride 900 true;
          "relude".components.library.planned = lib.mkOverride 900 true;
          "ghc-platform".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "tasty".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "hnefatafl".components.tests."bindings-test".planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}