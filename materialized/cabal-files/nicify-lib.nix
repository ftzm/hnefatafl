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
      identifier = { name = "nicify-lib"; version = "1.0.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "julian@scravy.de";
      author = "Julian Fleischer";
      homepage = "";
      url = "";
      synopsis = "Pretty print the standard output of default `Show` instances.";
      description = "Pretty print the standard output of `show` for algebraic datatypes";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nicify-lib-1.0.1.tar.gz";
      sha256 = "7d26f86d792dda166805e9dda17cfbc7a2101f3654fe798f4231385d8136e732";
    });
  }) // {
    package-description-override = "Name:           nicify-lib\nVersion:        1.0.1\nSynopsis:       Pretty print the standard output of default `Show` instances.\nDescription:    Pretty print the standard output of `show` for algebraic datatypes\n                \nLicense:        MIT\nLicense-File:   LICENSE\nAuthor:         Julian Fleischer\nMaintainer:     julian@scravy.de\nBuild-Type:     Simple\nCabal-Version:  >= 1.10\nCategory:       Text, Tools, Utilities\nStability:      stable\n\nSource-Repository head\n    type: git\n    location: https://github.com/scravy/nicify-lib\n\nLibrary\n    Exposed-Modules:    Text.Nicify\n    Build-Depends:      base ==4.*\n                        , parsec ==3.*\n                        , transformers >= 0.3\n    Hs-Source-Dirs:     src\n    Default-Language:   Haskell2010\n";
  }