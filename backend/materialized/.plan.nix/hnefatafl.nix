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
      specVersion = "2.4";
      identifier = { name = "hnefatafl"; version = "0.0.0.2"; };
      license = "NONE";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
    };
    components = {
      sublibs = {
        "bindings" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "bindings" ];
        };
      };
      tests = {
        "bindings-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnefatafl".components.sublibs.bindings or (errorHandler.buildDepError "hnefatafl:bindings"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations-pretty-diff" or (errorHandler.buildDepError "hspec-expectations-pretty-diff"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "bindings-test" ];
          mainPath = [ "Driver.hs" ];
        };
      };
    };
  } // rec { src = pkgs.lib.mkDefault ../.; }) // { cabal-generator = "hpack"; }