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
            (hsPkgs."hnefatafl".components.sublibs.core-data or (errorHandler.buildDepError "hnefatafl:core-data"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."effectful" or (errorHandler.buildDepError "effectful"))
            (hsPkgs."effectful-th" or (errorHandler.buildDepError "effectful-th"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "bindings" ];
        };
        "core-data" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."effectful" or (errorHandler.buildDepError "effectful"))
            (hsPkgs."effectful-th" or (errorHandler.buildDepError "effectful-th"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "core-data" ];
        };
        "storage" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnefatafl".components.sublibs.core-data or (errorHandler.buildDepError "hnefatafl:core-data"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."effectful" or (errorHandler.buildDepError "effectful"))
            (hsPkgs."effectful-th" or (errorHandler.buildDepError "effectful-th"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "storage" ];
        };
        "storage-sqlite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnefatafl".components.sublibs.core-data or (errorHandler.buildDepError "hnefatafl:core-data"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."effectful" or (errorHandler.buildDepError "effectful"))
            (hsPkgs."effectful-th" or (errorHandler.buildDepError "effectful-th"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."hnefatafl".components.sublibs.storage or (errorHandler.buildDepError "hnefatafl:storage"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "storage-sqlite" ];
        };
      };
      tests = {
        "bindings-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnefatafl".components.sublibs.bindings or (errorHandler.buildDepError "hnefatafl:bindings"))
            (hsPkgs."hnefatafl".components.sublibs.core-data or (errorHandler.buildDepError "hnefatafl:core-data"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."effectful" or (errorHandler.buildDepError "effectful"))
            (hsPkgs."effectful-th" or (errorHandler.buildDepError "effectful-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations-pretty-diff" or (errorHandler.buildDepError "hspec-expectations-pretty-diff"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "bindings-test" ];
          mainPath = [ "Driver.hs" ];
        };
        "storage-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnefatafl".components.sublibs.core-data or (errorHandler.buildDepError "hnefatafl:core-data"))
            (hsPkgs."derive-storable" or (errorHandler.buildDepError "derive-storable"))
            (hsPkgs."derive-storable-plugin" or (errorHandler.buildDepError "derive-storable-plugin"))
            (hsPkgs."direct-sqlite" or (errorHandler.buildDepError "direct-sqlite"))
            (hsPkgs."effectful" or (errorHandler.buildDepError "effectful"))
            (hsPkgs."effectful-th" or (errorHandler.buildDepError "effectful-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations-pretty-diff" or (errorHandler.buildDepError "hspec-expectations-pretty-diff"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."hnefatafl".components.sublibs.storage or (errorHandler.buildDepError "hnefatafl:storage"))
            (hsPkgs."hnefatafl".components.sublibs.storage-sqlite or (errorHandler.buildDepError "hnefatafl:storage-sqlite"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = true;
          modules = [ "Paths_hnefatafl" ];
          hsSourceDirs = [ "storage-test" ];
          mainPath = [ "Driver.hs" ];
        };
      };
    };
  } // rec { src = pkgs.lib.mkDefault ../.; }) // { cabal-generator = "hpack"; }