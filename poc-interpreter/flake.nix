{
  description = "A Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.11";
    hix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tek/hix?ref=0.8.0";
    };
  };

  outputs = {hix, ...}: hix.lib.flake ({config, ...}: {
    hackage.versionFile = "./ops/version.nix";

    compiler = "ghc912";
    ghcVersions = [
      "ghc912"
    ];

    envs.dev = {
      buildInputs = [ config.pkgs.cabal-install ];
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "dexterlb";
      ghc-options = [
        "-Werror"
        "-Wall"
        "-Wcompat"
        "-Widentities"
        "-Wmissing-deriving-strategies"
        # "-Wmissing-export-lists"
        "-Wpartial-fields"
        "-Wredundant-constraints"
        "-Wunused-type-patterns"
        "-Wincomplete-uni-patterns"
        "-Wunused-packages"
      ];
      default-extensions = [
        "BlockArguments"
        "DataKinds"
        "DeriveAnyClass"
        "DeriveGeneric"
        "DerivingStrategies"
        "DerivingVia"
        "GeneralisedNewtypeDeriving"
        "ImportQualifiedPost"
        "InstanceSigs"
        "LambdaCase"
        "NamedFieldPuns"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TupleSections"
        "TypeApplications"
        "KindSignatures"
        "OverloadedRecordDot"
        "DuplicateRecordFields"
        "ScopedTypeVariables"
        "FlexibleContexts"
        "MultiParamTypeClasses"
        "TypeOperators"
        "FlexibleInstances"
      ];
    };

    packages.poc-interpreter = {
      src = ./.;
      cabal.meta.synopsis = "PoC interpreter";

      library = {
        enable = true;
        dependencies = [
          "base >=4.7 && <5"
          "containers"
          "megaparsec"
          "extra"
          "parser-combinators"
          "text"
          "transformers"
          "timeit >= 2.0"
          "prettyprinter >= 1.7"
          "path"
          "mtl"
          "path-io >= 1.8"
          "optparse-generic"
        ];
      };

      executable.enable = true;

      test = {
        enable = true;
        dependencies = [
          "hspec"
          "filepath"
          "directory"
          "text"
          "pseudomacros"
        ];
      };
    };
  });
}
