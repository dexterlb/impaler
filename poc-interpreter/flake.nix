{
  description = "A Haskell project";

  inputs.hix.url = "github:tek/hix/main";

  outputs = {hix, ...}: hix.lib.flake ({config, ...}: {
    hackage.versionFile = "./ops/version.nix";

    compiler = "ghc94";
    ghcVersions = [
      "ghc94"
    ];

    envs.dev = {
      buildInputs = [ config.pkgs.cabal-install ];
      profiling = true;
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
