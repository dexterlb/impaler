{
  description = "impaler typst texts";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";
  inputs.treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    {
      nixpkgs,
      treefmt-nix,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      forAllSystems = lib.genAttrs lib.systems.flakeExposed;

      treefmtFor =
        pkgs:
        treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          programs.typstyle.enable = true;
        };

      typstRoot = ./.;

      texts = lib.mapAttrs (name: _: "texts/${name}/main.typ") (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir (typstRoot + "/texts"))
      );

      build-pdf =
        pkgs: name: main-file:
        pkgs.stdenvNoCC.mkDerivation {
          pname = "${name}.pdf";
          version = "0";
          src = typstRoot;
          nativeBuildInputs = [ pkgs.typst ];
          buildPhase = ''
            runHook preBuild
            typst compile --root . ${lib.escapeShellArg main-file} ${lib.escapeShellArg "${name}.pdf"}
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            install -Dm644 ${lib.escapeShellArg "${name}.pdf"} $out/${lib.escapeShellArg "${name}.pdf"}
            runHook postInstall
          '';
        };

      watch-app =
        pkgs: name: main-file:
        {
          type = "app";
          program = "${pkgs.writeShellScript "${name}-watch" ''
            exec ${pkgs.typst}/bin/typst watch --root . ${lib.escapeShellArg main-file} "''${1:-/tmp/out.pdf}"
          ''}";
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        let
          pdfs = lib.mapAttrs' (
            name: main-file: lib.nameValuePair "${name}.pdf" (build-pdf pkgs name main-file)
          ) texts;
        in
        pdfs
        // rec {
          all-pdfs = pkgs.runCommand "all-pdfs" { } ''
            mkdir -p $out
            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (name: drv: "cp -r --reflink=auto ${drv}/. $out/") pdfs
            )}
          '';
          default = all-pdfs;
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        lib.mapAttrs' (
          name: main-file: lib.nameValuePair "${name}-watch" (watch-app pkgs name main-file)
        ) texts
      );

      formatter = forAllSystems (
        system: (treefmtFor nixpkgs.legacyPackages.${system}).config.build.wrapper
      );
    };
}
