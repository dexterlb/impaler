{
  description = "Thesis text";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    with flake-utils.lib;
    eachSystem allSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        tex-megapkg = pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-small
            latex-bin latexmk
            extsizes euenc tools
            hyperref pdftexcmds infwarerr
            kvoptions l3kernel zref
            fontspec
            libertine
            geometry titling mathabx csquotes
            subfiles cleveref ebproof
            appendix
            minibox pbox mdframed needspace adjustbox
            lstaddons
            biblatex
            babel-bulgarian babel-english;
        };
        deps = [
          tex-megapkg
          pkgs.biber
        ];
      in
      {
        packages.default = pkgs.stdenvNoCC.mkDerivation rec {
          name = "thesis-text";
          src = ./.;
          buildInputs = [
            pkgs.coreutils
            pkgs.bash
          ] ++ deps;
          phases = ["unpackPhase" "buildPhase" "installPhase"];
          buildPhase = ''
            export PATH="${pkgs.lib.makeBinPath buildInputs}";

            mkdir -p .cache/texmf-var
            export TEXMFHOME=.cache TEXMFVAR=.cache/texmf-var

            bash build.sh
          '';
          installPhase = ''
            mkdir -p $out
            cp main.pdf $out/
          '';
        };
        devShell = pkgs.mkShell
          {
            packages = [
              tex-megapkg
              pkgs.biber
            ] ++ deps;
            shellHook = ''
              echo "you can use './build.sh' to build main.pdf"
              echo "you can also use './build.sh watch' to build automatically on file change."
            '';
          };
      }
    );
}
