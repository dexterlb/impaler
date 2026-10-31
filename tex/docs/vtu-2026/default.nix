{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.vtu-2026-abstract = buildLatex {
    src = ./.;
    pkgname = "vtu-2026-abstract";
    latexFiles = "abstract-en.tex";
  };
  packages.vtu-2026-slides = buildLatex {
    src = ./.;
    pkgname = "vtu-2026-slides";
    latexFiles = "slides.tex";
  };
  packages.vtu-2026-slides-with-notes = buildLatex {
    src = ./.;
    pkgname = "vtu-2026-slides-with-notes";
    latexFiles = "slides-with-notes.tex";
  };
  packages.vtu-2026-slides-presenter = pkgs.writeShellApplication
    {
      name = "present.sh";
      text = ''
        ${pkgs.pympress}/bin/pympress ${packages.vtu-2026-slides-with-notes}/slides-with-notes.pdf "''${@}"
      '';
    };
  packages.vtu-2026-all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "vtu-2026-all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.vtu-2026-abstract}/* ${packages.vtu-2026-slides}/* ${packages.vtu-2026-slides-with-notes}/* ${packages.vtu-2026-slides-presenter}/* $out/
    '';
  };
  apps.vtu-2026-slides-present = {
    type = "app";
    program = "${packages.vtu-2026-slides-presenter}/bin/present.sh";
  };
}
