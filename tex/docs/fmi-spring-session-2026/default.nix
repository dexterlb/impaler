{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.fmi-spring-session-2026-abstract = buildLatex {
    src = ./.;
    pkgname = "fmi-spring-session-2026-abstract";
    latexFiles = "abstract-en.tex abstract-bg.tex";
  };
  packages.fmi-spring-session-2026-slides = buildLatex {
    src = ./.;
    pkgname = "fmi-spring-session-2026-slides";
    latexFiles = "slides.tex";
  };
  packages.fmi-spring-session-2026-slides-with-notes = buildLatex {
    src = ./.;
    pkgname = "fmi-spring-session-2026-slides-with-notes";
    latexFiles = "slides-with-notes.tex";
  };
  packages.fmi-spring-session-2026-slides-presenter = pkgs.writeShellApplication
    {
      name = "present.sh";
      text = ''
        ${pkgs.pympress}/bin/pympress ${packages.fmi-spring-session-2026-slides-with-notes}/slides-with-notes.pdf "''${@}"
      '';
    };
  packages.fmi-spring-session-2026-all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "fmi-spring-session-2026-all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.fmi-spring-session-2026-abstract}/* ${packages.fmi-spring-session-2026-slides}/* ${packages.fmi-spring-session-2026-slides-with-notes}/* ${packages.fmi-spring-session-2026-slides-presenter}/* $out/
    '';
  };
  apps.fmi-spring-session-2026-slides-present = {
    type = "app";
    program = "${packages.fmi-spring-session-2026-slides-presenter}/bin/present.sh";
  };
}
