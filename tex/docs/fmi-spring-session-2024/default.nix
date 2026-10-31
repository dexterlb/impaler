{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.fmi-spring-session-2024-abstract = buildLatex {
    src = ./.;
    pkgname = "fmi-spring-session-2024-abstract";
    latexFiles = "abstract-en.tex abstract-bg.tex";
  };
  packages.fmi-spring-session-2024-slides = buildLatex {
    src = ./.;
    pkgname = "fmi-spring-session-2024-slides";
    latexFiles = "slides.tex";
  };
  packages.fmi-spring-session-2024-slides-with-notes = buildLatex {
    src = ./.;
    pkgname = "fmi-spring-session-2024-slides-with-notes";
    latexFiles = "slides-with-notes.tex";
  };
  packages.fmi-spring-session-2024-slides-presenter = pkgs.writeTextFile
    {
      name = "present.sh";
      text = ''
        #!/usr/bin/env bash
        ${pkgs.pympress}/bin/pympress ${packages.fmi-spring-session-2024-slides-with-notes}/slides-with-notes.pdf "''${@}"
      '';
      executable = true;
      destination = "/present.sh";
    };
  packages.fmi-spring-session-2024-all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "fmi-spring-session-2024-all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.fmi-spring-session-2024-abstract}/* ${packages.fmi-spring-session-2024-slides}/* ${packages.fmi-spring-session-2024-slides-with-notes}/* ${packages.fmi-spring-session-2024-slides-presenter}/* $out/
    '';
  };
  apps.fmi-spring-session-2024-slides-present = {
    type = "app";
    program = "${packages.fmi-spring-session-2024-slides-presenter}/present.sh";
  };
}
