{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.isesia-slides = buildLatex {
    src = ./.;
    pkgname = "isesia-slides";
    latexFiles = "slides.tex";
  };
  packages.isesia-slides-with-notes = buildLatex {
    src = ./.;
    pkgname = "isesia-slides-with-notes";
    latexFiles = "slides_with_notes.tex";
  };
  packages.isesia-slides-presenter = pkgs.writeTextFile
    {
      name = "present.sh";
      text = ''
        #!/usr/bin/env bash
        ${pkgs.pympress}/bin/pympress ${packages.isesia-slides-with-notes}/slides_with_notes.pdf "''${@}"
      '';
      executable = true;
      destination = "/present.sh";
    };
  packages.isesia-all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "isesia-all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.isesia-slides}/* ${packages.isesia-slides-with-notes}/* ${packages.isesia-slides-presenter}/* $out/
    '';
  };
  apps.isesia-slides-present = {
    type = "app";
    program = "${packages.isesia-slides-presenter}/present.sh";
  };
}
