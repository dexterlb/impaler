{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.isesia_slides = buildLatex {
    src = ./.;
    pkgname = "isesia_slides";
    latexFiles = "slides.tex";
  };
  packages.isesia_slides_with_notes = buildLatex {
    src = ./.;
    pkgname = "isesia_slides_with_notes";
    latexFiles = "slides_with_notes.tex";
  };
  packages.isesia_slides_presenter = pkgs.writeTextFile
    {
      name = "present.sh";
      text = ''
        #!/usr/bin/env bash
        ${pkgs.pympress}/bin/pympress ${packages.isesia_slides_with_notes}/slides_with_notes.pdf "''${@}"
      '';
      executable = true;
      destination = "/present.sh";
    };
  packages.isesia_all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "isesia_all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.isesia_abstract}/* ${packages.isesia_slides}/* ${packages.isesia_slides_with_notes}/* ${packages.isesia_slides_presenter}/* $out/
    '';
  };
  apps.isesia_slides_present = {
    type = "app";
    program = "${packages.isesia_slides_presenter}/present.sh";
  };
}
