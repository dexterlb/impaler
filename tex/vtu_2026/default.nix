{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.vtu_2026_abstract = buildLatex {
    src = ./.;
    pkgname = "vtu_2026_abstract";
    latexFiles = "abstract_en.tex";
  };
  packages.vtu_2026_slides = buildLatex {
    src = ./.;
    pkgname = "vtu_2026_slides";
    latexFiles = "slides.tex";
  };
  packages.vtu_2026_slides_with_notes = buildLatex {
    src = ./.;
    pkgname = "vtu_2026_slides_with_notes";
    latexFiles = "slides_with_notes.tex";
  };
  packages.vtu_2026_slides_presenter = pkgs.writeShellApplication
    {
      name = "present.sh";
      text = ''
        ${pkgs.pympress}/bin/pympress ${packages.vtu_2026_slides_with_notes}/slides_with_notes.pdf "''${@}"
      '';
    };
  packages.vtu_2026_all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "vtu_2026_all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.vtu_2026_abstract}/* ${packages.vtu_2026_slides}/* ${packages.vtu_2026_slides_with_notes}/* ${packages.vtu_2026_slides_presenter}/* $out/
    '';
  };
  apps.vtu_2026_slides_present = {
    type = "app";
    program = "${packages.vtu_2026_slides_presenter}/bin/present.sh";
  };
}
