{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.fmi_spring_session_slides = buildLatex {
    src = ./.;
    pkgname = "fmi_spring_session_slides";
    latexFiles = "slides.tex";
  };
  packages.fmi_spring_session_slides_with_notes = buildLatex {
    src = ./.;
    pkgname = "fmi_spring_session_slides_with_notes";
    latexFiles = "slides_with_notes.tex";
  };
  packages.fmi_spring_session_slides_presenter = pkgs.writeTextFile
    {
      name = "present.sh";
      text = ''
        ${pkgs.pympress}/bin/pympress ${packages.fmi_spring_session_slides_with_notes}/slides_with_notes.pdf "''${@}"
      '';
      executable = true;
      destination = "/present.sh";
    };
  packages.fmi_spring_session_slides_all = pkgs.stdenvNoCC.mkDerivation rec {
    name = "fmi_spring_session_slides_all";
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -rf ${packages.fmi_spring_session_slides}/* ${packages.fmi_spring_session_slides_with_notes}/* ${packages.fmi_spring_session_slides_presenter}/* $out/
    '';
  };
  apps.fmi_spring_session_slides_present = {
    type = "app";
    program = "${packages.fmi_spring_session_slides_presenter}/present.sh";
  };
}
