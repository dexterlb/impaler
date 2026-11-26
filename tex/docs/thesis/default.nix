{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.thesis = buildLatex {
    src = ./.;
    pkgname = "thesis";
    latexFiles = "main.tex";
  };
}
