{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.vtu-lite-paper = buildLatex {
    src = ./.;
    pkgname = "vtu-lite-paper";
    latexFiles = "main.tex";
  };
}
