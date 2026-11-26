{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.module-sys-paper = buildLatex {
    src = ./.;
    pkgname = "module-sys-paper";
    latexFiles = "main.tex";
  };
}
