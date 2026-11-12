{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.module-sys-barebones-paper = buildLatex {
    src = ./.;
    pkgname = "module-sys-barebones-paper";
    latexFiles = "main.tex";
  };
}
