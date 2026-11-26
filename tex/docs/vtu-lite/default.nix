{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages.vtu-lite-paper = buildLatex {
    src = ./.;
    pkgname = "vtu-lite-paper";
    latexFiles = "main.tex";
  };

  packages.vtu-lite-docx = pkgs.callPackage ./vtu2docx {
    src = ./.;
    common = ../../common;
    inherit latexTools;
  };
}
