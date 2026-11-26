{ pkgs, latexTools }:
let
  buildLatex = latexTools.buildLatex;
in rec {
  packages = {
    vtu-lite-paper = buildLatex {
      src = ./.;
      pkgname = "vtu-lite-paper";
      latexFiles = "main.tex";
    };

    vtu-lite-docx = pkgs.callPackage ./vtu2docx {
      src = ./.;
      common = ../../common;
      inherit latexTools;
    };
  } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
    vtu-lite-pdf-via-docx = pkgs.runCommand "vtu-lite-pdf-via-docx" {
      nativeBuildInputs = [ pkgs.libreoffice-fresh ];
    } ''
      export HOME="$PWD/home"
      mkdir -p "$HOME"
      soffice --headless --norestore --convert-to pdf \
        --outdir . ${packages.vtu-lite-docx}/vtu-lite.docx
      install -Dm644 vtu-lite.pdf "$out/vtu-lite.pdf"
    '';
  };
}
