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
      nativeBuildInputs = [ pkgs.libreoffice pkgs.liberation_ttf pkgs.dejavu_fonts ];
      FONTCONFIG_FILE = pkgs.writeText "fonts.conf" ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
          <dir>${pkgs.liberation_ttf}/share/fonts</dir>
          <dir>${pkgs.dejavu_fonts}/share/fonts</dir>
          <cachedir prefix="xdg">fontconfig</cachedir>
          <include ignore_missing="yes">${pkgs.fontconfig}/etc/fonts/conf.d</include>
        </fontconfig>
      '';
    } ''
      export HOME="$PWD/home"
      mkdir -p "$HOME"
      soffice --headless --norestore --convert-to pdf \
        --outdir . ${packages.vtu-lite-docx}/vtu-lite.docx
      install -Dm644 vtu-lite.pdf "$out/vtu-lite.pdf"
    '';
  };
}
