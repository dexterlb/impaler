{
  description = "Thesis text";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
    latex_tools.url = github:dexterlb/latex_tools;
  };

  outputs = { self, nixpkgs, flake-utils, latex_tools }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        texPkgs = [
          "subfiles"
          "scheme-small"
          "unicode-math"
          "svg" "trimspaces" "catchfile"
          "transparent"
          "latex-bin" "latexmk"
          "lualatex-math"
          "selnolig"
          "enumitem"
          "wrapfig"
          "extsizes" "euenc" "tools"
          "hyperref" "pdftexcmds" "infwarerr"
          "kvoptions" "l3kernel" "zref"
          "fontspec"
          "libertine"
          "geometry" "titling" "mathabx" "csquotes"
          "standalone" "cleveref" "ebproof"
          "appendix"
          "svn-prov"
          "luatex85"
          "minibox" "pbox" "mdframed" "needspace" "adjustbox"
          "lstaddons"
          "biblatex"
          "beamer"
          "cyrillic"
          "babel-bulgarian" "babel-english"
          "minted"
        ];

        extraBuildDeps = [
          pkgs.python311Packages.pygments
        ];

        latexTools = latex_tools.lib.mkLatexTools { inherit nixpkgs pkgs texPkgs extraBuildDeps; };
        fmiSpringSession = (import ./fmi_spring_session) { inherit pkgs latexTools; };
        isesia = (import ./isesia) { inherit pkgs latexTools; };
        thesis = (import ./thesis) { inherit pkgs latexTools; };
      in
      {
        packages
          = fmiSpringSession.packages
          // isesia.packages
          // thesis.packages
          // rec {
            default = thesis.packages.thesis;
          };
        devShell = pkgs.mkShell
          {
            packages = latexTools.deps;
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            shellHook = ''
              . ${latexTools.latexShellInit}

              echo "for any latex file, you can use:"
              echo " - 'latex_builder build foo.tex' to build dist/foo.pdf"
              echo " - 'latex_builder watch foo.tex' to build automatically on file change."
            '';
          };
        apps = fmiSpringSession.apps // isesia.apps;
      }
    );
}
