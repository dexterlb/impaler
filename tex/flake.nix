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
        latexTools = latex_tools.lib.mkLatexTools { inherit nixpkgs pkgs; };
        fmiSpringSession = (import ./fmi_spring_session) { inherit pkgs latexTools; };
      in
      {
        packages = fmiSpringSession.packages // {

        };
      }
    );
}
