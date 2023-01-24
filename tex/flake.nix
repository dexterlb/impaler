{
  description = "Thesis text";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      packages.x86_64-linux.default = pkgs.runCommand
        "thesis-text"
        { }
        ''
          # ${pkgs.nodePackages.reveal-md}/bin/reveal-md --static $out ${./present.md}
          echo foo
        '';
      devShell.x86_64-linux = pkgs.mkShell
        {
          packages = [
            pkgs.emacsPackages.auctex-latexmk
          ];
          shellHook = ''
            echo 'foo bar baz'.
          '';
        };
    };
}
