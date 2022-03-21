{

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [
      "aarch64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          sandbox = false;
        };

        package = pkgs.haskell.lib.buildStackProject {
          ghc = pkgs.ghc;
          name = "IDEAGlueBlog";
          buildInputs = with pkgs; [
            libffi
            gmp
            zlib

            inkscape
            nodePackages.katex
            imagemagick
            libaom
            python3Packages.pygments
          ];
        };
      in {
        defaultPackage = package;
        devShell = package;
      });
}
