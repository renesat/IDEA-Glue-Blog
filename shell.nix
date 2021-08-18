{ pkgs ? import <nixpkgs> { } }:

with pkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "IDEA-Glue-Blog";
  buildInputs = [
    nodePackages.katex

    inkscape
    imagemagick

    python39Packages.pygments

    zlib
  ];
}
