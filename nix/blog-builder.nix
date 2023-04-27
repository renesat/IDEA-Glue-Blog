{ haskell, ghc, libffi, gmp, zlib, inkscape, nodePackages, imagemagick, libaom
, python3Packages, }:

haskell.lib.buildStackProject {
  inherit ghc;
  name = "IDEAGlueBlog";
  buildInputs = [
    libffi
    gmp
    zlib

    inkscape
    nodePackages.katex
    imagemagick
    libaom
    python3Packages.pygments

  ];
}
